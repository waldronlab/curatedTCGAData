\name{ READ }
\alias{ READ }
\docType{data}
\title{ Rectum adenocarcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( READ )
ExperimentList class object of length 13: 
 [1] CNASeq-20160128: RaggedExperiment with 56380 rows and 70 columns 
 [2] CNASNP-20160128: RaggedExperiment with 156806 rows and 316 columns 
 [3] CNVSNP-20160128: RaggedExperiment with 35765 rows and 316 columns 
 [4] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 165 columns 
 [5] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 165 columns 
 [6] Methylation_methyl27-20160128: SummarizedExperiment with 27578 rows and 73 columns 
 [7] Methylation_methyl450-20160128: SummarizedExperiment with 485577 rows and 106 columns 
 [8] miRNASeqGene-20160128: SummarizedExperiment with 705 rows and 76 columns 
 [9] mRNAArray-20160128: SummarizedExperiment with 17814 rows and 72 columns 
 [10] Mutation-20160128: RaggedExperiment with 22075 rows and 69 columns 
 [11] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 72 columns 
 [12] RNASeqGene-20160128: SummarizedExperiment with 20502 rows and 72 columns 
 [13] RPPAArray-20160128: SummarizedExperiment with 208 rows and 131 columns 

> rownames( READ )
CharacterList of length 13
[["CNASeq-20160128"]] character(0)
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["GISTIC_ThresholdedByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["Methylation_methyl27-20160128"]] cg00000292 cg00002426 ... cg27665659
[["Methylation_methyl450-20160128"]] cg00000029 cg00000108 ... rs9839873
[["miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["mRNAArray-20160128"]] ELMO2 CREB3L1 RPS11 PNMA1 ... SLC39A6 SNRPD2 AQP7 CTSC
[["Mutation-20160128"]] character(0)
...
<3 more elements>

> colnames( READ )
CharacterList of length 13
[["CNASeq-20160128"]] TCGA-AF-2691-01A-01D-1167-02 ...
[["CNASNP-20160128"]] TCGA-AF-2687-01A-02D-1732-01 ...
[["CNVSNP-20160128"]] TCGA-AF-2687-01A-02D-1732-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-AF-2687-01A-02D-1732-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-AF-2687-01A-02D-1732-01 ...
[["Methylation_methyl27-20160128"]] TCGA-AF-2689-11A-01D-1552-05 ...
[["Methylation_methyl450-20160128"]] TCGA-AF-2687-01A-02D-1734-05 ...
[["miRNASeqGene-20160128"]] TCGA-AF-2687-01A-02T-1735-13 ...
[["mRNAArray-20160128"]] TCGA-AF-2689-11A-01R-1758-07 ...
[["Mutation-20160128"]] TCGA-AF-2689-01A-01W-0831-10 ... TCGA-AG-A036-01
...
<3 more elements>

Sizes of each ExperimentList element:

                               assay size.Mb
1                    CNASeq-20160128  1.5 Mb
2                    CNASNP-20160128  4.3 Mb
3                    CNVSNP-20160128  1.1 Mb
4          GISTIC_AllByGene-20160128  4.3 Mb
5  GISTIC_ThresholdedByGene-20160128  4.3 Mb
6      Methylation_methyl27-20160128  4.4 Mb
7     Methylation_methyl450-20160128 67.4 Mb
8              miRNASeqGene-20160128  0.1 Mb
9                 mRNAArray-20160128    1 Mb
10                 Mutation-20160128  9.1 Mb
11          RNASeq2GeneNorm-20160128  1.1 Mb
12               RNASeqGene-20160128  1.1 Mb
13                RPPAArray-20160128    0 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   142 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  27.00   27.00    2.00    1.44    3.25 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  31.00   57.00   66.00   64.37   72.00   90.00 

vital_status:
  0   1 
141  28 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   59.0   347.5   730.0   786.1  1193.0  1741.0     142 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   366.0   625.0   779.5  1096.0  3932.0      28 

tumor_tissue_site:
rectum   NA's 
   166      3 




pathology_M_stage:
  m0   m1  m1a   mx NA's 
 128   22    2   14    3 

gender:
female   male 
    77     92 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1999    2007    2009    2008    2010    2012 

days_to_last_known_alive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   31.0   292.2   863.0  1420.1  2214.5  3667.0     161 

radiation_therapy:
  no  yes NA's 
 114   22   33 

histological_type:
         rectal adenocarcinoma rectal mucinous adenocarcinoma 
                           150                             13 
                          NA's 
                             6 

tumor_stage:
stage iia      NA's 
        1       168 

residual_tumor:
  r0   r1   r2   rx NA's 
 126    2   12    5   24 

number_of_lymph_nodes:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.692   3.000  31.000      10 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     1                     84                     84 

admin.bcr:
nationwide children's hospital 
                           169 

admin.day_of_dcc_upload:
  1  10 
168   1 

admin.disease_code:
read 
 169 


admin.month_of_dcc_upload:
  7  10 
  1 168 

admin.patient_withdrawal.withdrawn:
false 
  169 

admin.project_code:
tcga 
 169 

admin.year_of_dcc_upload:
2015 
 169 

patient.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  31.00   57.00   66.00   64.37   72.00   90.00 

patient.anatomic_neoplasm_subdivision:
rectosigmoid junction                rectum         sigmoid colon 
                   50                   112                     5 
                 NA's 
                    2 


patient.braf_gene_analysis_performed:
  no  yes NA's 
 142    8   19 

patient.braf_gene_analysis_result:
normal   NA's 
     7    162 

patient.circumferential_resection_margin:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.00   10.00   14.57   22.00   70.00     116 

patient.clinical_cqcf.consent_or_death_status:
consented 
      169 


patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -22.00    0.00    0.00   25.01   30.00 1384.00 

patient.clinical_cqcf.frozen_specimen_anatomic_site:
rectosigmoid junction                rectum         sigmoid colon 
                   50                   114                     5 

patient.clinical_cqcf.histological_type:
         rectal adenocarcinoma rectal mucinous adenocarcinoma 
                           150                             14 
                          NA's 
                             5 

patient.clinical_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                             168 
yes, pharmaceutical treatment prior to resection 
                                               1 

patient.clinical_cqcf.history_of_prior_malignancy:
                              no yes, history of prior malignancy 
                             159                               10 

patient.clinical_cqcf.normal_tissue_anatomic_site:
        other        rectum sigmoid colon          NA's 
           21             7             6           135 

patient.clinical_cqcf.normal_tissue_proximity:
distal (>2cm)          NA's 
           26           143 

patient.clinical_cqcf.other_anatomic_site_normal_tissue:
blood  NA's 
    5   164 

patient.clinical_cqcf.tumor_type:
primary 
    169 

patient.colon_polyps_present:
  no  yes NA's 
  64   12   93 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   15.00   18.00   16.65   18.00   31.00      11 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -32873  -26542  -24167  -23672  -21093  -11381 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   59.0   556.0   992.0   961.6  1432.0  1741.0     160 

patient.days_to_initial_pathologic_diagnosis:
  0 
169 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     2.5    31.0   244.9   224.5  3667.0       6 

patient.days_to_last_known_alive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   31.0   292.2   863.0  1420.1  2214.5  3667.0     161 

patient.drugs.drug.10.bcr_drug_barcode:
 tcga-af-2689-d3028 tcga-af-3914-d41489 tcga-dc-6156-d44758 tcga-f5-6571-d65923 
                  1                   1                   1                   1 
               NA's 
                165 

patient.drugs.drug.10.bcr_drug_uuid:
45c103ca-fafa-4be8-96bb-786e15d2d8ca 4d891da7-ccc2-4b3e-b531-0bbd6385e6a6 
                                   1                                    1 
55f67205-87d7-49df-85d4-77dba50b9a0a a35156fa-affb-498c-ae11-e35ecc46908e 
                                   1                                    1 
                                NA's 
                                 165 

patient.drugs.drug.10.day_of_form_completion:
   1    5   19   26 NA's 
   1    1    1    1  165 

patient.drugs.drug.10.days_to_drug_therapy_end:
 344  761 1024 NA's 
   1    1    1  166 

patient.drugs.drug.10.days_to_drug_therapy_start:
 316  395  924 1255 NA's 
   1    1    1    1  165 

patient.drugs.drug.10.drug_name:
                                      bevacizumab 
                                                1 
fluorouracil iv continuous infusion over 46 hours 
                                                1 
                                       leucovorin 
                                                1 
                                      oxaliplatin 
                                                1 
                                             NA's 
                                              165 

patient.drugs.drug.10.measure_of_response:
clinical progressive disease               stable disease 
                           1                            1 
                        NA's 
                         167 

patient.drugs.drug.10.month_of_form_completion:
   3    6   10   11 NA's 
   1    1    1    1  165 

patient.drugs.drug.10.number_cycles:
   3 NA's 
   1  168 

patient.drugs.drug.10.prescribed_dose:
568-576    NA's 
      1     168 

patient.drugs.drug.10.prescribed_dose_units:
  mg NA's 
   1  168 

patient.drugs.drug.10.regimen_indication:
progression        NA's 
          1         168 

patient.drugs.drug.10.regimen_number:
   1 NA's 
   1  168 

patient.drugs.drug.10.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              168 

patient.drugs.drug.10.therapy_ongoing:
  no  yes NA's 
   3    1  165 

patient.drugs.drug.10.therapy_types.therapy_type:
  chemotherapy other, specify           NA's 
             3              1            165 

patient.drugs.drug.10.therapy_types.therapy_type_notes:
other ancillary-velour 10262 study- aflibercept/placebo (4mg/kg) every 2 weeks with irinotecan/5fu combination (folfiri) 
                                                                                                                       1 
                                                                                                                    NA's 
                                                                                                                     168 

patient.drugs.drug.10.total_dose:
1716 NA's 
   1  168 

patient.drugs.drug.10.total_dose_units:
  mg NA's 
   1  168 

patient.drugs.drug.10.tx_on_clinical_trial:
  no NA's 
   3  166 

patient.drugs.drug.10.year_of_form_completion:
2010 2013 2014 NA's 
   1    2    1  165 

patient.drugs.drug.11.bcr_drug_barcode:
 tcga-af-2689-d3307 tcga-f5-6571-d65924                NA's 
                  1                   1                 167 

patient.drugs.drug.11.bcr_drug_uuid:
5df65b50-c401-4ff5-af88-293a113e284a 82fa8f68-90f5-42e8-8abf-5a8c488d0f1e 
                                   1                                    1 
                                NA's 
                                 167 

patient.drugs.drug.11.day_of_form_completion:
   1   12 NA's 
   1    1  167 

patient.drugs.drug.11.days_to_drug_therapy_end:
 559 NA's 
   1  168 

patient.drugs.drug.11.days_to_drug_therapy_start:
 379 1255 NA's 
   1    1  167 

patient.drugs.drug.11.drug_name:
fudr (floxuridine)        gemcitabine               NA's 
                 1                  1                167 

patient.drugs.drug.11.month_of_form_completion:
  10   11 NA's 
   1    1  167 

patient.drugs.drug.11.prescribed_dose:
1135-1500      NA's 
        1       168 

patient.drugs.drug.11.prescribed_dose_units:
  mg NA's 
   1  168 

patient.drugs.drug.11.regimen_indication:
progression        NA's 
          1         168 

patient.drugs.drug.11.regimen_number:
   2 NA's 
   1  168 

patient.drugs.drug.11.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              168 

patient.drugs.drug.11.therapy_ongoing:
  no  yes NA's 
   1    1  167 

patient.drugs.drug.11.therapy_types.therapy_type:
chemotherapy         NA's 
           2          167 

patient.drugs.drug.11.total_dose_units:
  mg NA's 
   1  168 

patient.drugs.drug.11.tx_on_clinical_trial:
  no NA's 
   1  168 

patient.drugs.drug.11.year_of_form_completion:
2010 2014 NA's 
   1    1  167 

patient.drugs.drug.12.bcr_drug_barcode:
tcga-af-2689-d3308               NA's 
                 1                168 

patient.drugs.drug.12.bcr_drug_uuid:
8c673e37-976e-454d-841d-a0888f2d93ed                                 NA's 
                                   1                                  168 

patient.drugs.drug.12.day_of_form_completion:
  12 NA's 
   1  168 

patient.drugs.drug.12.days_to_drug_therapy_end:
 559 NA's 
   1  168 

patient.drugs.drug.12.days_to_drug_therapy_start:
 379 NA's 
   1  168 

patient.drugs.drug.12.drug_name:
camptosar      NA's 
        1       168 

patient.drugs.drug.12.month_of_form_completion:
  11 NA's 
   1  168 

patient.drugs.drug.12.prescribed_dose:
330-430    NA's 
      1     168 

patient.drugs.drug.12.prescribed_dose_units:
  mg NA's 
   1  168 

patient.drugs.drug.12.regimen_indication:
progression        NA's 
          1         168 

patient.drugs.drug.12.regimen_number:
   2 NA's 
   1  168 

patient.drugs.drug.12.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              168 

patient.drugs.drug.12.therapy_ongoing:
  no NA's 
   1  168 

patient.drugs.drug.12.therapy_types.therapy_type:
chemotherapy         NA's 
           1          168 

patient.drugs.drug.12.total_dose_units:
  mg NA's 
   1  168 

patient.drugs.drug.12.year_of_form_completion:
2010 NA's 
   1  168 

patient.drugs.drug.13.bcr_drug_barcode:
tcga-af-2689-d3309               NA's 
                 1                168 

patient.drugs.drug.13.bcr_drug_uuid:
7eefebc0-11fd-4028-87c6-cab921f47ab3                                 NA's 
                                   1                                  168 

patient.drugs.drug.13.day_of_form_completion:
  12 NA's 
   1  168 

patient.drugs.drug.13.days_to_drug_therapy_end:
 559 NA's 
   1  168 

patient.drugs.drug.13.days_to_drug_therapy_start:
 462 NA's 
   1  168 

patient.drugs.drug.13.drug_name:
avastin    NA's 
      1     168 

patient.drugs.drug.13.month_of_form_completion:
  11 NA's 
   1  168 

patient.drugs.drug.13.prescribed_dose:
500-515    NA's 
      1     168 

patient.drugs.drug.13.prescribed_dose_units:
  mg NA's 
   1  168 

patient.drugs.drug.13.regimen_indication:
progression        NA's 
          1         168 

patient.drugs.drug.13.regimen_number:
   2 NA's 
   1  168 

patient.drugs.drug.13.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              168 

patient.drugs.drug.13.therapy_ongoing:
  no NA's 
   1  168 

patient.drugs.drug.13.therapy_types.therapy_type:
targeted molecular therapy                       NA's 
                         1                        168 

patient.drugs.drug.13.total_dose_units:
  mg NA's 
   1  168 

patient.drugs.drug.13.year_of_form_completion:
2010 NA's 
   1  168 

patient.drugs.drug.14.bcr_drug_barcode:
tcga-af-2689-d4053               NA's 
                 1                168 

patient.drugs.drug.14.bcr_drug_uuid:
62a137f0-ba1f-4bfa-a6cc-cfe749a79ca4                                 NA's 
                                   1                                  168 

patient.drugs.drug.14.day_of_form_completion:
  24 NA's 
   1  168 

patient.drugs.drug.14.days_to_drug_therapy_end:
 559 NA's 
   1  168 

patient.drugs.drug.14.days_to_drug_therapy_start:
 379 NA's 
   1  168 

patient.drugs.drug.14.drug_name:
leucovorin       NA's 
         1        168 

patient.drugs.drug.14.month_of_form_completion:
  11 NA's 
   1  168 

patient.drugs.drug.14.prescribed_dose:
710-760    NA's 
      1     168 

patient.drugs.drug.14.prescribed_dose_units:
  mg NA's 
   1  168 

patient.drugs.drug.14.regimen_indication:
progression        NA's 
          1         168 

patient.drugs.drug.14.regimen_number:
   2 NA's 
   1  168 

patient.drugs.drug.14.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              168 

patient.drugs.drug.14.therapy_ongoing:
  no NA's 
   1  168 

patient.drugs.drug.14.therapy_types.therapy_type:
other, specify           NA's 
             1            168 

patient.drugs.drug.14.therapy_types.therapy_type_notes:
ancillary      NA's 
        1       168 

patient.drugs.drug.14.total_dose_units:
  mg NA's 
   1  168 

patient.drugs.drug.14.year_of_form_completion:
2010 NA's 
   1  168 



patient.drugs.drug.2.clinical_trail_drug_classification:
 n/a NA's 
  15  154 

patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    5.00    7.00   12.06   18.00   29.00     121 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   61.0   207.0   254.0   338.3   345.5  1673.0     122 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   17.0    52.0    68.0   180.6   185.0  1277.0     120 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease            complete response 
                           5                           16 
            partial response               stable disease 
                           3                            3 
                        NA's 
                         142 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   9.000   8.265  12.000  12.000     120 

patient.drugs.drug.2.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   7.250  10.000   9.167  12.000  17.000     151 


patient.drugs.drug.2.prescribed_dose_units:
   mg mg/m2  NA's 
   14     1   154 

patient.drugs.drug.2.regimen_indication:
               adjuvant other: specify in notes              palliative 
                     13                       1                       2 
            progression                    NA's 
                      2                     151 

patient.drugs.drug.2.regimen_number:
   1    2 NA's 
  14    3  152 

patient.drugs.drug.2.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
              17              152 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
  47    2  120 

patient.drugs.drug.2.therapy_types.therapy_type:
                 ancillary               chemotherapy 
                         4                         41 
            other, specify targeted molecular therapy 
                         3                          1 
                      NA's 
                       120 

patient.drugs.drug.2.therapy_types.therapy_type.2:
other, specify           NA's 
             1            168 

patient.drugs.drug.2.therapy_types.therapy_type_notes:
                                                                                               ancillary 
                                                                                                       2 
velour 10262 study- aflibercept/placebo (4mg/kg) every 2 weeks with irinotecan/5fu combination (folfiri) 
                                                                                                       1 
                                                                                                    NA's 
                                                                                                     166 

patient.drugs.drug.2.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    260    1678    4090    6243   11124   15200     155 

patient.drugs.drug.2.total_dose_units:
  mg NA's 
  14  155 

patient.drugs.drug.2.tx_on_clinical_trial:
  no NA's 
  31  138 

patient.drugs.drug.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2015     120 



patient.drugs.drug.3.clinical_trail_drug_classification:
 n/a NA's 
  10  159 

patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    5.00    7.00   12.59   18.00   29.00     130 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   61.0   202.5   240.5   259.3   269.8   952.0     131 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  28.00   55.75   90.00  144.45  133.00 1095.00     129 


patient.drugs.drug.3.measure_of_response:
clinical progressive disease            complete response 
                           4                           11 
            partial response               stable disease 
                           2                            3 
                        NA's 
                         149 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   9.000   8.025  12.000  12.000     129 

patient.drugs.drug.3.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    9.00   11.00   10.12   12.00   17.00     152 


patient.drugs.drug.3.prescribed_dose_units:
   mg mg/m2  NA's 
   14     1   154 

patient.drugs.drug.3.regimen_indication:
   adjuvant  palliative progression        NA's 
         14           2           1         152 

patient.drugs.drug.3.regimen_number:
   1    2 NA's 
  12    4  153 

patient.drugs.drug.3.route_of_administrations.route_of_administration:
 intravenous (iv) subcutaneous (sc)              NA's 
               15                 1               153 

patient.drugs.drug.3.therapy_ongoing:
  no  yes NA's 
  38    1  130 

patient.drugs.drug.3.therapy_types.therapy_type:
                 ancillary               chemotherapy 
                         2                         30 
            other, specify targeted molecular therapy 
                         5                          3 
                      NA's 
                       129 

patient.drugs.drug.3.therapy_types.therapy_type_notes:
ancillary      NA's 
        4       165 

patient.drugs.drug.3.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     36    2838    7538   17364   32325   51786     155 

patient.drugs.drug.3.total_dose_units:
  mg NA's 
  14  155 

patient.drugs.drug.3.tx_on_clinical_trial:
  no NA's 
  22  147 

patient.drugs.drug.3.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2015     129 



patient.drugs.drug.4.clinical_trail_drug_classification:
 n/a NA's 
   3  166 

patient.drugs.drug.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    7.00   13.00   15.17   26.00   29.00     146 

patient.drugs.drug.4.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   73.0   234.0   263.0   321.4   324.0   952.0     146 

patient.drugs.drug.4.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   28.0    54.5    91.0   162.1   197.0   847.0     146 


patient.drugs.drug.4.measure_of_response:
clinical progressive disease            complete response 
                           2                            2 
            partial response               stable disease 
                           2                            2 
                        NA's 
                         161 

patient.drugs.drug.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.500   8.000   7.783  11.000  12.000     146 

patient.drugs.drug.4.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   9.000  11.500   9.857  12.000  12.000     155 


patient.drugs.drug.4.prescribed_dose_units:
  mg NA's 
  13  156 

patient.drugs.drug.4.regimen_indication:
   adjuvant  palliative progression        NA's 
         12           1           1         155 

patient.drugs.drug.4.regimen_number:
   1    2 NA's 
   9    4  156 

patient.drugs.drug.4.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
              14              155 

patient.drugs.drug.4.therapy_ongoing:
  no NA's 
  23  146 

patient.drugs.drug.4.therapy_types.therapy_type:
                 ancillary               chemotherapy 
                         1                         19 
            other, specify targeted molecular therapy 
                         2                          1 
                      NA's 
                       146 

patient.drugs.drug.4.therapy_types.therapy_type.2:
other, specify           NA's 
             1            168 

patient.drugs.drug.4.therapy_types.therapy_type_notes:
                                                                                               ancillary 
                                                                                                       1 
velour 10262 study- aflibercept/placebo (4mg/kg) every 2 weeks with irinotecan/5fu combination (folfiri) 
                                                                                                       1 
                                                                                                    NA's 
                                                                                                     167 

patient.drugs.drug.4.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    850    2820    7225   17019   28850   61776     157 

patient.drugs.drug.4.total_dose_units:
  mg NA's 
  12  157 

patient.drugs.drug.4.tx_on_clinical_trial:
  no NA's 
   9  160 

patient.drugs.drug.4.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2012    2012    2015     146 



patient.drugs.drug.5.clinical_trail_drug_classification:
 n/a NA's 
   2  167 

patient.drugs.drug.5.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    5.00   11.50   13.17   19.00   28.00     157 

patient.drugs.drug.5.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  106.0   180.5   294.0   389.5   377.0  1096.0     158 

patient.drugs.drug.5.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   62.0   104.8   159.0   290.0   342.5   851.0     157 


patient.drugs.drug.5.measure_of_response:
clinical progressive disease            complete response 
                           1                            1 
            partial response               stable disease 
                           1                            1 
                        NA's 
                         165 

patient.drugs.drug.5.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.750   8.000   7.833  11.250  12.000     157 

patient.drugs.drug.5.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   6.000   8.000   7.857  10.000  12.000     162 


patient.drugs.drug.5.prescribed_dose_units:
  mg NA's 
   7  162 

patient.drugs.drug.5.regimen_indication:
   adjuvant  palliative progression        NA's 
          5           1           1         162 

patient.drugs.drug.5.regimen_number:
   1    2 NA's 
   3    4  162 

patient.drugs.drug.5.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               7              162 

patient.drugs.drug.5.therapy_ongoing:
  no  yes NA's 
  11    1  157 

patient.drugs.drug.5.therapy_types.therapy_type:
     ancillary   chemotherapy other, specify           NA's 
             1             10              1            157 

patient.drugs.drug.5.therapy_types.therapy_type_notes:
velour 10262 study- aflibercept/placebo (4mg/kg) every 2 weeks with irinotecan/5fu combination (folfiri) 
                                                                                                       1 
                                                                                                    NA's 
                                                                                                     168 

patient.drugs.drug.5.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    537    1910    7300    9577   10594   34193     162 

patient.drugs.drug.5.total_dose_units:
  mg NA's 
   7  162 

patient.drugs.drug.5.tx_on_clinical_trial:
  no NA's 
   5  164 

patient.drugs.drug.5.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2015     157 



patient.drugs.drug.6.clinical_trail_drug_classification:
 n/a NA's 
   1  168 

patient.drugs.drug.6.day_of_form_completion:
   1    5   19   22 NA's 
   1    2    1    1  164 

patient.drugs.drug.6.days_to_drug_therapy_end:
 239  396 1134 1157 NA's 
   1    1    1    1  165 

patient.drugs.drug.6.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   85.0   335.0   395.0   560.8   924.0  1065.0     164 

patient.drugs.drug.6.drug_name:
bevacizumab  irinotecan oxaliplatin        NA's 
          3           1           1         164 

patient.drugs.drug.6.measure_of_response:
clinical progressive disease               stable disease 
                           2                            1 
                        NA's 
                         166 

patient.drugs.drug.6.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     3.0    10.0     7.6    11.0    12.0     164 

patient.drugs.drug.6.number_cycles:
   6   10 NA's 
   1    1  167 

patient.drugs.drug.6.prescribed_dose:
229.5 - 259.1           480          NA's 
            1             1           167 

patient.drugs.drug.6.prescribed_dose_units:
  mg NA's 
   2  167 

patient.drugs.drug.6.regimen_indication:
palliative       NA's 
         2        167 

patient.drugs.drug.6.regimen_number:
   1    2 NA's 
   1    1  167 

patient.drugs.drug.6.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               2              167 

patient.drugs.drug.6.therapy_ongoing:
  no  yes NA's 
   4    1  164 

patient.drugs.drug.6.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                         2                          1 
targeted molecular therapy                       NA's 
                         2                        164 

patient.drugs.drug.6.total_dose:
2363.5   2880   NA's 
     1      1    167 

patient.drugs.drug.6.total_dose_units:
  mg NA's 
   2  167 

patient.drugs.drug.6.tx_on_clinical_trial:
  no NA's 
   3  166 

patient.drugs.drug.6.year_of_form_completion:
2010 2012 2013 2014 NA's 
   1    2    1    1  164 



patient.drugs.drug.7.clinical_trail_drug_classification:
 n/a NA's 
   1  168 

patient.drugs.drug.7.day_of_form_completion:
   1    5   16   19 NA's 
   1    2    1    1  164 

patient.drugs.drug.7.days_to_drug_therapy_end:
 269  580 1022 1157 NA's 
   1    1    1    1  165 

patient.drugs.drug.7.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   85.0   365.0   427.0   573.2   924.0  1065.0     164 

patient.drugs.drug.7.drug_name:
         5fu fluorouracil   irinotecan   leucovorin         NA's 
           1            1            1            2          164 

patient.drugs.drug.7.measure_of_response:
clinical progressive disease               stable disease 
                           1                            2 
                        NA's 
                         166 

patient.drugs.drug.7.month_of_form_completion:
   3   10   11   12 NA's 
   2    1    1    1  164 

patient.drugs.drug.7.number_cycles:
   8   12 NA's 
   1    1  167 

patient.drugs.drug.7.prescribed_dose:
      320 3480-3648      NA's 
        1         1       167 

patient.drugs.drug.7.prescribed_dose_units:
  mg NA's 
   2  167 

patient.drugs.drug.7.regimen_indication:
palliative       NA's 
         2        167 

patient.drugs.drug.7.regimen_number:
   1    2 NA's 
   1    1  167 

patient.drugs.drug.7.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               2              167 

patient.drugs.drug.7.therapy_ongoing:
  no  yes NA's 
   4    1  164 

patient.drugs.drug.7.therapy_types.therapy_type:
   ancillary chemotherapy         NA's 
           2            3          164 

patient.drugs.drug.7.total_dose:
 2368 42192  NA's 
    1     1   167 

patient.drugs.drug.7.total_dose_units:
  mg NA's 
   2  167 

patient.drugs.drug.7.tx_on_clinical_trial:
  no NA's 
   3  166 

patient.drugs.drug.7.year_of_form_completion:
2010 2012 2013 2014 NA's 
   1    2    1    1  164 



patient.drugs.drug.8.clinical_trail_drug_classification:
 n/a NA's 
   1  168 

patient.drugs.drug.8.day_of_form_completion:
   1    5   16   19 NA's 
   1    2    1    1  164 

patient.drugs.drug.8.days_to_drug_therapy_end:
 267  580 1022 1157 NA's 
   1    1    1    1  165 

patient.drugs.drug.8.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   85.0   365.0   427.0   573.2   924.0  1065.0     164 


patient.drugs.drug.8.measure_of_response:
clinical progressive disease               stable disease 
                           1                            2 
                        NA's 
                         166 

patient.drugs.drug.8.month_of_form_completion:
   3   10   11   12 NA's 
   2    1    1    1  164 

patient.drugs.drug.8.number_cycles:
   8   12 NA's 
   1    1  167 

patient.drugs.drug.8.prescribed_dose:
    100 580-608    NA's 
      1       1     167 

patient.drugs.drug.8.prescribed_dose_units:
  mg NA's 
   2  167 

patient.drugs.drug.8.regimen_indication:
palliative       NA's 
         2        167 

patient.drugs.drug.8.regimen_number:
   1    2 NA's 
   1    1  167 

patient.drugs.drug.8.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               2              167 

patient.drugs.drug.8.therapy_ongoing:
  no  yes NA's 
   4    1  164 

patient.drugs.drug.8.therapy_types.therapy_type:
chemotherapy         NA's 
           5          164 

patient.drugs.drug.8.total_dose:
 740 7032 NA's 
   1    1  167 

patient.drugs.drug.8.total_dose_units:
  mg NA's 
   2  167 

patient.drugs.drug.8.tx_on_clinical_trial:
  no NA's 
   3  166 

patient.drugs.drug.8.year_of_form_completion:
2010 2012 2013 2014 NA's 
   1    2    1    1  164 



patient.drugs.drug.9.clinical_trail_drug_classification:
 n/a NA's 
   1  168 

patient.drugs.drug.9.day_of_form_completion:
   1    5   16   19 NA's 
   1    2    1    1  164 

patient.drugs.drug.9.days_to_drug_therapy_end:
 344  580 1022 NA's 
   1    1    1  166 

patient.drugs.drug.9.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  316.0   365.0   427.0   619.4   924.0  1065.0     164 

patient.drugs.drug.9.drug_name:
 bevacizumab fluorouracil   irinotecan         NA's 
           1            2            2          164 

patient.drugs.drug.9.measure_of_response:
clinical progressive disease               stable disease 
                           1                            1 
                        NA's 
                         167 

patient.drugs.drug.9.month_of_form_completion:
   3   10   11   12 NA's 
   2    1    1    1  164 

patient.drugs.drug.9.number_cycles:
   3    8 NA's 
   1    1  167 

patient.drugs.drug.9.prescribed_dose:
256-259     800    NA's 
      1       1     167 

patient.drugs.drug.9.prescribed_dose_units:
  mg NA's 
   2  167 

patient.drugs.drug.9.regimen_indication:
 palliative progression        NA's 
          1           1         167 

patient.drugs.drug.9.regimen_number:
   1    2 NA's 
   1    1  167 

patient.drugs.drug.9.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               2              167 

patient.drugs.drug.9.therapy_ongoing:
  no  yes NA's 
   3    2  164 

patient.drugs.drug.9.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         4                          1 
                      NA's 
                       164 

patient.drugs.drug.9.therapy_types.therapy_type.2:
other, specify           NA's 
             1            168 

patient.drugs.drug.9.therapy_types.therapy_type_notes:
velour 10262 study- aflibercept/placebo (4mg/kg) every 2 weeks with irinotecan/5fu combination (folfiri) 
                                                                                                       1 
                                                                                                    NA's 
                                                                                                     168 

patient.drugs.drug.9.total_dose:
 772 5920 NA's 
   1    1  167 

patient.drugs.drug.9.total_dose_units:
  mg NA's 
   2  167 

patient.drugs.drug.9.tx_on_clinical_trial:
  no NA's 
   3  166 

patient.drugs.drug.9.year_of_form_completion:
2010 2012 2013 2014 NA's 
   1    2    1    1  164 



patient.drugs.drug.clinical_trail_drug_classification:
 n/a NA's 
  20  149 

patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    5.00    7.00   13.21   26.00   30.00      92 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   61.0   153.0   224.0   277.6   267.0  1673.0     100 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   17.0    46.5    62.0   141.7    98.5  1277.0      94 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                           5                           29 
            partial response               stable disease 
                           3                            3 
                        NA's 
                         129 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.250   9.000   7.833  12.000  12.000      91 

patient.drugs.drug.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    6.00   11.00    9.36   12.00   17.00     144 



patient.drugs.drug.regimen_indication:
   adjuvant  palliative progression  recurrence        NA's 
         27           3           1           2         136 

patient.drugs.drug.regimen_number:
   1    2 NA's 
  17    2  150 

patient.drugs.drug.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
              27                5              137 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
  68    7   94 

patient.drugs.drug.therapy_types.therapy_type:
                 ancillary               chemotherapy 
                         1                         72 
            other, specify targeted molecular therapy 
                         4                          1 
                      NA's 
                        91 

patient.drugs.drug.therapy_types.therapy_type_notes:
      ancillary ancillary agent      leucovorin            NA's 
              1               2               1             165 

patient.drugs.drug.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    336    3088    7409   16713   10630   93000     153 

patient.drugs.drug.total_dose_units:
g/day    mg  NA's 
    1    24   144 

patient.drugs.drug.tx_on_clinical_trial:
  no NA's 
  45  124 

patient.drugs.drug.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2012    2012    2012    2015      91 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     1                     84                     84 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   13.00   17.50   17.83   20.50   31.00     133 

patient.follow_ups.follow_up.2.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  334.0   512.0   524.0   633.8   598.0  1201.0     164 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  306.0   453.2   737.5   731.7   942.5  1309.0     139 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             3                             31 
                          NA's 
                           135 

patient.follow_ups.follow_up.2.followup_treatment_success:
complete remission/response         progressive disease 
                         12                           4 
                       NA's 
                        153 

patient.follow_ups.follow_up.2.lost_follow_up:
  no  yes NA's 
  19    1  149 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1       2       4       5       7      12     132 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    8  160 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   9  160 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
 869 NA's 
   1  168 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   30.0   368.0   461.0   466.1   660.0   846.0     160 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
locoregional disease                 NA's 
                   1                  168 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   8    1  160 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.site_of_additional_surgery_new_tumor_event_mets:
liver  NA's 
    2   167 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  26    9  134 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        26          8        135 

patient.follow_ups.follow_up.2.postoperative_rx_tx:
  no  yes NA's 
  18   11  140 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success:
complete remission/response         progressive disease 
                         13                           2 
                       NA's 
                        154 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
  25    4  140 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
   30     6   133 

patient.follow_ups.follow_up.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2012    2012    2013    2013    2015     132 



patient.follow_ups.follow_up.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   21.00   26.00   23.13   26.00   30.00     154 

patient.follow_ups.follow_up.3.days_to_death:
1201 NA's 
   1  168 

patient.follow_ups.follow_up.3.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  762.0   829.8  1128.0  1109.8  1326.2  1581.0     155 

patient.follow_ups.follow_up.3.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             2                             13 
                          NA's 
                           154 

patient.follow_ups.follow_up.3.followup_treatment_success:
complete remission/response         progressive disease 
                          3                           4 
             stable disease                        NA's 
                          1                         161 

patient.follow_ups.follow_up.3.lost_follow_up:
  no NA's 
  15  154 

patient.follow_ups.follow_up.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     4.5     6.0     5.8     6.5    12.0     154 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   4    2  163 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   6  163 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   30.0   889.5  1039.5   892.7  1115.2  1267.0     163 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no NA's 
   6  163 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.site_of_additional_surgery_new_tumor_event_mets:
liver  NA's 
    1   168 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   9    6  154 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        10          5        154 

patient.follow_ups.follow_up.3.postoperative_rx_tx:
  no  yes NA's 
   8    6  155 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success:
complete remission/response                        NA's 
                          6                         163 

patient.follow_ups.follow_up.3.radiation_therapy:
  no  yes NA's 
  11    3  155 

patient.follow_ups.follow_up.3.vital_status:
alive  dead  NA's 
   14     1   154 

patient.follow_ups.follow_up.3.year_of_form_completion:
2012 2013 2014 2015 NA's 
   2   11    1    1  154 

patient.follow_ups.follow_up.4.bcr_followup_barcode:
tcga-af-2689-f34761                NA's 
                  1                 168 

patient.follow_ups.follow_up.4.bcr_followup_uuid:
836c3d65-5ad5-41bb-9be8-5d3b9cf0024c                                 NA's 
                                   1                                  168 

patient.follow_ups.follow_up.4.day_of_form_completion:
  30 NA's 
   1  168 

patient.follow_ups.follow_up.4.days_to_death:
1201 NA's 
   1  168 

patient.follow_ups.follow_up.4.followup_case_report_form_submission_reason:
additional new tumor event                       NA's 
                         1                        168 

patient.follow_ups.follow_up.4.followup_treatment_success:
progressive disease                NA's 
                  1                 168 

patient.follow_ups.follow_up.4.lost_follow_up:
  no NA's 
   1  168 

patient.follow_ups.follow_up.4.month_of_form_completion:
   8 NA's 
   1  168 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no NA's 
   1  168 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   1  168 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
1134 NA's 
   1  168 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1  168 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event_after_initial_treatment:
 yes NA's 
   1  168 

patient.follow_ups.follow_up.4.person_neoplasm_cancer_status:
with tumor       NA's 
         1        168 

patient.follow_ups.follow_up.4.vital_status:
dead NA's 
   1  168 

patient.follow_ups.follow_up.4.year_of_form_completion:
2012 NA's 
   1  168 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.00   12.00   14.15   24.50   30.00      26 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   61.0   349.8   930.5   863.2  1272.0  1741.0     149 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  106.0   358.5   537.5   734.9   974.0  3932.0      43 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason:
scheduled follow-up submission                           NA's 
                           132                             37 

patient.follow_ups.follow_up.followup_treatment_success:
complete remission/response  partial remission/response 
                         53                           4 
        progressive disease              stable disease 
                         11                           1 
                       NA's 
                        100 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
  81    5   83 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000  10.000   8.226  12.000  12.000      23 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   6    9  154 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  12    4  153 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    127     150     304     359     396     818     164 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   157.0   368.0   582.2   791.5  3316.0     142 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
locoregional disease           metastatic                 NA's 
                   2                    4                  163 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  21    6  142 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r0   rx NA's 
   2    1  166 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.site_of_additional_surgery_new_tumor_event_mets:
liver other  NA's 
    3     1   165 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 108   27   34 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        98         25         46 

patient.follow_ups.follow_up.postoperative_rx_tx:
  no  yes NA's 
  71   66   32 

patient.follow_ups.follow_up.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         37                           3 
        progressive disease              stable disease 
                          6                           1 
                       NA's 
                        122 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
 113   23   33 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
  125    20    24 

patient.follow_ups.follow_up.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2012    2012    2012    2015      23 

patient.gender:
female   male 
    77     92 

patient.height:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  150.0   163.0   170.0   170.7   178.0   189.3      94 

patient.histological_type:
         rectal adenocarcinoma rectal mucinous adenocarcinoma 
                           150                             13 
                          NA's 
                             6 

patient.history_of_colon_polyps:
  no  yes NA's 
 117   33   19 

patient.history_of_neoadjuvant_treatment:
 no yes 
168   1 

patient.icd_10:
c18.9   c19   c20 c49.4 c80.1 
    6    70    90     2     1 

patient.icd_o_3_histology:
8140/3 8211/3 8255/3 8263/3 8480/3 
   139      5      2      8     15 

patient.icd_o_3_site:
c18.9 c19.9 c20.9 c49.4 c80.9 
    6    70    90     2     1 

patient.informed_consent_verified:
yes 
169 

patient.kras_gene_analysis_performed:
  no  yes NA's 
 136   15   18 

patient.kras_mutation_codon:
  12   13   61 NA's 
   6    2    1  160 

patient.kras_mutation_found:
  no  yes NA's 
   8    9  152 

patient.loss_expression_of_mismatch_repair_proteins_by_ihc:
  no  yes NA's 
 127    8   34 

patient.loss_expression_of_mismatch_repair_proteins_by_ihc_results.loss_expression_of_mismatch_repair_proteins_by_ihc_result:
    mlh1 expressed mlh1 not expressed               NA's 
                 5                  2                162 

patient.loss_expression_of_mismatch_repair_proteins_by_ihc_results.loss_expression_of_mismatch_repair_proteins_by_ihc_result.2:
    msh2 expressed msh2 not expressed               NA's 
                 5                  2                162 

patient.loss_expression_of_mismatch_repair_proteins_by_ihc_results.loss_expression_of_mismatch_repair_proteins_by_ihc_result.3:
    pms2 expressed pms2 not expressed               NA's 
                 5                  2                162 

patient.loss_expression_of_mismatch_repair_proteins_by_ihc_results.loss_expression_of_mismatch_repair_proteins_by_ihc_result.4:
    msh6 expressed msh6 not expressed               NA's 
                 5                  2                162 

patient.lymph_node_examined_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   14.00   18.00   20.98   27.50   72.00      10 

patient.lymphatic_invasion:
  no  yes NA's 
  84   67   18 

patient.microsatellite_instability:
  no NA's 
  25  144 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   5.000   6.000   6.254   7.000  12.000 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
 yes NA's 
   1  168 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
 yes NA's 
   1  168 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
1458 NA's 
   1  168 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
1217 NA's 
   1  168 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
locoregional disease                 NA's 
                   1                  168 

patient.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   1  168 

patient.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r1 NA's 
   1  168 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   2    1  166 

patient.non_nodal_tumor_deposits:
  no  yes NA's 
  60   11   98 

patient.number_of_abnormal_loci:
   0 NA's 
  17  152 

patient.number_of_first_degree_relatives_with_cancer_diagnosis:
   0    1    2 NA's 
 128   14    2   25 

patient.number_of_loci_tested:
   0    4    5    6 NA's 
   1    1   15    1  151 

patient.number_of_lymphnodes_positive_by_he:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.692   3.000  31.000      10 

patient.number_of_lymphnodes_positive_by_ihc:
   0 NA's 
  16  153 


patient.perineural_invasion_present:
  no  yes NA's 
  40   14  115 

patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        61         89         19 

patient.postoperative_rx_tx:
  no  yes NA's 
   1    2  166 

patient.preoperative_pretreatment_cea_level:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.33    1.60    3.60  136.37   10.70 7868.00      56 

patient.primary_lymph_node_presentation_assessment:
  no  yes NA's 
   5  158    6 

patient.primary_therapy_outcome_success:
complete remission/response                        NA's 
                          3                         166 

patient.prior_dx:
 no yes 
154  15 

patient.race:
                    asian black or african american                     white 
                        1                         6                        83 
                     NA's 
                       79 

patient.radiation_therapy:
  no  yes NA's 
   2    1  166 

patient.radiations.radiation.2.anatomic_treatment_site:
primary tumor field                NA's 
                  1                 168 

patient.radiations.radiation.2.bcr_radiation_barcode:
tcga-af-2692-r9920               NA's 
                 1                168 

patient.radiations.radiation.2.bcr_radiation_uuid:
94bd837c-1105-4e0c-b84c-b5392c7ebc8b                                 NA's 
                                   1                                  168 

patient.radiations.radiation.2.course_number:
   1 NA's 
   1  168 

patient.radiations.radiation.2.day_of_form_completion:
  21 NA's 
   1  168 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
 385 NA's 
   1  168 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
 350 NA's 
   1  168 

patient.radiations.radiation.2.month_of_form_completion:
  12 NA's 
   1  168 

patient.radiations.radiation.2.numfractions:
  25 NA's 
   1  168 

patient.radiations.radiation.2.radiation_dosage:
5000 NA's 
   1  168 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no NA's 
   1  168 

patient.radiations.radiation.2.radiation_type:
external beam          NA's 
            1           168 

patient.radiations.radiation.2.regimen_indication:
recurrence       NA's 
         1        168 

patient.radiations.radiation.2.units:
 cgy NA's 
   1  168 

patient.radiations.radiation.2.year_of_form_completion:
2010 NA's 
   1  168 

patient.radiations.radiation.anatomic_treatment_site:
 distant recurrence        distant site primary tumor field                NA's 
                  2                   1                  25                 141 



patient.radiations.radiation.course_number:
   1 NA's 
   8  161 

patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   13.00   13.00   14.14   13.00   28.00     140 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   89.0    92.0   105.5   177.0   157.0  1369.0     141 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   30.0    61.0    65.0   164.6   104.0  1308.0     140 

patient.radiations.radiation.measure_of_response:
complete response  partial response              NA's 
               14                 1               154 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000  12.000   9.414  12.000  12.000     140 

patient.radiations.radiation.numfractions:
  14   23   28   30 NA's 
   1    1    6    1  160 

patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     41      50      50    2123    5040    5400     146 

patient.radiations.radiation.radiation_treatment_ongoing:
  no  yes NA's 
  28    1  140 

patient.radiations.radiation.radiation_type:
     external external beam          NA's 
           17             8           144 

patient.radiations.radiation.regimen_indication:
  adjuvant palliative       NA's 
        10          1        158 

patient.radiations.radiation.units:
 cgy   gy NA's 
   9   14  146 

patient.radiations.radiation.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2012    2012    2012    2015     140 

patient.residual_tumor:
  r0   r1   r2   rx NA's 
 126    2   12    5   24 


patient.stage_event.system_version:
 5th  6th  7th NA's 
  21   71   59   18 

patient.stage_event.tnm_categories.pathologic_categories.pathologic_m:
  m0   m1  m1a   mx NA's 
 129   22    2   14    2 



patient.synchronous_colon_cancer_present:
  no  yes NA's 
 144    3   22 

patient.tissue_prospective_collection_indicator:
  no  yes NA's 
 109   57    3 

patient.tissue_retrospective_collection_indicator:
  no  yes NA's 
  57  109    3 


patient.tumor_tissue_site:
rectum   NA's 
   166      3 

patient.venous_invasion:
  no  yes NA's 
 109   37   23 

patient.vital_status:
alive  dead 
  160     9 

patient.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   40.0    64.0    77.0    79.5    90.0   172.4      92 

patient.year_of_form_completion:
2010 2011 2012 2013 
  61  104    1    3 

patient.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1999    2007    2009    2008    2010    2012 


patient.microsatellite_instability_test_results.microsatellite_instability_test_result.mononucleotide_and_dinucleotide_marker_panel_analysis_status:
indeterminate         msi-h         msi-l           mss 
            1             5            19           144 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason:
     case matched expression control     case matched methylation control 
                                  10                                    7 
primary tumor and recurrent tumor(s)                                 NA's 
                                   1                                  151 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2:
case matched methylation control              germline comparison 
                              10                                1 
                            NA's 
                             158 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3:
germline comparison                NA's 
                  5                 164 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus 
             151               18 

patient.biospecimen_cqcf.consent_or_death_status:
consented 
      169 


patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    6.0    92.0   260.0   609.9   870.0  4069.0      24 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -11.00    0.00    0.00   21.92   30.00 1391.00 

patient.biospecimen_cqcf.digital_image_submitted:
 no yes 
168   1 

patient.biospecimen_cqcf.ethnicity:
not hispanic or latino                   NA's 
                     2                    167 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site:
rectosigmoid junction                rectum         sigmoid colon 
                   50                   114                     5 

patient.biospecimen_cqcf.histological_type:
         rectal adenocarcinoma rectal mucinous adenocarcinoma 
                           150                             14 
                          NA's 
                             5 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                             168 
yes, pharmaceutical treatment prior to resection 
                                               1 

patient.biospecimen_cqcf.method_of_sample_procurement:
abdomino-perineal resection of rectum          anterior resection of rectum 
                                    6                                    98 
               other method (specify)                     sigmoid colectomy 
                                   62                                     3 


patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0    29.0   104.8    38.0  1373.0     144 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc:
0.079 0.091  0.24  NA's 
    1     1     1   166 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qm:
picogreen      NA's 
        3       166 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty:
  20 NA's 
   3  166 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol:
  84  221  254 NA's 
   1    1    1  166 

patient.biospecimen_cqcf.normal_controls.normal_control.2.method_of_normal_sample_procurement:
anterior resection of rectum                   blood draw 
                           2                           14 
      other method (specify)                         NA's 
                           6                          147 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site:
        other        rectum sigmoid colon          NA's 
            3             3             4           159 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            5           164 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1    2    3 NA's 
  11   39    1  118 

patient.biospecimen_cqcf.normal_controls.normal_control.3.bcr_sample_uuid:
679ead10-6bfd-4c7d-907a-830993fe5120                                 NA's 
                                   1                                  168 

patient.biospecimen_cqcf.normal_controls.normal_control.3.vial_number:
   2 NA's 
   1  168 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -19.00    0.00    5.00   49.33   31.00 1391.00      31 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
 0.01570  0.03535  0.12550  4.87700  0.88525 45.60000      149 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qm:
  nanodrop pico green  picogreen       NA's 
         8         10          2        149 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   19.75   20.00   18.94   20.00   20.00     149 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.50   71.03  313.00  449.23  810.00 1200.00     149 


patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site:
        other        rectum sigmoid colon          NA's 
           17            10             2           140 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           17           152 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
   1    2    3 NA's 
 134   17    1   17 



patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
  no  yes NA's 
   1  114   54 

patient.biospecimen_cqcf.path_confirm_report_attached:
 no yes 
 12 157 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
  no  yes NA's 
   1  114   54 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
  no  yes NA's 
   1  114   54 

patient.biospecimen_cqcf.race:
black or african american                      NA's 
                        3                       166 

patient.biospecimen_cqcf.sample_prescreened:
yes 
169 

patient.biospecimen_cqcf.site_of_disease:
rectum 
   169 

patient.biospecimen_cqcf.site_of_disease_description:
rectosigmoid junction         sigmoid colon                  NA's 
                   50                     5                   114 

patient.biospecimen_cqcf.submitted_for_lce:
  no NA's 
   3  166 

patient.biospecimen_cqcf.top_slide_submitted:
 no yes 
 79  90 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   5   15 NA's 
   1    1  167 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
  75   90 NA's 
   1    1  167 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
 200  233 NA's 
   1    1  167 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   1    2 NA's 
   2    5  162 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_necrosis_percent:
   0 NA's 
   5  164 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_nuclei_percent:
  70   75   80 NA's 
   2    1    2  164 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_weight:
 150  160  180 NA's 
   2    1    2  164 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.vial_number:
   1    2 NA's 
   1    4  164 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.4.tumor_necrosis_percent:
   0 NA's 
   5  164 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.4.tumor_nuclei_percent:
  70   75   80 NA's 
   2    1    2  164 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.4.tumor_weight:
 150  160  180 NA's 
   2    1    2  164 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.4.vial_number:
   1    2 NA's 
   4    1  164 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.5.bcr_sample_uuid:
0f4b61f5-0f06-4268-9b6d-4d285edd7ecc 797a1b9a-e736-41e0-a54a-3602eba38e89 
                                   1                                    1 
9e79c2b0-cd84-47d8-b0f4-e96460bb1fcc fdbb3c6c-3fe9-4d1a-abc5-bbff48a9cb3f 
                                   1                                    1 
                                NA's 
                                 165 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.5.tumor_necrosis_percent:
   0 NA's 
   4  165 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.5.tumor_nuclei_percent:
  70   75   80 NA's 
   2    1    1  165 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.5.tumor_weight:
 150  180 NA's 
   2    2  165 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.5.vial_number:
   1 NA's 
   4  165 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.6.bcr_sample_uuid:
2c755719-c529-46ae-9e1d-fbd044b78fb3 cd584ad1-7dfd-4180-ad7a-adb2f2a5496a 
                                   1                                    1 
                                NA's 
                                 167 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.6.tumor_necrosis_percent:
   0 NA's 
   2  167 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.6.tumor_nuclei_percent:
  70   80 NA's 
   1    1  167 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.6.tumor_weight:
 180 NA's 
   2  167 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.6.vial_number:
   2 NA's 
   2  167 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    5.00   20.00   12.58   20.00   30.00 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  60.00   70.00   70.00   75.21   80.00  100.00 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   19.0   100.0   150.0   264.3   253.0  1647.0 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
  1 
169 

patient.biospecimen_cqcf.vessel_used:
              cassette               cryomold               cryovial 
                    12                     18                    129 
other (please specify) 
                    10 

patient.days_to_index:
  0 
169 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
primary tumor and recurrent tumor(s)                                 NA's 
                                   1                                  168 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     26     354    1462    1639    2388    4107     136 

patient.samples.sample.2.initial_weight:
  90  210  270  400 NA's 
   1    1    2    1  164 

patient.samples.sample.2.intermediate_dimension:
 0.6  0.7  0.9    1 NA's 
   3    1    2    2  161 

patient.samples.sample.2.is_ffpe:
 no 
169 

patient.samples.sample.2.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.60    1.00    1.10    1.45    1.55    3.60     161 

patient.samples.sample.2.oct_embedded:
false  NA's 
   33   136 

patient.samples.sample.2.pathology_report_file_name:
tcga-g5-6572.7414d641-9069-4255-ade1-4d7a0ec72955.pdf 
                                                    1 
                                                 NA's 
                                                  168 

patient.samples.sample.2.pathology_report_uuid:
7414d641-9069-4255-ade1-4d7a0ec72955                                 NA's 
                                   1                                  168 

patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio:
 1.7  1.8    2 NA's 
   1    4    1  163 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
97514928 97515064 97516323 97991405 98889968 98890017      117 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   9   10 NA's 
  61    8  100 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
 0.5 NA's 
  69  100 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00    7.00   11.00   13.84   29.00   29.00     100 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  69  100 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   4    5    6 NA's 
  42    9   18  100 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.333   7.000  12.000     100 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
  40 NA's 
  69  100 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22   23 NA's 
  52   17  100 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  80 NA's 
  69  100 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
  69  100 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.136e+08 1.136e+08 1.136e+08 2.704e+08 1.136e+08 1.054e+09       157 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
  10 NA's 
  12  157 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
 0.5 NA's 
  12  157 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
  16   31 NA's 
   2   10  157 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  12  157 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   5    8 NA's 
   2   10  157 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   1    2 NA's 
   8    4  157 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
1763 1990 NA's 
   2   10  157 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
   4   10 NA's 
  10    2  157 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22 NA's 
  12  157 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
   8   20 NA's 
  10    2  157 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2011 NA's 
  12  157 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-ag-3605-10a-01w-1990-10 tcga-ag-3608-10a-01w-1990-10 
                           1                            1 
                        NA's 
                         167 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_uuid:
05acfdea-6475-470b-bcc5-d0a4fe604754 250866ef-4482-4ec1-ad62-44e2b4b44179 
                                   1                                    1 
                                NA's 
                                 167 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.biospecimen_barcode_bottom:
113628024 113628047      NA's 
        1         1       167 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.center_id:
  10 NA's 
   2  167 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.concentration:
 0.5 NA's 
   2  167 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
  31 NA's 
   2  167 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   2  167 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
   8 NA's 
   2  167 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   1 NA's 
   2  167 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_id:
1990 NA's 
   2  167 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_row:
   e    f NA's 
   1    1  167 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.quantity:
   3    4 NA's 
   1    1  167 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.source_center:
  22 NA's 
   2  167 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.volume:
   6    8 NA's 
   1    1  167 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2011 NA's 
   2  167 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
8.949e+07 9.889e+07 1.070e+08 3.656e+08 1.054e+09 1.054e+09        20 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    9   10   13 NA's 
   5    8  152    1    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.5000  0.5000  0.4873  0.5000  0.5000       3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    6.00   21.00   16.93   24.00   29.00       3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 166    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   6.000   5.295   7.000  10.000       3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.250   6.000   6.325   7.750  12.000       3 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   40.00   40.00   38.68   40.00   40.00       3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
 133   33    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
13.3   20 26.7   80 NA's 
   1    1    4  160    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 2011 2014 NA's 
  75   88    3    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    72.0   149.0   120.8   175.0   201.0      36 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                 160                    6                    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
   h    r    w NA's 
   1    5  160    3 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.5000  0.5000  0.4841  0.5000  0.5000      36 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
 160    9 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no  yes NA's 
 127   33    9 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             166 
                                            NA's 
                                               3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 166    3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
   allprep rna extraction mirvana (allprep dna) rna                   repli-g 
                        5                         1                       160 
                     NA's 
                        3 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.980   1.208   1.385   1.367   1.540   1.710     163 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.300   8.475   9.400   8.917   9.575   9.600     163 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
       39        94        36 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
   5    1  163 

patient.samples.sample.2.portions.portion.analytes.analyte.3.a260_a280_ratio:
 1.8 NA's 
   1  168 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97529432  97529462  97529491 102895632 113628012 113628093       142 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   9   10 NA's 
  27   17  125 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
  44  125 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
   5   11   29   31 NA's 
   8    9   18    9  125 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  44  125 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   4    5    6    8 NA's 
   8    9   18    9  125 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.000   5.000   5.159   6.000   7.000     125 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
1074 1990 a00i a00l NA's 
  18    9    8    9  125 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
   4   40 NA's 
   9   35  125 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22   23 NA's 
  27   17  125 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
   8   80 NA's 
   9   35  125 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 2011 NA's 
  35    9  125 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-ag-3909-10a-01x-1990-10 tcga-ag-4001-10a-01x-1990-10 
                           1                            1 
                        NA's 
                         167 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_uuid:
03bcf355-dc28-4416-bd01-5f0b3c519ae4 cbc956ea-a616-4b54-9982-e1591b242be1 
                                   1                                    1 
                                NA's 
                                 167 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.biospecimen_barcode_bottom:
113628051 113628068      NA's 
        1         1       167 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.center_id:
  10 NA's 
   2  167 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.concentration:
 0.5 NA's 
   2  167 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.day_of_shipment:
  31 NA's 
   2  167 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   2  167 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.month_of_shipment:
   8 NA's 
   2  167 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_column:
   4 NA's 
   2  167 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_id:
1990 NA's 
   2  167 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_row:
   c    d NA's 
   1    1  167 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.quantity:
   4 NA's 
   2  167 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.source_center:
  22 NA's 
   2  167 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.volume:
   8 NA's 
   2  167 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.year_of_shipment:
2011 NA's 
   2  167 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.753e+07 9.753e+07 1.039e+08 1.328e+08 1.039e+08 1.054e+09       107 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   7    9   10 NA's 
   1    8   70   90 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
0.15  0.5 NA's 
   1   78   90 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   21.00   25.00   21.51   25.00   31.00      90 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  79   90 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   6.000   6.000   7.468  10.000  10.000      90 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   5.000   6.000   6.253   7.000   9.000      90 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
   2    4   40 NA's 
   1    1   77   90 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22   23 NA's 
  56   23   90 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
   8 13.3   80 NA's 
   1    1   77   90 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
  72    7   90 

patient.samples.sample.2.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   40.2   102.0   141.0   130.4   155.8   232.0     113 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                    rna 
                     5                     73                      1 
                  NA's 
                    90 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id:
   r    w    x NA's 
   1    5   73   90 



patient.samples.sample.2.portions.portion.analytes.analyte.3.concentration:
0.15  0.5 NA's 
   1   55  113 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
  78   91 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no  yes NA's 
  55   23   91 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              79 
                                            NA's 
                                              90 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
  79   90 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
allprep rna extraction                repli-g              repli-g x 
                     1                      5                     73 
                  NA's 
                    90 

patient.samples.sample.2.portions.portion.analytes.analyte.3.rna.ratio_28s_18s:
1.25 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.3.rna.rinvalue:
 7.8 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.3.spectrophotometer_method:
picogreen   uv spec      NA's 
        1        55       113 

patient.samples.sample.2.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2 NA's 
   5    1  163 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_barcode:
tcga-g5-6572-02a-12w-1903-10                         NA's 
                           1                          168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_uuid:
a556d537-36a3-4d5a-97a2-656b59b0f5c4                                 NA's 
                                   1                                  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
1053966901       NA's 
         1        168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
  10 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
  20 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   7 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   4 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id:
1903 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_row:
   f NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2011 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.amount:
 135 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                   1                  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.analyte_type_id:
   w NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.bcr_analyte_barcode:
tcga-g5-6572-02a-12w                 NA's 
                   1                  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.bcr_analyte_uuid:
2bdace49-5acf-473f-8930-662dd87b1e8b                                 NA's 
                                   1                                  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.concentration:
 0.5 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      1     168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.spectrophotometer_method:
picogreen      NA's 
        1       168 

patient.samples.sample.2.portions.portion.analytes.analyte.4.subportion_sequence:
   2 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.700   1.800   1.900   1.867   1.900   2.200       8 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.bcr_aliquot_barcode:
tcga-ag-3727-10a-01d-1989-10                         NA's 
                           1                          168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.bcr_aliquot_uuid:
8aecf7ad-6962-433a-8d76-9a0daf6c4fde                                 NA's 
                                   1                                  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.biospecimen_barcode_bottom:
113628519      NA's 
        1       168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.center_id:
  10 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.concentration:
0.15 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.day_of_shipment:
  31 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.month_of_shipment:
   8 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_column:
   9 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_id:
1989 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_row:
   e NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.quantity:
   3 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.source_center:
  22 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.volume:
  20 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.10.year_of_shipment:
2011 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.11.bcr_aliquot_barcode:
tcga-ag-3727-10a-01d-a469-10                         NA's 
                           1                          168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.11.bcr_aliquot_uuid:
a579d39a-6b27-4724-92b6-b9fbb4e9f9d6                                 NA's 
                                   1                                  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.11.biospecimen_barcode_bottom:
167811768      NA's 
        1       168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.11.center_id:
  10 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.11.concentration:
0.08 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.11.day_of_shipment:
  21 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.11.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.11.month_of_shipment:
   1 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.11.plate_column:
   1 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.11.plate_id:
a469 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.11.plate_row:
   f NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.11.quantity:
1.07 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.11.source_center:
  23 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.11.volume:
13.4 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.11.year_of_shipment:
2015 NA's 
   1  168 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 9.885e+07 1.070e+08 2.796e+08 1.136e+08 1.054e+09        17 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   1.000   3.621   5.000  23.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0700  0.1400  0.1500  0.1447  0.1500  0.2200 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00    4.00   14.00   14.47   22.00   30.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
169 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.000   3.000   5.000   4.905   6.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   6.000   6.574  10.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.800   1.000   1.070   1.833   3.470   4.540       2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 22  23 
134  35 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.67    6.67   13.06   26.70   40.00       2 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 2014 
  74   92    3 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 9.754e+07 1.070e+08 2.829e+08 1.136e+08 1.054e+09        21 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000  10.000  10.000   9.157  10.000  23.000       3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.01000 0.05000 0.06000 0.09169 0.14000 0.18000       3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    4.00   14.00   15.04   24.00   30.00       3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 166    3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   5.223   6.000  10.000       3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.361  10.000  12.000       3 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.070   2.000   2.062   3.000   4.270       4 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
 135   31    3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.70   40.00   33.22   60.00   60.00       4 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 NA's 
  72   94    3 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
8.949e+07 9.753e+07 1.070e+08 4.827e+08 1.054e+09 1.054e+09        19 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     9.0    10.0    14.1    23.0    24.0      10 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1350  0.1500  0.1375  0.1500  0.2200      10 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   12.00   22.00   20.86   28.00   31.00      10 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 159   10 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    5.00    6.00    6.34    8.00   11.00      10 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.00    6.00    7.05   10.00   12.00      10 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   0.955   1.070   1.520   1.950   4.010      83 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
  57  102   10 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.70    6.70   14.60   13.34   60.00      83 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 NA's 
  71   88   10 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
8.949e+07 1.005e+08 1.053e+09 6.059e+08 1.053e+09 1.054e+09        84 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   10.00   24.00   16.78   24.00   24.00      84 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1500  0.1432  0.1500  0.1700      84 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   22.00   31.00   25.89   31.00   31.00      84 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  85   84 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   4.647   6.000   9.000      84 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   8.000   6.953   9.000  12.000      84 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.3200  0.9475  1.0400  1.5087  1.8600 11.8500      91 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22   23 NA's 
  54   31   84 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    6.67    6.67   13.63   13.00  237.00      91 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2011 2012 2015 NA's 
  23   59    2    1   84 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 1.134e+08 1.053e+09 7.417e+08 1.053e+09 1.053e+09       105 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   9   10   23   24 NA's 
   5   10   35   15  104 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0100  0.1300  0.1500  0.1351  0.1600  0.1700     104 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0    22.0    22.0    25.6    31.0    31.0     104 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
  65  104 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   2    3    4    8 NA's 
   1   50    5    9  104 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   7.000   7.123   9.000  12.000     104 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.000   1.010   3.452   5.600  30.000     139 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22   23 NA's 
  14   51  104 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
6.67  6.7   80  600 NA's 
  15    5    9    1  139 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2010 2011 NA's 
   5   60  104 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.885e+07 1.136e+08 1.053e+09 6.389e+08 1.053e+09 1.053e+09       119 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   2   10   23   24 NA's 
  19   20    9    3  118 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0100  0.1300  0.1400  0.1369  0.1550  0.1700     118 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   28.00   28.00   27.84   31.00   31.00     118 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
  51  118 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   3    7    8 NA's 
  29    1   21  118 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   8.000   9.000   8.608  10.000  12.000     118 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.860   2.130   2.759   3.000   6.400     127 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  22   23 NA's 
  36   15  118 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.30   13.30   23.95   20.00   80.00     127 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2010 2011 NA's 
   2   49  118 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.136e+08 1.136e+08 1.136e+08 3.178e+08 1.136e+08 1.053e+09       146 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
  10   23   24 NA's 
  18    3    2  146 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1350  0.1400  0.1448  0.1500  0.1700     146 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
  22   31 NA's 
   3   20  146 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
  23  146 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   3    8 NA's 
   5   18  146 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   5    7    8    9 NA's 
   1    5   12    5  146 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id:
1615 1620 1989 NA's 
   2    3   18  146 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.340   2.600   2.800   2.602   3.000   3.400     149 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  22   23 NA's 
  20    3  146 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
   2 6.67 14.5   20 NA's 
   1    1    1   17  149 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2011 NA's 
  23  146 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_barcode:
tcga-af-2689-10a-01d-1989-10 tcga-ag-3727-10a-01d-1620-23 
                           1                            1 
tcga-ag-3728-10a-01d-1620-23                         NA's 
                           1                          166 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_uuid:
015da186-c9a7-4242-b9c3-0e0c995452fe 08537b52-c844-4c28-b746-dcf3a5b7700b 
                                   1                                    1 
dffa871a-a031-4d1b-a3db-4b65b2ab56bc                                 NA's 
                                   1                                  166 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom:
 113628558 1052627012 1052627027       NA's 
         1          1          1        166 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.center_id:
  10   23 NA's 
   1    2  166 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.concentration:
0.14 0.15 0.17 NA's 
   1    1    1  166 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.day_of_shipment:
  22   31 NA's 
   2    1  166 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe:
  no NA's 
   3  166 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.month_of_shipment:
   3    8 NA's 
   2    1  166 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_column:
   7    8 NA's 
   1    2  166 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_id:
1620 1989 NA's 
   2    1  166 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_row:
   b    d    e NA's 
   1    1    1  166 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.quantity:
 2.8 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.source_center:
  22   23 NA's 
   1    2  166 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.volume:
  20 NA's 
   1  168 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.year_of_shipment:
2011 NA's 
   3  166 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.650e+07 9.885e+07 1.070e+08 2.794e+08 1.136e+08 1.054e+09        17 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   2  24 
 56 105   8 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1200  0.1400  0.1500  0.1494  0.1500  0.2200 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   3.00    6.00   14.00   14.84   22.00   31.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no 
169 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2.00    3.00    5.00    4.87    6.00   12.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   6.000   6.521  10.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.010   1.500   1.492   1.950   2.860       3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 22  23 
133  36 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  6.600   6.700  10.000   9.956  13.300  13.300       3 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 2014 
  76   90    3 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   20.00   68.61  157.72  232.40 1149.70 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
dna 
169 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
  d 
169 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1200  0.1400  0.1500  0.1494  0.1500  0.2200 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
169 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 no yes 
 83  86 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             169 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
169 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type     chemical lysis dna extraction 
                               34                               111 
pre-extracted dna received by tss 
                               24 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
        8       161 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
   5    1  163 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    1.0    11.0    12.0    14.4    21.0    28.0 

patient.samples.sample.2.portions.portion.is_ffpe:
 no 
169 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   4.331   6.000  11.000 

patient.samples.sample.2.portions.portion.portion_number:
  1   2  11  12 
162   1   5   1 

patient.samples.sample.2.portions.portion.portion_sequence:
   1 NA's 
  33  136 




patient.samples.sample.2.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   7  162 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   5   25   35   40 NA's 
   1    1    2    1  164 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   1    5    7 NA's 
   1    1    3  164 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_necrosis:
  15 NA's 
   1  168 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   0 NA's 
   5  164 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_normal_cells:
   0  100 NA's 
   1    5  163 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_stromal_cells:
  25 NA's 
   1  168 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_cells:
   0   60 NA's 
   5    1  163 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_nuclei:
  80 NA's 
   1  168 

patient.samples.sample.2.portions.portion.slides.slide.2.section_location:
 top NA's 
   7  162 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  13  156 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
  25   30   35   40 NA's 
   1    1    2    1  164 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   6    7    8 NA's 
   2    2    1  164 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
  10 NA's 
   1  168 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    3 NA's 
   4    1  164 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   0  100 NA's 
   1    5  163 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
  20 NA's 
   1  168 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   0   70 NA's 
   5    1  163 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
  85 NA's 
   1  168 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
bottom    top   NA's 
     7      6    156 

patient.samples.sample.2.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   30.00  100.00   85.26  118.00  200.00     142 

patient.samples.sample.2.portions.portion.year_of_creation:
2010 2011 2013 
  94   72    3 

patient.samples.sample.2.portions.shipment_portion.bcr_shipment_portion_uuid:
125cb77d-7136-4043-b7a1-463898db7281                                 NA's 
                                   1                                  168 

patient.samples.sample.2.portions.shipment_portion.center_id:
  20 NA's 
   1  168 

patient.samples.sample.2.portions.shipment_portion.is_ffpe:
  no NA's 
   1  168 

patient.samples.sample.2.portions.shipment_portion.plate_id:
2189 NA's 
   1  168 

patient.samples.sample.2.portions.shipment_portion.portion_number:
  13 NA's 
   1  168 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_bcr_aliquot_barcode:
tcga-g5-6572-02a-13-2189-20                        NA's 
                          1                         168 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_day_of_shipment:
   5 NA's 
   1  168 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_month_of_shipment:
   3 NA's 
   1  168 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_year_of_shipment:
2012 NA's 
   1  168 

patient.samples.sample.2.sample_type:
blood derived normal      recurrent tumor  solid tissue normal 
                 156                    1                   12 

patient.samples.sample.2.sample_type_id:
  2  10  11 
  1 156  12 

patient.samples.sample.2.shortest_dimension:
 0.3  0.5  0.6    1 NA's 
   3    3    1    1  161 

patient.samples.sample.2.vial_number:
  a   b 
168   1 



patient.samples.sample.3.intermediate_dimension:
 0.7  0.8  0.9 NA's 
   1    2    3  163 

patient.samples.sample.3.is_ffpe:
  no NA's 
   7  162 

patient.samples.sample.3.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.300   1.425   1.500   1.650   1.875   2.200     163 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio:
 1.8 NA's 
   5  164 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.889e+07 1.054e+09 1.054e+09 8.629e+08 1.054e+09 1.054e+09       164 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7    9 NA's 
   4    1  164 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.14 0.15 0.16  0.5 NA's 
   2    1    1    1  164 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  11   29 NA's 
   4    1  164 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   5  164 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   5    6 NA's 
   4    1  164 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   5 NA's 
   5  164 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
1073 1758 NA's 
   1    4  164 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
1.86    2 2.13   40 NA's 
   2    1    1    1  164 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
   5  164 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
13.3   80 NA's 
   4    1  164 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 2011 NA's 
   1    4  164 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-af-2689-11a-01r-a32z-07 tcga-af-2691-11a-01r-a32z-07 
                           1                            1 
tcga-af-2692-11a-01r-a32z-07 tcga-af-3400-11a-01r-a32z-07 
                           1                            1 
                        NA's 
                         165 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid:
509563ff-856a-45f0-a2e2-5ad04e652dae 523635dd-1fcb-4dbb-9405-2397c00b02be 
                                   1                                    1 
9fd9dc77-9ec1-445b-9326-254c272d38c4 a061d300-0dec-42c6-a21c-13af7bba943d 
                                   1                                    1 
                                NA's 
                                 165 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
150757406 150757407 150757409 150757425      NA's 
        1         1         1         1       165 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7 NA's 
   4  165 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.14 0.15 0.16 NA's 
   2    1    1  165 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   2 NA's 
   4  165 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   4  165 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
  10 NA's 
   4  165 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   3    4    5 NA's 
   1    2    1  165 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a32z NA's 
   4  165 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   e    f NA's 
   3    1  165 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
1.82 1.95 2.08 NA's 
   2    1    1  165 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
   4  165 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
  13 NA's 
   4  165 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2013 NA's 
   4  165 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.889e+07 5.805e+08 1.054e+09 7.822e+08 1.054e+09 1.054e+09       162 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7   10   13 NA's 
   1    2    4  162 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.14 0.15 0.16  0.5 NA's 
   2    1    2    2  162 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   6   11   20   29 NA's 
   1    4    1    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   7  162 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   4    5    6    7 NA's 
   1    4    1    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   2    5    8 NA's 
   1    5    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
1071 1660 1757 1903 NA's 
   1    1    4    1  162 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.80    2.90    3.20   13.72   22.14   40.00     162 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
   7  162 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  20 26.7   80 NA's 
   4    1    2  162 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   1    6  162 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.20   41.70   47.80   57.23   52.60  161.00     162 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                   2                    5                  162 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
   5    2  162 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
0.14 0.15 0.16  0.5 NA's 
   2    1    2    2  162 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   2  167 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no NA's 
   2  167 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               7 
                                            NA's 
                                             162 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   7  162 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
allprep rna extraction                repli-g                   NA's 
                     5                      2                    162 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.200   1.460   1.480   1.474   1.540   1.690     164 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue:
 7.1    8  8.4  8.7 NA's 
   1    1    1    2  164 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
picogreen   uv spec      NA's 
        1         6       162 

patient.samples.sample.3.portions.portion.analytes.analyte.2.subportion_sequence:
   1 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-af-3913-11a-01x-1074-09                         NA's 
                           1                          168 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_uuid:
779b1cef-0733-4dff-aa5b-7a8e09dda68e                                 NA's 
                                   1                                  168 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
97529506     NA's 
       1      168 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   9 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
  29 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   6 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   5 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
1074 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_row:
   a NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
  40 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  80 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-af-3913-11a-01x-1072-10 tcga-af-5654-11a-11w-1729-10 
                           1                            1 
                        NA's 
                         167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
6668bc7a-c23d-459f-aa7a-6c14e18f1b4a eee08d03-83ca-4e3f-8d17-bf3bb7de6cff 
                                   1                                    1 
                                NA's 
                                 167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
 97529314 108686858      NA's 
        1         1       167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
  10 NA's 
   2  167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   2  167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   4   29 NA's 
   1    1  167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   5    6 NA's 
   1    1  167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   3    5 NA's 
   1    1  167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
1072 1729 NA's 
   1    1  167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   a    b NA's 
   1    1  167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
   2  167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
   2  167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
   2  167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   1    1  167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.amount:
  51  160 NA's 
   1    1  167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                   NA's 
                     1                      1                    167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id:
   w    x NA's 
   1    1  167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.bcr_analyte_barcode:
tcga-af-3913-11a-01x tcga-af-5654-11a-11w                 NA's 
                   1                    1                  167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.bcr_analyte_uuid:
2e80b46c-2a85-4cf9-9016-1bf796f12eb9 6825ab5c-0ddd-4846-a522-aa8b4f1bd094 
                                   1                                    1 
                                NA's 
                                 167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.concentration:
 0.5 NA's 
   2  167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   2  167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
   2  167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   2  167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
        1         1       167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
      2     167 

patient.samples.sample.3.portions.portion.analytes.analyte.3.subportion_sequence:
   1 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.a260_a280_ratio:
 1.9 NA's 
   7  162 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.885e+07 5.793e+08 1.050e+09 7.802e+08 1.050e+09 1.054e+09       162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    5 NA's 
   6    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.12 0.15 0.16 NA's 
   1    4    2  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   6   14   22 NA's 
   1    5    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   7  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   2    4    6    7 NA's 
   4    1    1    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   1    2    3    7 NA's 
   2    3    1    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
1116 1550 1656 1825 NA's 
   1    4    1    1  162 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
 0.8    1 1.07 4.01 NA's 
   1    3    2    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
   7  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67 26.7 NA's 
   6    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 NA's 
   1    6  162 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.885e+07 5.785e+08 1.050e+09 7.799e+08 1.050e+09 1.054e+09       162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5    9   10 NA's 
   4    1    2  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.05 0.15 0.16 NA's 
   2    4    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   6   14   22 NA's 
   1    5    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   7  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   2    4    6    7 NA's 
   4    1    1    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   1    2    3    7 NA's 
   2    3    1    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
1117 1552 1657 1826 NA's 
   1    4    1    1  162 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.010   2.500   4.010   3.187   4.010   4.270     162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
   7  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
 6.7 26.7   40   60 NA's 
   1    4    1    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 NA's 
   1    6  162 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.754e+07 5.785e+08 1.050e+09 7.798e+08 1.050e+09 1.054e+09       162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5   10   23 NA's 
   1    5    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.05 0.12 0.15 0.16 NA's 
   4    1    1    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   6   14   22 NA's 
   1    5    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   7  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   2    4    6    7 NA's 
   4    1    1    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.000   9.000   7.286  10.000  10.000     162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
1118 1554 1658 1827 NA's 
   1    4    1    1  162 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
1.01    2 4.27 NA's 
   1    4    1  163 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
   6    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
 6.7 26.7   40 NA's 
   1    1    4  163 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 NA's 
   1    6  162 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.040e+08 1.050e+09 1.050e+09 8.930e+08 1.050e+09 1.054e+09       163 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   1    9   23 NA's 
   1    4    1  163 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.05 0.15 0.16 NA's 
   4    1    1  163 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  14   22   29 NA's 
   4    1    1  163 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   6  163 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   2    6   11 NA's 
   4    1    1  163 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   1    2    3 NA's 
   4    1    1  163 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
1428 1556 1863 NA's 
   1    4    1  163 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   1    2 NA's 
   1    4  164 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22   23 NA's 
   5    1  163 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
6.67   40 NA's 
   1    4  164 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2011 NA's 
   1    5  163 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-af-2689-11a-01d-2294-05 tcga-af-3913-11a-01d-1613-24 
                           1                            1 
                        NA's 
                         167 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
5d9ca3e8-79b3-4aaf-a7c9-7a103e46736f 93f77504-f0c1-4e93-877b-14c055e9e532 
                                   1                                    1 
                                NA's 
                                 167 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
 120990284 1052625266       NA's 
         1          1        167 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   5   24 NA's 
   1    1  167 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.15 NA's 
   2  167 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   4   31 NA's 
   1    1  167 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   2  167 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   3    6 NA's 
   1    1  167 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   5    7 NA's 
   1    1  167 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
1613 2294 NA's 
   1    1  167 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   c    h NA's 
   1    1  167 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   1    6 NA's 
   1    1  167 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22 NA's 
   2  167 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
6.67   40 NA's 
   1    1  167 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2011 2012 NA's 
   1    1  167 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-af-3913-11a-01d-1618-23                         NA's 
                           1                          168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid:
158c5edb-dbff-44d8-aebf-573c83596227                                 NA's 
                                   1                                  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
1052626424       NA's 
         1        168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
  23 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
0.15 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
  22 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   3 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   7 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id:
1618 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_row:
   c NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  23 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2011 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_barcode:
tcga-af-3913-11a-01d-1989-10                         NA's 
                           1                          168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_uuid:
41d452bf-7340-4177-9bd7-a2fea5d13c99                                 NA's 
                                   1                                  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
113628510      NA's 
        1       168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
  10 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
0.15 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
  31 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   8 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   7 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id:
1989 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_row:
   f NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
   3 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  22 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
  20 NA's 
   1  168 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2011 NA's 
   1  168 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.885e+07 5.785e+08 1.050e+09 7.799e+08 1.050e+09 1.054e+09       162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   2 NA's 
   7  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.12 0.15 0.16 NA's 
   1    4    2  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   6   14   22 NA's 
   1    5    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   7  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   2    4    6    7 NA's 
   4    1    1    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   1    2    3    7 NA's 
   2    3    1    1  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
1115 1548 1655 1824 NA's 
   1    4    1    1  162 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
 1.2    2 2.13 NA's 
   1    4    2  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22 NA's 
   7  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
  10 13.3 NA's 
   1    6  162 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   1    6  162 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   16.0   137.0   146.4   198.4   251.9   448.9     162 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
   7  162 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
   7  162 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
0.12 0.15 0.16 NA's 
   1    4    2  162 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   7  162 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no NA's 
   7  162 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               7 
                                            NA's 
                                             162 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   7  162 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type pre-extracted dna received by tss 
                                6                                 1 
                             NA's 
                              162 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      7     162 

patient.samples.sample.3.portions.portion.analytes.analyte.subportion_sequence:
   1 NA's 
   1  168 



patient.samples.sample.3.portions.portion.day_of_creation:
   4   15   16   18 NA's 
   1    1    4    1  162 

patient.samples.sample.3.portions.portion.is_ffpe:
  no NA's 
   7  162 

patient.samples.sample.3.portions.portion.month_of_creation:
   2    3    4   12 NA's 
   1    1    1    4  162 

patient.samples.sample.3.portions.portion.portion_number:
   1   11 NA's 
   6    1  162 

patient.samples.sample.3.portions.portion.slides.slide.2.bcr_slide_barcode:
tcga-af-3913-11a-01-ts1                    NA's 
                      1                     168 

patient.samples.sample.3.portions.portion.slides.slide.2.bcr_slide_uuid:
5c6a412c-af1b-4c42-8d64-a87904deb6c6                                 NA's 
                                   1                                  168 

patient.samples.sample.3.portions.portion.slides.slide.2.image_file_name:
tcga-af-3913-11a-01-ts1.5c6a412c-af1b-4c42-8d64-a87904deb6c6.svs 
                                                               1 
                                                            NA's 
                                                             168 

patient.samples.sample.3.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.3.portions.portion.slides.slide.2.section_location:
 top NA's 
   1  168 




patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   6  163 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
bottom    top   NA's 
     1      5    163 

patient.samples.sample.3.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  30.00   61.25   64.50   67.33   68.50  115.00     163 

patient.samples.sample.3.portions.portion.year_of_creation:
2010 2011 NA's 
   5    2  162 

patient.samples.sample.3.sample_type:
blood derived normal  solid tissue normal                 NA's 
                   1                    6                  162 

patient.samples.sample.3.sample_type_id:
  10   11 NA's 
   1    6  162 

patient.samples.sample.3.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.500   0.625   0.700   0.700   0.775   0.900     163 

patient.samples.sample.3.vial_number:
   a NA's 
   7  162 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     26     354    1462    1615    2388    4107     136 


patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.2:
d7bb7792-df51-498b-9d16-fc26e219249f                                 NA's 
                                   1                                  168 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  130.0   170.0   290.0   324.8   410.0   780.0     136 

patient.samples.sample.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.3000  0.6000  0.7000  0.7971  0.9000  2.1000      33 

patient.samples.sample.is_ffpe:
 no 
169 

patient.samples.sample.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   0.900   1.000   1.202   1.425   3.300      33 

patient.samples.sample.oct_embedded:
false  NA's 
   33   136 



patient.samples.sample.portions.portion.2.analytes.analyte.2.a260_a280_ratio:
 1.8 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-af-4110-01a-02r-1736-07                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
b4fa26df-1a2d-4f07-bee9-240c924ec651                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
107033558      NA's 
        1       168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.center_id:
   7 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.concentration:
0.15 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   4 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   5 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_column:
   1 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_id:
1736 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_row:
   e NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.quantity:
4.01 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.volume:
26.7 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.amount:
341.9  NA's 
    1   168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.analyte_type:
 rna NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.analyte_type_id:
   r NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.bcr_analyte_barcode:
tcga-af-4110-01a-02r                 NA's 
                   1                  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.bcr_analyte_uuid:
af5dfd2b-5c95-4f4e-bc7d-d888d66dccf9                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.concentration:
0.15 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
allprep rna extraction                   NA's 
                     1                    168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.rna.ratio_28s_18s:
1.54 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.rna.rinvalue:
 8.8 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      1     168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-af-4110-01a-02t-1735-13                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
f8e5124b-281c-408a-8d24-5ce73e3ed3e1                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
107033078      NA's 
        1       168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.center_id:
  13 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.concentration:
 0.1 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   4 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   5 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_column:
   1 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_id:
1735 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_row:
   e NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.quantity:
   3 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.volume:
  30 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2011 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.amount:
200.5  NA's 
    1   168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.analyte_type:
total rna      NA's 
        1       168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.analyte_type_id:
   t NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.bcr_analyte_barcode:
tcga-af-4110-01a-02t                 NA's 
                   1                  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.bcr_analyte_uuid:
1df92bf9-a3dc-4324-ac1f-f2256703286b                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.concentration:
 0.1 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
total rna      NA's 
        1       168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.rna.ratio_28s_18s:
1.42 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.rna.rinvalue:
 7.7 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
      1     168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_barcode:
tcga-af-4110-01a-02w-1796-10                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_uuid:
0c4da890-540b-4e09-a65c-4158ad7b04d6                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
107043135      NA's 
        1       168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.center_id:
  10 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   1 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   6 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_column:
   1 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_id:
1796 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_row:
   f NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2011 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.amount:
 179 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                   1                  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.analyte_type_id:
   w NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.bcr_analyte_barcode:
tcga-af-4110-01a-02w                 NA's 
                   1                  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.bcr_analyte_uuid:
19a35fd7-ce5e-4d7d-b9cf-daf35511e0aa                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.concentration:
 0.5 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      1     168 

patient.samples.sample.portions.portion.2.analytes.analyte.4.spectrophotometer_method:
uv spec    NA's 
      1     168 

patient.samples.sample.portions.portion.2.analytes.analyte.a260_a280_ratio:
1.89  1.9 1.98 NA's 
   1    2    2  164 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-af-4110-01a-02d-1732-01                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
74a25ecb-fb62-474b-8816-d07ff9fe8011                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
107033174      NA's 
        1       168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.center_id:
   1 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.concentration:
0.16 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   4 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   5 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_column:
   1 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_id:
1732 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_row:
   e NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.quantity:
1.07 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.volume:
6.67 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-af-4110-01a-02d-1733-10                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
2794481a-dadf-4ccc-a915-e5189600c7e4                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
107018582      NA's 
        1       168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.center_id:
  10 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.concentration:
0.05 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   4 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   5 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_column:
   1 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_id:
1733 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_row:
   e NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.quantity:
   3 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.volume:
  60 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-af-4110-01a-02d-1734-05                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
a1204547-14db-4af5-82e1-c2d2d24d9a40                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
107033846      NA's 
        1       168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.center_id:
   5 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.concentration:
0.16 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   4 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   5 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_column:
   1 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_id:
1734 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_row:
   e NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.quantity:
2.13 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.volume:
13.3 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2011 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-af-4110-01a-02d-1863-23                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
f1501a47-873e-47ad-8007-ce1c0ad95445                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
1053965003       NA's 
         1        168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.center_id:
  23 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.concentration:
0.16 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  29 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   6 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_column:
   5 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_id:
1863 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_row:
   e NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   1  168 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2011 NA's 
   1  168 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99036088  99050952  99066189 100936953 100512762 107018774       164 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.center_id:
   1    2   13 NA's 
   1    1    3  164 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.16 0.17 NA's 
   2    2    1  164 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.day_of_shipment:
   4    8   23 NA's 
   1    3    1  164 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   5  164 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.month_of_shipment:
   5    8    9 NA's 
   1    1    3  164 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_column:
   1    4    6    7 NA's 
   1    1    1    2  164 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_id:
1181 1731 a076 NA's 
   1    1    3  164 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   2.130   2.800   2.494   3.200   3.400     164 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
   2    3  164 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.volume:
 6.7 13.3   20 NA's 
   1    1    3  164 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   4    1  164 

patient.samples.sample.portions.portion.2.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  62.12   96.57  132.29  195.88  203.00  485.40     164 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type:
 dna  rna NA's 
   2    3  164 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type_id:
   d    r NA's 
   2    3  164 



patient.samples.sample.portions.portion.2.analytes.analyte.concentration:
0.14 0.16 0.17 NA's 
   2    2    1  164 

patient.samples.sample.portions.portion.2.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   2  167 

patient.samples.sample.portions.portion.2.analytes.analyte.dna.pcr_amplification_successful:
  no NA's 
   2  167 

patient.samples.sample.portions.portion.2.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               5 
                                            NA's 
                                             164 

patient.samples.sample.portions.portion.2.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   5  164 

patient.samples.sample.portions.portion.2.analytes.analyte.protocols.protocol.experimental_protocol_type:
    adna preparation type mirvana (allprep dna) rna                      NA's 
                        2                         3                       164 

patient.samples.sample.portions.portion.2.analytes.analyte.rna.ratio_28s_18s:
 1.5  1.6 NA's 
   1    2  166 

patient.samples.sample.portions.portion.2.analytes.analyte.rna.rinvalue:
 7.6  7.8  9.1 NA's 
   1    1    1  166 

patient.samples.sample.portions.portion.2.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      5     164 

patient.samples.sample.portions.portion.2.analytes.analyte.subportion_sequence:
   1 NA's 
   3  166 



patient.samples.sample.portions.portion.2.day_of_creation:
   1    6   26   30 NA's 
   2    1    1    1  164 

patient.samples.sample.portions.portion.2.is_ffpe:
  no NA's 
   5  164 

patient.samples.sample.portions.portion.2.month_of_creation:
   4    7   11 NA's 
   1    3    1  164 

patient.samples.sample.portions.portion.2.portion_number:
   2   21 NA's 
   2    3  164 

patient.samples.sample.portions.portion.2.portion_sequence:
   2 NA's 
   4  165 

patient.samples.sample.portions.portion.2.slides.slide.2.bcr_slide_barcode:
tcga-af-4110-01a-02-bs2 tcga-ag-a01l-01a-02-bs2 tcga-ag-a025-01a-02-ms2 
                      1                       1                       1 
tcga-ag-a02g-01a-02-bs2                    NA's 
                      1                     165 

patient.samples.sample.portions.portion.2.slides.slide.2.bcr_slide_uuid:
6566737c-e511-4a3a-96ed-4bfb244dbad9 6d81089c-8be4-4974-9042-4a1532909c77 
                                   1                                    1 
9b350554-4f93-4693-8a48-e2d16dda664c c9ccbfd3-a565-4fa7-b071-847509783b3e 
                                   1                                    1 
                                NA's 
                                 165 

patient.samples.sample.portions.portion.2.slides.slide.2.image_file_name:
tcga-af-4110-01a-02-bs2.c9ccbfd3-a565-4fa7-b071-847509783b3e.svs 
                                                               1 
tcga-ag-a01l-01a-02-bs2.6566737c-e511-4a3a-96ed-4bfb244dbad9.svs 
                                                               1 
tcga-ag-a025-01a-02-ms2.9b350554-4f93-4693-8a48-e2d16dda664c.svs 
                                                               1 
tcga-ag-a02g-01a-02-bs2.6d81089c-8be4-4974-9042-4a1532909c77.svs 
                                                               1 
                                                            NA's 
                                                             165 

patient.samples.sample.portions.portion.2.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   4  165 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_lymphocyte_infiltration:
   3    4   15 NA's 
   1    1    1  166 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_monocyte_infiltration:
   1    2 NA's 
   1    2  166 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_necrosis:
   1    2    3 NA's 
   1    1    2  165 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_neutrophil_infiltration:
   0    1    3 NA's 
   1    1    1  166 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_normal_cells:
  10   15   37   70 NA's 
   1    1    1    1  165 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_stromal_cells:
   0    3    5   18 NA's 
   1    1    1    1  165 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_tumor_cells:
  24   55   70   84 NA's 
   1    1    1    1  165 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_tumor_nuclei:
  25   60   75 NA's 
   1    1    2  165 

patient.samples.sample.portions.portion.2.slides.slide.2.section_location:
bottom   NA's 
     4    165 




patient.samples.sample.portions.portion.2.slides.slide.is_derived_from_ffpe:
  no NA's 
   5  164 

patient.samples.sample.portions.portion.2.slides.slide.percent_lymphocyte_infiltration:
   5   12   15 NA's 
   1    1    1  166 

patient.samples.sample.portions.portion.2.slides.slide.percent_monocyte_infiltration:
   0    3 NA's 
   1    2  166 

patient.samples.sample.portions.portion.2.slides.slide.percent_necrosis:
   1    2    5 NA's 
   1    2    2  164 

patient.samples.sample.portions.portion.2.slides.slide.percent_neutrophil_infiltration:
   0    2 NA's 
   1    2  166 

patient.samples.sample.portions.portion.2.slides.slide.percent_normal_cells:
   0    5   40   70 NA's 
   2    1    1    1  164 

patient.samples.sample.portions.portion.2.slides.slide.percent_stromal_cells:
   5    8   14 NA's 
   2    2    1  164 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_cells:
  20   50   85   90 NA's 
   1    1    2    1  164 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_nuclei:
  50   60   75   90 NA's 
   1    1    2    1  164 

patient.samples.sample.portions.portion.2.slides.slide.section_location:
 top NA's 
   5  164 

patient.samples.sample.portions.portion.2.weight:
  20   30  151 NA's 
   1    3    1  164 

patient.samples.sample.portions.portion.2.year_of_creation:
2010 2012 NA's 
   4    1  164 

patient.samples.sample.portions.portion.3.analytes.analyte.2.a260_a280_ratio:
1.76 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-ag-a026-01a-32r-a082-13                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
80d3c384-e339-42f8-9c32-549fed1a1f52                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
99026161     NA's 
       1      168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.center_id:
  13 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.concentration:
0.16 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
  22 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   9 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.plate_column:
   4 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.plate_id:
a082 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.plate_row:
   c NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.quantity:
 3.2 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.volume:
  20 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.amount:
56.03  NA's 
    1   168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.analyte_type:
 rna NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.analyte_type_id:
   r NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.bcr_analyte_barcode:
tcga-ag-a026-01a-32r                 NA's 
                   1                  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.bcr_analyte_uuid:
98560496-0080-461e-83e8-e98a978fc01d                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.concentration:
0.16 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                      NA's 
                        1                       168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.rna.ratio_28s_18s:
 1.7 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.rna.rinvalue:
 8.2 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      1     168 

patient.samples.sample.portions.portion.3.analytes.analyte.2.subportion_sequence:
   2 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.a260_a280_ratio:
1.85 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-ag-a026-01a-32r-a083-07                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
ddd5720b-3ee3-4c0d-91a1-e7925d1a46fd                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
99027197     NA's 
       1      168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.center_id:
   7 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.concentration:
0.14 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
  22 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   9 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.plate_column:
   4 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.plate_id:
a083 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.plate_row:
   c NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.quantity:
3.74 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.volume:
26.7 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.amount:
51.26  NA's 
    1   168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.analyte_type:
 rna NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.analyte_type_id:
   r NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.bcr_analyte_barcode:
tcga-ag-a026-01a-32r                 NA's 
                   1                  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.bcr_analyte_uuid:
98560496-0080-461e-83e8-e98a978fc01d                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.concentration:
0.14 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
allprep rna extraction                   NA's 
                     1                    168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.rna.ratio_28s_18s:
 1.5 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.rna.rinvalue:
 8.1 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
      1     168 

patient.samples.sample.portions.portion.3.analytes.analyte.3.subportion_sequence:
   2 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_barcode:
tcga-ag-a026-01a-32w-a096-10                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_uuid:
b7fe7664-cf0d-45e3-b7cd-c3c4dfb88ccc                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
100489954      NA's 
        1       168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.center_id:
  10 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   6 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
  10 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.plate_column:
   6 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.plate_id:
a096 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.plate_row:
   d NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.source_center:
  23 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2010 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                   1                  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.analyte_type_id:
   w NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.bcr_analyte_barcode:
tcga-ag-a026-01a-32w                 NA's 
                   1                  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.bcr_analyte_uuid:
79223a2d-5272-4c3c-9da8-bccf3cd30dc3                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.dna.pcr_amplification_successful:
 yes NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      1     168 

patient.samples.sample.portions.portion.3.analytes.analyte.4.subportion_sequence:
   2 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.aliquots.aliquot.bcr_aliquot_barcode:
tcga-ag-a026-01a-32x-a095-10                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.aliquots.aliquot.bcr_aliquot_uuid:
34855b20-9ccc-4050-86eb-2f3258fce529                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.aliquots.aliquot.biospecimen_barcode_bottom:
99029514     NA's 
       1      168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.aliquots.aliquot.center_id:
  10 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.aliquots.aliquot.concentration:
 0.5 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.aliquots.aliquot.day_of_shipment:
   6 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.aliquots.aliquot.month_of_shipment:
  10 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.aliquots.aliquot.plate_column:
   6 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.aliquots.aliquot.plate_id:
a095 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.aliquots.aliquot.plate_row:
   d NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.aliquots.aliquot.quantity:
  40 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.aliquots.aliquot.source_center:
  23 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.aliquots.aliquot.volume:
  80 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.aliquots.aliquot.year_of_shipment:
2010 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.analyte_type:
repli-g x (qiagen) dna                   NA's 
                     1                    168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.analyte_type_id:
   x NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.bcr_analyte_barcode:
tcga-ag-a026-01a-32x                 NA's 
                   1                  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.bcr_analyte_uuid:
e067e7ae-1e41-4afa-9bac-f3454fce2238                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.dna.normal_tumor_genotype_match:
 yes NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.dna.pcr_amplification_successful:
 yes NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.protocols.protocol.experimental_protocol_type:
repli-g x      NA's 
        1       168 

patient.samples.sample.portions.portion.3.analytes.analyte.5.subportion_sequence:
   2 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.790   1.830   1.910   1.886   1.940   1.970     160 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-ag-a026-01a-32d-a079-02                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
9a12344e-d311-43cf-ad9d-19fa8a89d111                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
99015448     NA's 
       1      168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.center_id:
   2 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.concentration:
0.15 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  22 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   9 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.plate_column:
   5 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.plate_id:
a079 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.plate_row:
   d NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.quantity:
1.95 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.volume:
  13 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-ag-a026-01a-32d-a080-01                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
0e2365ab-f8ff-401a-bb6e-c75dd6f752f2                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
99015419     NA's 
       1      168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.center_id:
   1 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.concentration:
0.15 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  22 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   9 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.plate_column:
   5 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.plate_id:
a080 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.plate_row:
   d NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.quantity:
   1 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.volume:
6.67 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-ag-a026-01a-32d-a081-05                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
e2ebe633-e76f-4e04-bfe0-a6711027e8e0                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
99015459     NA's 
       1      168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.center_id:
   5 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.concentration:
0.15 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  22 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   9 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.plate_column:
   5 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.plate_id:
a081 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.plate_row:
   d NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.quantity:
4.01 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.volume:
26.7 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-ag-a026-01a-32d-a094-10                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
c14b0ec9-6195-48ea-be3f-0c778ab670cb                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
100489879      NA's 
        1       168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.center_id:
  10 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.concentration:
0.08 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   6 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
  10 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.plate_column:
   6 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.plate_id:
a094 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.plate_row:
   d NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.quantity:
2.08 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.volume:
  26 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-ag-a026-01a-32d-a13g-24                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
1f615468-51f2-4b48-9234-7e8740125757                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
100473425      NA's 
        1       168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.center_id:
  24 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.concentration:
0.15 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  31 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   3 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.plate_column:
   3 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.plate_id:
a13g NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.plate_row:
   b NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.quantity:
   1 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.volume:
6.67 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2011 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-ag-a026-01a-32d-a183-10                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid:
75846f72-de42-461b-b9d9-fc9f34bc375b                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
113415867      NA's 
        1       168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.center_id:
  10 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.concentration:
0.08 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
  31 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   8 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.plate_column:
   4 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.plate_id:
a183 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.plate_row:
   g NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.quantity:
 6.4 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.source_center:
  23 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.volume:
  80 NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2011 NA's 
   1  168 



patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
99013496 99024933 99028650 99030835 99031359 99066371      161 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.center_id:
  13   23 NA's 
   8    1  160 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.concentration:
0.13 0.14 0.16 NA's 
   1    3    4  161 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.day_of_shipment:
   8 NA's 
   8  161 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   8  161 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.month_of_shipment:
   9 NA's 
   8  161 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.plate_column:
   6    7 NA's 
   7    1  161 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.plate_id:
a076 yyyy NA's 
   8    1  160 


patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.quantity:
 2.6  2.8  3.2 NA's 
   1    3    4  161 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   9  160 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.volume:
  20 NA's 
   8  161 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 NA's 
   8  161 

patient.samples.sample.portions.portion.3.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  48.99   58.03   72.60   73.85   89.75  100.65     160 

patient.samples.sample.portions.portion.3.analytes.analyte.analyte_type:
 dna  rna NA's 
   1    8  160 

patient.samples.sample.portions.portion.3.analytes.analyte.analyte_type_id:
   d    r NA's 
   1    8  160 



patient.samples.sample.portions.portion.3.analytes.analyte.concentration:
0.13 0.14 0.15 0.16 NA's 
   1    3    1    4  160 

patient.samples.sample.portions.portion.3.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   1  168 

patient.samples.sample.portions.portion.3.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               9 
                                            NA's 
                                             160 

patient.samples.sample.portions.portion.3.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   9  160 

patient.samples.sample.portions.portion.3.analytes.analyte.protocols.protocol.experimental_protocol_type:
    adna preparation type mirvana (allprep dna) rna                      NA's 
                        1                         8                       160 

patient.samples.sample.portions.portion.3.analytes.analyte.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   1.500   1.600   1.675   1.850   2.000     161 

patient.samples.sample.portions.portion.3.analytes.analyte.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.500   7.900   8.200   8.425   9.075   9.600     161 

patient.samples.sample.portions.portion.3.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      9     160 

patient.samples.sample.portions.portion.3.analytes.analyte.subportion_sequence:
   1    2 NA's 
   8    1  160 



patient.samples.sample.portions.portion.3.day_of_creation:
   1   26   30 NA's 
   7    1    1  160 

patient.samples.sample.portions.portion.3.is_ffpe:
  no NA's 
   9  160 

patient.samples.sample.portions.portion.3.month_of_creation:
   6    7 NA's 
   1    8  160 

patient.samples.sample.portions.portion.3.portion_number:
  21   32 NA's 
   8    1  160 

patient.samples.sample.portions.portion.3.portion_sequence:
   2    3 NA's 
   8    1  160 




patient.samples.sample.portions.portion.3.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   9  160 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   5.778  10.000  15.000     160 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_monocyte_infiltration:
   1    2 NA's 
   5    4  160 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_necrosis:
   1    2    3 NA's 
   6    2    1  160 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   1.000   1.889   3.000   5.000     160 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   10.00   14.11   15.00   44.00     160 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    3.00    9.00   10.56   20.00   25.00     160 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  53.00   60.00   70.00   73.89   81.00   99.00     160 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  60.00   65.00   70.00   71.11   80.00   80.00     160 

patient.samples.sample.portions.portion.3.slides.slide.2.section_location:
bottom    top   NA's 
     7      2    160 




patient.samples.sample.portions.portion.3.slides.slide.is_derived_from_ffpe:
  no NA's 
   9  160 

patient.samples.sample.portions.portion.3.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   5.333   8.000  11.000     160 

patient.samples.sample.portions.portion.3.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   2.000   1.889   2.000   4.000     160 

patient.samples.sample.portions.portion.3.slides.slide.percent_necrosis:
   1    2   60 NA's 
   7    1    1  160 

patient.samples.sample.portions.portion.3.slides.slide.percent_neutrophil_infiltration:
   0    1    2    3 NA's 
   3    4    1    1  160 

patient.samples.sample.portions.portion.3.slides.slide.percent_normal_cells:
   0    5   10 NA's 
   7    1    1  160 

patient.samples.sample.portions.portion.3.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.000   4.000   8.000   8.444  14.000  15.000     160 

patient.samples.sample.portions.portion.3.slides.slide.percent_tumor_cells:
  25   85   90   95 NA's 
   1    4    1    3  160 

patient.samples.sample.portions.portion.3.slides.slide.percent_tumor_nuclei:
  80   85   90   95 NA's 
   4    2    1    2  160 

patient.samples.sample.portions.portion.3.slides.slide.section_location:
bottom    top   NA's 
     2      7    160 

patient.samples.sample.portions.portion.3.weight:
  30   40 NA's 
   8    1  160 

patient.samples.sample.portions.portion.3.year_of_creation:
2010 NA's 
   9  160 

patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   1.800   1.800   1.784   1.800   1.960       3 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.754e+07 9.902e+07 9.902e+07 3.570e+08 6.023e+08 1.054e+09       154 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7    9 NA's 
  13    2  154 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.14 0.15 0.16  0.5 NA's 
   3    5    5    2  154 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     6.0    10.0    11.2    10.0    29.0     154 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  15  154 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   5    6    7   10 NA's 
   7    2    5    1  154 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   1    2    7 NA's 
   8    3    4  154 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.860   2.105   4.010   8.159   4.270  40.000     154 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22   23 NA's 
   7    8  154 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13 13.3 26.7   80 NA's 
   1    4    8    2  154 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 2011 2013 NA's 
   3   11    1  154 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-af-2689-01a-01r-a466-07 tcga-ag-3902-01a-01r-a32z-07 
                           1                            1 
                        NA's 
                         167 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid:
7737e9f0-02f1-427b-9acf-619aa5563059 c1dbdbe7-4cd9-4363-acf3-ed83cef757f2 
                                   1                                    1 
                                NA's 
                                 167 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
150757375 167812938      NA's 
        1         1       167 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7 NA's 
   2  167 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.14 0.16 NA's 
   1    1  167 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   2    9 NA's 
   1    1  167 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   2  167 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
  10   12 NA's 
   1    1  167 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   6 NA's 
   2  167 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a32z a466 NA's 
   1    1  167 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   b    e NA's 
   1    1  167 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
0.92 2.08 NA's 
   1    1  167 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
   2  167 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
 6.6   13 NA's 
   1    1  167 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2013 2014 NA's 
   1    1  167 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
8.378e+07 9.754e+07 1.070e+08 2.677e+08 1.136e+08 1.054e+09        18 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7   10   13 NA's 
  85    2   81    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1500  0.1573  0.1525  0.6000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    5.50   14.00   13.86   22.00   30.00       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 168    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   5.000   4.875   6.000   9.000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   2.994   4.000  12.000       1 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.600   3.000   3.740   4.033   4.010  40.000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
 135   33    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  20 26.7   80 NA's 
  81   85    2    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 2011 2014 NA's 
  76   89    3    1 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.50   71.88  176.31  254.49  400.07  855.30       1 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                   2                  166                    1 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
   h    r    w NA's 
  76   90    2    1 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1500  0.1574  0.1525  0.6000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   2  167 

patient.samples.sample.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no NA's 
   2  167 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             168 
                                            NA's 
                                               1 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 168    1 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
   allprep rna extraction mirvana (allprep dna) rna                   repli-g 
                       85                        81                         2 
                     NA's 
                        1 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.300   1.580   1.547   1.750   3.200       4 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   8.300   8.900   8.684   9.300  10.000       4 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
    168       1 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
  77    4   88 

patient.samples.sample.portions.portion.analytes.analyte.3.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.700   1.800   1.469   1.800   1.980      71 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97528977  98189797 150756981 128133967 150757394 150757431       162 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   7    9   13 NA's 
   4    2    1  162 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.1 0.15 0.16  0.5 NA's 
   1    1    3    2  162 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
   2   14   29 NA's 
   4    1    2  162 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   7  162 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   6    7   10 NA's 
   2    1    4  162 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   1    2    3    5 NA's 
   3    1    2    1  162 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
1074 1120 a32y a32z NA's 
   2    1    1    3  162 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_row:
   a    b    d    g NA's 
   1    1    1    4  162 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
1.95 2.08    3   40 NA's 
   1    3    1    2  162 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22   23 NA's 
   3    4  162 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  13   30   80 NA's 
   4    1    2  162 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 2013 NA's 
   3    4  162 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 9.885e+07 1.070e+08 2.690e+08 1.136e+08 1.054e+09        19 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   7   10   13 NA's 
  81    2   84    2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.1000  0.1000  0.1301  0.1500  0.5000       2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0     5.0    14.0    13.9    22.0    30.0       2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 167    2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   5.000   4.886   6.000   9.000       2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   2.994   4.000  12.000       2 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.860   2.130   3.000   3.425   3.000  40.000       2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22   23 NA's 
 135   32    2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   13.30   30.00   25.46   30.00   80.00       2 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 2014 NA's 
  75   89    3    2 

patient.samples.sample.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   34.65   74.00   93.81  127.60  704.00       2 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
repli-g x (qiagen) dna                    rna              total rna 
                     2                     81                     84 
                  NA's 
                     2 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
   r    t    x NA's 
  81   84    2    2 



patient.samples.sample.portions.portion.analytes.analyte.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.1000  0.1000  0.1301  0.1500  0.5000       2 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   2  167 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
   2  167 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             167 
                                            NA's 
                                               2 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 167    2 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
allprep rna extraction              repli-g x              total rna 
                    81                      2                     84 
                  NA's 
                     2 

patient.samples.sample.portions.portion.analytes.analyte.3.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.660   1.220   1.520   1.474   1.670   2.120      72 

patient.samples.sample.portions.portion.analytes.analyte.3.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   7.900   8.500   8.452   9.100   9.800      72 

patient.samples.sample.portions.portion.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
    167       2 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2 NA's 
  77    4   88 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
97514937 97515026 97515285 97973980 98889952 98890440      118 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.center_id:
   9   10 NA's 
  60    8  101 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
  68  101 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00    7.00    9.00   13.62   17.75   29.00     101 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  68  101 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
   4    5    6 NA's 
  42    9   17  101 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.559   5.000   7.000     101 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.quantity:
  40 NA's 
  68  101 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.source_center:
  22   23 NA's 
  51   17  101 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.volume:
  80 NA's 
  68  101 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
  68  101 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-ag-3608-01a-01w-1763-10                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_uuid:
2e139928-c90e-45dc-a0bf-85bcaf521c7f                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.biospecimen_barcode_bottom:
1053960493       NA's 
         1        168 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.center_id:
  10 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.concentration:
 0.5 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.day_of_shipment:
  16 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.month_of_shipment:
   5 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_column:
   1 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_id:
1763 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_row:
   e NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.quantity:
  10 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.source_center:
  22 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.volume:
  20 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.year_of_shipment:
2011 NA's 
   1  168 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.651e+07 9.889e+07 1.070e+08 3.711e+08 1.054e+09 1.054e+09        23 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   9   10 NA's 
   8  155    6 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
 163    6 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    6.00   21.00   16.94   24.00   29.00       6 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 163    6 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   6.000   5.294   7.000  10.000       6 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.399   5.000   7.000       6 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
 163    6 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22   23 NA's 
 131   32    6 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
 163    6 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2010 2011 2014 NA's 
  73   87    3    6 

patient.samples.sample.portions.portion.analytes.analyte.4.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    80.0   143.0   119.8   169.0   215.0      38 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                 163                    6 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type_id:
   w NA's 
 163    6 



patient.samples.sample.portions.portion.analytes.analyte.4.concentration:
 0.5 NA's 
 131   38 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
 163    6 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no  yes NA's 
 131   32    6 

patient.samples.sample.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             163 
                                            NA's 
                                               6 

patient.samples.sample.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
 163    6 

patient.samples.sample.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
    163       6 

patient.samples.sample.portions.portion.analytes.analyte.4.spectrophotometer_method:
picogreen   uv spec      NA's 
       41        90        38 

patient.samples.sample.portions.portion.analytes.analyte.4.subportion_sequence:
   1    2 NA's 
  77    4   88 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
97528972 97529437 97529461 97529384 97529486 97529509      152 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.center_id:
   9   10 NA's 
  26    8  135 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.concentration:
 0.5 NA's 
  34  135 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.day_of_shipment:
   5   11   29 NA's 
   8    9   17  135 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  34  135 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.month_of_shipment:
   4    5    6 NA's 
   8    9   17  135 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_column:
   1    2    3    4 NA's 
   7   12   13    2  135 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_id:
1074 a00i a00l NA's 
  17    8    9  135 


patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.quantity:
  40 NA's 
  34  135 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.source_center:
  22   23 NA's 
  17   17  135 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.volume:
  80 NA's 
  34  135 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
  34  135 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97528965  97529294 103948670 101479716 103948714 103948741       116 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.center_id:
   9   10 NA's 
   8   62   99 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.concentration:
 0.5 NA's 
  70   99 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   23.00   25.00   22.59   25.00   29.00      99 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  70   99 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.month_of_shipment:
   3    5    6   10 NA's 
   8    9   17   36   99 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   2.814   3.750   6.000      99 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.quantity:
  40 NA's 
  70   99 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.source_center:
  22   23 NA's 
  48   22   99 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.volume:
  80 NA's 
  70   99 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.year_of_shipment:
2010 NA's 
  70   99 

patient.samples.sample.portions.portion.analytes.analyte.5.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   56.0    97.5   148.0   129.4   159.2   186.0     121 

patient.samples.sample.portions.portion.analytes.analyte.5.analyte_type:
repli-g x (qiagen) dna                   NA's 
                    70                     99 

patient.samples.sample.portions.portion.analytes.analyte.5.analyte_type_id:
   x NA's 
  70   99 



patient.samples.sample.portions.portion.analytes.analyte.5.concentration:
 0.5 NA's 
  48  121 

patient.samples.sample.portions.portion.analytes.analyte.5.dna.normal_tumor_genotype_match:
 yes NA's 
  70   99 

patient.samples.sample.portions.portion.analytes.analyte.5.dna.pcr_amplification_successful:
  no  yes NA's 
  48   22   99 

patient.samples.sample.portions.portion.analytes.analyte.5.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              70 
                                            NA's 
                                              99 

patient.samples.sample.portions.portion.analytes.analyte.5.is_derived_from_ffpe:
  no NA's 
  70   99 

patient.samples.sample.portions.portion.analytes.analyte.5.protocols.protocol.experimental_protocol_type:
repli-g x      NA's 
       70        99 

patient.samples.sample.portions.portion.analytes.analyte.5.spectrophotometer_method:
uv spec    NA's 
     48     121 

patient.samples.sample.portions.portion.analytes.analyte.5.subportion_sequence:
   1    2 NA's 
   4    1  164 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.650   1.900   1.900   1.885   1.900   2.120       8 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.136e+08 3.477e+08 1.050e+09 7.383e+08 1.050e+09 1.053e+09       163 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.center_id:
   9   10   23 NA's 
   2    3    1  163 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.concentration:
0.05 0.14 0.15 0.16 NA's 
   3    1    1    1  163 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.day_of_shipment:
  14   22   31 NA's 
   3    1    2  163 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.is_derived_from_ffpe:
  no NA's 
   6  163 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.month_of_shipment:
   2    3    8 NA's 
   3    1    2  163 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_column:
   1    4    5    7 NA's 
   1    2    1    2  163 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_id:
1554 1555 1620 1989 NA's 
   1    2    1    2  163 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_row:
   a    b    c    e NA's 
   2    1    1    2  163 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.quantity:
   2    3  3.2 NA's 
   3    1    1  164 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.source_center:
  22   23 NA's 
   5    1  163 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.volume:
  20   40 NA's 
   2    3  164 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.year_of_shipment:
2011 NA's 
   6  163 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.bcr_aliquot_barcode:
tcga-af-2689-01a-01d-1555-09 tcga-af-2691-01a-01d-1615-24 
                           1                            1 
tcga-af-3400-01a-01d-1615-24 tcga-ag-3727-01a-01d-a469-10 
                           1                            1 
                        NA's 
                         165 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.bcr_aliquot_uuid:
1f39af67-7408-4432-bd1c-2cb8eabe711b 79f1b2f2-0390-4f56-b662-0bbe87f1fa56 
                                   1                                    1 
7fb3c6a1-40ac-46ab-b4dd-40ed3039c852 919ae66d-cc80-40b1-aa70-ff84faaa4be7 
                                   1                                    1 
                                NA's 
                                 165 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.biospecimen_barcode_bottom:
 167811791 1049986001 1052625356 1052625380       NA's 
         1          1          1          1        165 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.center_id:
   9   10   24 NA's 
   1    1    2  165 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.concentration:
0.05 0.08 0.15 0.16 NA's 
   1    1    1    1  165 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.day_of_shipment:
  14   21   31 NA's 
   1    1    2  165 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.is_derived_from_ffpe:
  no NA's 
   4  165 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.month_of_shipment:
   1    2    3 NA's 
   1    1    2  165 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.plate_column:
   1    6 NA's 
   3    1  165 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.plate_id:
1555 1615 a469 NA's 
   1    2    1  165 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.plate_row:
   b    d    e    h NA's 
   1    1    1    1  165 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.quantity:
   1 1.07    2 NA's 
   1    2    1  165 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.source_center:
  22   23 NA's 
   3    1  165 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.volume:
6.67 13.4   40 NA's 
   2    1    1  165 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.11.year_of_shipment:
2011 2015 NA's 
   3    1  165 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.bcr_aliquot_barcode:
tcga-af-2689-01a-01d-1615-24 tcga-af-2691-01a-01d-1620-23 
                           1                            1 
tcga-af-3400-01a-01d-1620-23                         NA's 
                           1                          166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.bcr_aliquot_uuid:
1a293dcc-cd00-4606-9e6d-0e664e17403d d02810b7-ff5f-4925-887f-392263b892b1 
                                   1                                    1 
e3d12a82-638a-4734-9d40-d5f67c3c188e                                 NA's 
                                   1                                  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.biospecimen_barcode_bottom:
1052625333 1052626995 1052627019       NA's 
         1          1          1        166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.center_id:
  23   24 NA's 
   2    1  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.concentration:
0.15 0.16 NA's 
   2    1  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.day_of_shipment:
  22   31 NA's 
   2    1  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.is_derived_from_ffpe:
  no NA's 
   3  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.month_of_shipment:
   3 NA's 
   3  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.plate_column:
   1 NA's 
   3  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.plate_id:
1615 1620 NA's 
   1    2  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.plate_row:
   a    b    d NA's 
   1    1    1  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.quantity:
   1 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.source_center:
  22   23 NA's 
   1    2  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.volume:
6.67 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.12.year_of_shipment:
2011 NA's 
   3  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.bcr_aliquot_barcode:
tcga-af-2689-01a-01d-1620-23 tcga-af-2691-01a-01d-1989-10 
                           1                            1 
tcga-af-3400-01a-01d-1989-10                         NA's 
                           1                          166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.bcr_aliquot_uuid:
0b27a278-ac5f-4d65-8bfd-6f16f48be737 4fe80cab-7ff0-4a39-a0b8-2aca9fd9c804 
                                   1                                    1 
81b1e78c-6032-4ff4-b52a-83456b9450ea                                 NA's 
                                   1                                  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.biospecimen_barcode_bottom:
 113628528  113628552 1052626972       NA's 
         1          1          1        166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.center_id:
  10   23 NA's 
   2    1  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.concentration:
0.15 0.16 NA's 
   2    1  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.day_of_shipment:
  22   31 NA's 
   1    2  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.is_derived_from_ffpe:
  no NA's 
   3  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.month_of_shipment:
   3    8 NA's 
   1    2  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.plate_column:
   1 NA's 
   3  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.plate_id:
1620 1989 NA's 
   1    2  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.plate_row:
   a    b    d NA's 
   1    1    1  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.quantity:
   3  3.2 NA's 
   1    1  167 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.source_center:
  22   23 NA's 
   2    1  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.volume:
  20 NA's 
   2  167 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.13.year_of_shipment:
2011 NA's 
   3  166 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.bcr_aliquot_barcode:
tcga-af-2689-01a-01d-1989-10                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.bcr_aliquot_uuid:
5b180356-cf58-4fad-a3d4-00fc12b43fcc                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.biospecimen_barcode_bottom:
113628575      NA's 
        1       168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.center_id:
  10 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.concentration:
0.15 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.day_of_shipment:
  31 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.month_of_shipment:
   8 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.plate_column:
   1 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.plate_id:
1989 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.plate_row:
   a NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.quantity:
   3 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.source_center:
  22 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.volume:
  20 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.14.year_of_shipment:
2011 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.bcr_aliquot_barcode:
tcga-af-2689-01a-01d-2294-05                         NA's 
                           1                          168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.bcr_aliquot_uuid:
0bfbf2b2-de69-4dd5-ba6b-9a023ea3761e                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.biospecimen_barcode_bottom:
120988627      NA's 
        1       168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.center_id:
   5 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.concentration:
0.15 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.day_of_shipment:
   4 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.is_derived_from_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.month_of_shipment:
   6 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.plate_column:
   5 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.plate_id:
2294 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.plate_row:
   g NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.quantity:
   6 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.source_center:
  22 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.volume:
  40 NA's 
   1  168 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.15.year_of_shipment:
2012 NA's 
   1  168 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 9.885e+07 1.070e+08 2.820e+08 1.136e+08 1.054e+09        19 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   3.707   5.000  24.000       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1500  0.1499  0.1500  0.1700       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    5.00   14.00   14.57   22.00   31.00       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 167    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   5.000   4.778   6.000   9.000       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.102   5.000   9.000       2 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.000   1.070   2.243   4.010   4.540       4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
 133   34    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.60    6.67    6.67   14.91   26.70   26.70       4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 2014 NA's 
  73   91    3    2 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 9.753e+07 1.070e+08 2.830e+08 1.136e+08 1.054e+09        20 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   9.000  10.000   8.922  10.000  23.000       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.01000 0.05000 0.07000 0.09341 0.15000 0.17000       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    5.00   22.00   16.31   24.00   30.00       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 167    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   5.156   6.000   9.000       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   2.982   4.000  11.000       2 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.87    1.07    2.00    2.07    3.00    4.01       3 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
 134   33    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.70   40.00   33.29   60.00   60.00       3 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 2014 NA's 
  72   92    3    2 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 9.754e+07 1.070e+08 2.714e+08 1.136e+08 1.054e+09        21 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00    9.00   10.72   10.00   23.00       4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0800  0.1400  0.1500  0.1422  0.1500  0.1700       4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    4.00   14.00   14.77   22.00   31.00       4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 165    4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   5.388   6.000   9.000       4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   2.891   4.000   6.000       4 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.810   1.010   1.105   1.792   2.000   4.540      43 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22   23 NA's 
  94   71    4 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.20    6.70   13.30   12.86   13.34   40.00      43 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 2014 NA's 
  72   90    3    4 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.751e+07 1.005e+08 1.053e+09 6.148e+08 1.054e+09 1.054e+09        13 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    5.00   11.34   23.00   24.00      11 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1500  0.1427  0.1500  0.1700      11 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   22.00   22.00   21.39   29.00   31.00      11 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
 158   11 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.000   6.000   6.741   8.000  11.000      11 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.481   6.000  11.000      11 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   1.070   1.860   1.948   2.000  30.000      50 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22   23 NA's 
  90   68   11 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    6.67   13.30   18.31   13.30  600.00      50 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2011 NA's 
  53  105   11 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.648e+07 1.005e+08 1.053e+09 6.317e+08 1.053e+09 1.054e+09        90 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   10.00   23.00   19.24   24.00   24.00      89 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0100  0.1500  0.1500  0.1439  0.1525  0.1700      89 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   22.00   31.00   25.74   31.00   31.00      89 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
  80   89 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   3.513   3.000   9.000      89 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.475   4.000  11.000      89 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.280   1.000   1.000   2.135   1.335  30.000     109 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22   23 NA's 
  31   49   89 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    6.67    6.67   23.17   13.00  600.00     109 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2010 2011 2015 NA's 
   7   72    1   89 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.885e+07 1.134e+08 1.053e+09 7.387e+08 1.053e+09 1.053e+09       103 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00   10.00   13.48   23.00   24.00      96 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0100  0.1100  0.1500  0.1309  0.1500  0.1700     102 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   22.00   28.00   25.21   31.00   31.00     102 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
  67  102 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   2    3    7    8 NA's 
   4   45    1   17  102 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   5.358   9.000  11.000     102 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.195   3.359   6.400   7.200     125 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  22   23 NA's 
  28   45   96 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.30   13.30   35.59   80.00   80.00     125 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2010 2011 2012 NA's 
   2   64    1  102 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.885e+07 1.136e+08 1.136e+08 2.606e+08 1.136e+08 1.054e+09       118 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   10.00   12.37   10.00   24.00     109 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0800  0.1450  0.1500  0.1482  0.1600  0.1700     118 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   31.00   31.00   28.49   31.00   31.00     118 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
  51  118 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   8.000   8.000   7.137   8.000   8.000     118 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.500   4.000   3.922   5.500   7.000     118 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   3.149   3.200   6.400     120 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  22   23 NA's 
  47   13  109 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   20.00   20.00   22.45   20.00   80.00     120 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2010 2011 2012 NA's 
   1   49    1  118 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.136e+08 8.159e+08 1.050e+09 8.169e+08 1.053e+09 1.053e+09       161 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.center_id:
   5   10   23   24 NA's 
   1    4    2    1  161 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1100  0.1450  0.1225  0.1500  0.1600     161 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.day_of_shipment:
  14   22   31 NA's 
   3    2    3  161 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe:
  no NA's 
   8  161 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.month_of_shipment:
   2    3    8 NA's 
   3    3    2  161 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_column:
   3    4    5    6 NA's 
   2    3    2    1  161 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1551    1554    1616    1686    1712    1989     161 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_row:
   b    c    g    h NA's 
   3    2    2    1  161 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   2.000   2.300   2.423   2.900   4.010     163 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.source_center:
  22   23 NA's 
   6    2  161 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.volume:
6.67   20 26.7   40 NA's 
   1    2    1    2  163 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.year_of_shipment:
2011 NA's 
   8  161 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.181e+07 9.752e+07 1.070e+08 2.975e+08 1.136e+08 1.054e+09        32 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
 1  2  5 23 24 
50 99  1 17  2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1547  0.1500  1.0100      17 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    4.00   14.00   13.43   22.00   31.00      17 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 152   17 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    3.00    5.00    4.77    6.00    8.00      17 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   2.908   4.000   6.000      17 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.010   1.500   1.678   2.000  26.970      20 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 22  23 
133  36 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.60    6.70   10.00   10.21   13.30   26.70      20 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
  68   84   17 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    2.0   203.0   300.0   362.2   500.0  1020.2 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
dna 
169 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
  d 
169 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.130   0.150   0.150   0.155   0.150   1.010 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
169 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 no yes 
 91  78 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             169 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
169 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type 
                  169 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
        8       161 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
  76    4   89 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    8.00   10.00   14.17   25.00   31.00 

patient.samples.sample.portions.portion.is_ffpe:
 no 
169 

patient.samples.sample.portions.portion.lce:
 lce NA's 
   3  166 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.414   6.000  12.000 

patient.samples.sample.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   2.000   7.213  11.000  41.000 

patient.samples.sample.portions.portion.portion_sequence:
   1    2    3    4 NA's 
  24    6    3    1  135 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
 148   21 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   10.00   10.58   15.00   50.00     124 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   2.029   2.000  15.000     134 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   4.000   4.446   5.000  20.000      21 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   2.000   4.135   3.000  40.000     132 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   7.234  10.000  89.000      24 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   12.00   13.43   20.00   50.00      24 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   70.00   76.00   75.33   85.00   98.00      21 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   75.00   75.00   75.95   80.00   97.00      21 

patient.samples.sample.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
    21    127     21 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
 no 
169 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   3.750   7.000   8.135  10.250  50.000     117 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   1.227   2.000   5.000     125 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   1.000   5.000   4.893   8.000  20.000 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   2.936   3.000  40.000     122 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   7.108  10.000  70.000       2 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    6.00   10.00   12.65   15.00   52.00       4 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   20.0    69.0    77.0    75.8    85.0   100.0 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  50.00   70.00   75.00   76.34   80.00   95.00 

patient.samples.sample.portions.portion.slides.slide.section_location:
bottom    top 
   127     42 

patient.samples.sample.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  10.00   30.00   55.00   73.79  118.00  174.00 

patient.samples.sample.portions.portion.year_of_creation:
2009 2010 2011 2012 
   6   83   76    4 


patient.samples.sample.portions.shipment_portion.2.center_id:
  20   27 NA's 
   9   18  142 

patient.samples.sample.portions.shipment_portion.2.is_ffpe:
  no NA's 
  27  142 

patient.samples.sample.portions.shipment_portion.2.plate_id:
2150 2214 a17n a20o NA's 
   8    1    8   10  142 

patient.samples.sample.portions.shipment_portion.2.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  12.00   22.00   31.00   28.67   31.00   71.00     142 

patient.samples.sample.portions.shipment_portion.2.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.250   3.000   3.056   3.000   7.000     151 


patient.samples.sample.portions.shipment_portion.2.shipment_portion_day_of_shipment:
  12   17   25 NA's 
   1    8   18  142 

patient.samples.sample.portions.shipment_portion.2.shipment_portion_month_of_shipment:
   1    3    8 NA's 
  18    1    8  142 

patient.samples.sample.portions.shipment_portion.2.shipment_portion_year_of_shipment:
2011 2012 NA's 
   8   19  142 

patient.samples.sample.portions.shipment_portion.3.bcr_shipment_portion_uuid:
cc314f5c-54d1-4822-99f4-cf4970e83605                                 NA's 
                                   1                                  168 

patient.samples.sample.portions.shipment_portion.3.center_id:
  27 NA's 
   1  168 

patient.samples.sample.portions.shipment_portion.3.is_ffpe:
  no NA's 
   1  168 

patient.samples.sample.portions.shipment_portion.3.plate_id:
a20o NA's 
   1  168 

patient.samples.sample.portions.shipment_portion.3.portion_number:
  31 NA's 
   1  168 

patient.samples.sample.portions.shipment_portion.3.portion_sequence:
   3 NA's 
   1  168 

patient.samples.sample.portions.shipment_portion.3.shipment_portion_bcr_aliquot_barcode:
tcga-ag-a00h-01a-31-a20o-27                        NA's 
                          1                         168 

patient.samples.sample.portions.shipment_portion.3.shipment_portion_day_of_shipment:
  25 NA's 
   1  168 

patient.samples.sample.portions.shipment_portion.3.shipment_portion_month_of_shipment:
   1 NA's 
   1  168 

patient.samples.sample.portions.shipment_portion.3.shipment_portion_year_of_shipment:
2012 NA's 
   1  168 


patient.samples.sample.portions.shipment_portion.center_id:
  20   27 NA's 
 123   12   34 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
 135   34 


patient.samples.sample.portions.shipment_portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   13.00   21.00   18.96   21.00   43.00      34 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   1    2    3    4 NA's 
   3   19    6    2  139 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
  16   17   25   30 NA's 
 103   19   12    1   34 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   1    7    8 NA's 
  12    1  122   34 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2011 2012 2014 NA's 
 122   12    1   34 

patient.samples.sample.sample_type:
primary tumor 
          169 

patient.samples.sample.sample_type_id:
  1 
169 

patient.samples.sample.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.100   0.300   0.400   0.439   0.500   0.900      33 

patient.samples.sample.vial_number:
  a   b   c 
166   2   1 

}}
\keyword{datasets}
