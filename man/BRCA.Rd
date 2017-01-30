\name{ BRCA }
\alias{ BRCA }
\docType{data}
\title{ BRCA }

\details{
\preformatted{
> BRCA
A MultiAssayExperiment object of 12 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 12: 
 [1] RNASeqGene: ExpressionSet with 20502 rows and 878 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 1212 columns 
 [3] miRNASeqGene: ExpressionSet with 1046 rows and 849 columns 
 [4] CNASNP: RangedRaggedAssay with 1132786 rows and 2209 columns 
 [5] CNVSNP: RangedRaggedAssay with 284458 rows and 2199 columns 
 [6] CNAseq: RangedRaggedAssay with 5298 rows and 38 columns 
 [7] Methylation: SummarizedExperiment with 485577 rows and 885 columns 
 [8] mRNAArray: ExpressionSet with 17814 rows and 590 columns 
 [9] RPPAArray: ExpressionSet with 142 rows and 410 columns 
 [10] Mutations: RangedRaggedAssay with 90490 rows and 993 columns 
 [11] gistica: SummarizedExperiment with 24776 rows and 1080 columns 
 [12] gistict: SummarizedExperiment with 24776 rows and 1080 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( BRCA )
CharacterList of length 12
[["RNASeqGene"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZYX ZZEF1 ZZZ3 psiTPTE22 tAKR
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1:61735-1510801 1:1627918-1672603 ... 24:28514121-59018259
[["CNVSNP"]] 1:3218610-63469503 1:63471492-63472103 ... 23:143612268-154905589
[["CNAseq"]] 1:10209-2583075 1:2583076-249240606 ... 24:2649450-28784074
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["mRNAArray"]] ELMO2 CREB3L1 RPS11 PNMA1 MMP2 ... SLC39A6 SNRPD2 AQP7 CTSC
[["RPPAArray"]] 14-3-3_epsilon 4E-BP1 ... p70S6K_pT389 p90RSK_pT359_S363
[["Mutations"]] ABLIM1 ADAMTS20 CADM2 DTNB ... ZNF649 ZNF768 ZNF792 ZNF813
...
<2 more elements>

> colnames( BRCA )
CharacterList of length 12
[["RNASeqGene"]] TCGA-A1-A0SB-01A-11R-A144-07 ... TCGA-GM-A2DK-01A-21R-A180-07
[["RNASeq2GeneNorm"]] TCGA-3C-AAAU-01A-11R-A41B-07 ...
[["miRNASeqGene"]] TCGA-3C-AAAU-01A-11R-A41G-13 ...
[["CNASNP"]] TCGA-3C-AAAU-01A-11D-A41E-01 ... TCGA-Z7-A8R6-10A-01D-A41E-01
[["CNVSNP"]] TCGA-3C-AAAU-01A-11D-A41E-01 ... TCGA-Z7-A8R6-10A-01D-A41E-01
[["CNAseq"]] TCGA-A2-A0EU-01A-22D-A060-02 ... TCGA-BH-A0W5-10A-01D-A106-02
[["Methylation"]] TCGA-3C-AAAU-01A-11D-A41Q-05 ... TCGA-Z7-A8R6-01A-11D-A41Q-05
[["mRNAArray"]] TCGA-A1-A0SD-01A-11R-A115-07 ... TCGA-E2-A1BD-01A-11R-A12P-07
[["RPPAArray"]] TCGA-A1-A0SH-01A-21-A13A-20 ... TCGA-E2-A1BD-01A-21-A13E-20
[["Mutations"]] TCGA-A1-A0SB-01A-11D-A142-09 ... TCGA-PE-A5DE-01A-11D-A27P-09
...
<2 more elements>

Sizes of each ExperimentList element:

             assay size.Mb
1       RNASeqGene  1.3 Mb
2  RNASeq2GeneNorm  1.3 Mb
3     miRNASeqGene  0.2 Mb
4           CNASNP 80.6 Mb
5           CNVSNP 25.3 Mb
6           CNAseq  0.4 Mb
7      Methylation 67.5 Mb
8        mRNAArray  1.1 Mb
9        RPPAArray  0.1 Mb
10       Mutations 64.2 Mb
11         gistica  4.4 Mb
12         gistict  4.4 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   947 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 151.00  151.00    3.48    2.83    4.50 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  26.00   49.00   59.00   58.58   68.00   90.00      15 

vital_status:
  0   1 
946 152 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  116.0   700.5  1272.0  1644.6  2367.0  7455.0     947 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -31.0   434.2   761.5  1186.5  1583.2  8605.0     104 

tumor_tissue_site:
breast 
  1098 




pathology_M_stage:
cm0 (i+)       m0       m1       mx 
       6      907       22      163 

gender:
female   male 
  1086     12 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1988    2007    2009    2008    2010    2013       2 

days_to_last_known_alive:
 735 2576 NA's 
   1    1 1096 

radiation_therapy:
  no  yes NA's 
 446  556   96 


number_of_lymph_nodes:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    1.00    2.36    2.00   35.00     168 

race:
american indian or alaska native                            asian 
                               1                               61 
       black or african american                            white 
                             183                              758 
                            NA's 
                              95 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    39                    885                    174 

admin.bcr:
nationwide children's hospital 
                          1098 

admin.day_of_dcc_upload:
   1   27 
1097    1 

admin.disease_code:
brca 
1098 


admin.month_of_dcc_upload:
   2   10 
   1 1097 

admin.patient_withdrawal.withdrawn:
false 
 1098 

admin.project_code:
tcga NA's 
1097    1 

admin.year_of_dcc_upload:
2014 2015 
   1 1097 

patient.additional_studies.additional_study.disease_code:
fppp NA's 
   5 1093 

patient.additional_studies.additional_study.project_code:
tcga NA's 
   5 1093 

patient.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  26.00   49.00   58.00   58.45   67.00   90.00 





patient.anatomic_neoplasm_subdivisions.anatomic_neoplasm_subdivision.5:
                      left  left lower outer quadrant 
                         4                          1 
                     right right lower outer quadrant 
                         5                          1 
                      NA's 
                      1087 




patient.breast_cancer_surgery_margin_status:
   close negative positive     NA's 
       1       56        2     1039 

patient.breast_carcinoma_estrogen_receptor_status:
indeterminate      negative      positive          NA's 
            2           238           809            49 

patient.breast_carcinoma_immunohistochemistry_er_pos_finding_scale:
3 point scale 4 point scale          NA's 
          130            17           951 


patient.breast_carcinoma_immunohistochemistry_progesterone_receptor_pos_finding_scale:
3 point scale 4 point scale          NA's 
          126            16           956 

patient.breast_carcinoma_primary_surgical_procedure_name:
                 lumpectomy              mastectomy nos 
                          9                          18 
modified radical mastectomy                       other 
                         14                          14 
                       NA's 
                       1043 

patient.breast_carcinoma_progesterone_receptor_status:
indeterminate      negative      positive          NA's 
            4           344           700            50 

patient.breast_carcinoma_surgical_procedure_name:
                 lumpectomy modified radical mastectomy 
                        247                         322 
                      other           simple mastectomy 
                        273                         200 
                       NA's 
                         56 


patient.clinical_cqcf.consent_or_death_status:
consented  deceased    waiver 
     1056        41         1 


patient.clinical_cqcf.days_to_consent:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
-1452.00     0.00    15.00    84.68    34.00  6029.00       44 

patient.clinical_cqcf.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    224    1034    1673    1799    2273    3959    1057 


patient.clinical_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                            1094 
yes, pharmaceutical treatment prior to resection 
                                               3 
                                            NA's 
                                               1 

patient.clinical_cqcf.normal_tissue_anatomic_site:
breast  other   NA's 
   282     27    789 

patient.clinical_cqcf.normal_tissue_proximity:
distal (>2cm)          NA's 
          284           814 


patient.clinical_cqcf.tumor_type:
primary 
   1098 

patient.cytokeratin_immunohistochemistry_staining_method_micrometastasis_indicator:
  no  yes NA's 
 467  263  368 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    9.00   15.00   16.31   26.00   31.00 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -32873  -24862  -21559  -21582  -18023   -9706      15 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  116.0   752.8  1409.0  1617.3  2384.0  4456.0     994 

patient.days_to_initial_pathologic_diagnosis:
   0 
1098 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -7.0   109.0   361.5   730.7  1052.8  7067.0     104 

patient.distant_metastasis_present_ind2:
  no  yes NA's 
 382   12  704 

patient.drugs.drug.10.bcr_drug_barcode:
tcga-a2-a0ew-d59521 tcga-ao-a03n-d20931 tcga-e2-a107-d17174 tcga-gm-a2da-d47223 
                  1                   1                   1                   1 
               NA's 
               1094 

patient.drugs.drug.10.bcr_drug_uuid:
01259757-dd3d-4df9-9686-9cb07ae8a150 1b00197c-7f84-4429-80e9-471f087d8753 
                                   1                                    1 
46e34570-da26-49e6-9564-cd69c9a9ece6 b90f3a89-5cdc-406e-b9b8-c850c6c5dd76 
                                   1                                    1 
                                NA's 
                                1094 

patient.drugs.drug.10.day_of_form_completion:
  14   23   24   28 NA's 
   1    1    1    1 1094 

patient.drugs.drug.10.days_to_drug_therapy_end:
 151 1718 4435 NA's 
   1    1    1 1095 

patient.drugs.drug.10.days_to_drug_therapy_start:
 109 1425 1857 4347 NA's 
   1    1    1    1 1094 

patient.drugs.drug.10.drug_name:
cyclophosphamide        denosumab           megace      vinorelbine 
               1                1                1                1 
            NA's 
            1094 

patient.drugs.drug.10.measure_of_response:
clinical progressive disease                         NA's 
                           2                         1096 

patient.drugs.drug.10.month_of_form_completion:
   1    5    8    9 NA's 
   1    1    1    1 1094 

patient.drugs.drug.10.number_cycles:
   4   16 NA's 
   1    1 1096 

patient.drugs.drug.10.prescribed_dose:
  25 1050 NA's 
   1    1 1096 

patient.drugs.drug.10.prescribed_dose_units:
   mg mg/m2  NA's 
    1     1  1096 

patient.drugs.drug.10.regimen_indication:
  adjuvant recurrence       NA's 
         1          1       1096 

patient.drugs.drug.10.regimen_number:
   1   12 NA's 
   1    1 1096 

patient.drugs.drug.10.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               2             1096 

patient.drugs.drug.10.therapy_ongoing:
  no  yes NA's 
   3    1 1094 

patient.drugs.drug.10.therapy_types.therapy_type:
      ancillary    chemotherapy hormone therapy            NA's 
              1               2               1            1094 

patient.drugs.drug.10.total_dose:
 704 4200 NA's 
   1    1 1096 

patient.drugs.drug.10.total_dose_units:
  mg NA's 
   2 1096 

patient.drugs.drug.10.tx_on_clinical_trial:
  no NA's 
   2 1096 

patient.drugs.drug.10.year_of_form_completion:
2011 2012 2013 2014 NA's 
   1    1    1    1 1094 

patient.drugs.drug.11.bcr_drug_barcode:
tcga-a2-a0ew-d59522 tcga-ao-a03n-d20927 tcga-e2-a107-d17823 tcga-gm-a2da-d47224 
                  1                   1                   1                   1 
               NA's 
               1094 

patient.drugs.drug.11.bcr_drug_uuid:
66bf0675-e5ac-453d-b319-3e0dce3c4c2a b6bdb863-8723-4e39-981c-050bd35d2153 
                                   1                                    1 
c89e1f88-a478-4697-9540-2ac63a295747 dc919671-3b3f-4a46-b89c-1b4bb7f356d1 
                                   1                                    1 
                                NA's 
                                1094 

patient.drugs.drug.11.day_of_form_completion:
  14   23   24   28 NA's 
   1    1    1    1 1094 

patient.drugs.drug.11.days_to_drug_therapy_end:
 333 1722 1762 4494 NA's 
   1    1    1    1 1094 

patient.drugs.drug.11.days_to_drug_therapy_start:
 249 1618 1699 4470 NA's 
   1    1    1    1 1094 

patient.drugs.drug.11.drug_name:
bevacizumab   tamoxifen   tesetaxel      xeloda        NA's 
          1           1           1           1        1094 

patient.drugs.drug.11.measure_of_response:
clinical progressive disease                         NA's 
                           2                         1096 

patient.drugs.drug.11.month_of_form_completion:
   1    5    8    9 NA's 
   1    1    1    1 1094 

patient.drugs.drug.11.number_cycles:
   4 NA's 
   2 1096 

patient.drugs.drug.11.prescribed_dose:
  27 1049 NA's 
   1    1 1096 

patient.drugs.drug.11.prescribed_dose_units:
   mg mg/m2  NA's 
    1     1  1096 

patient.drugs.drug.11.regimen_indication:
  adjuvant recurrence       NA's 
         1          1       1096 

patient.drugs.drug.11.regimen_number:
   3    9 NA's 
   1    1 1096 

patient.drugs.drug.11.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               1                1             1096 

patient.drugs.drug.11.therapy_ongoing:
  no NA's 
   4 1094 

patient.drugs.drug.11.therapy_types.therapy_type:
   chemotherapy hormone therapy            NA's 
              3               1            1094 

patient.drugs.drug.11.total_dose:
 230 4196 NA's 
   1    1 1096 

patient.drugs.drug.11.total_dose_units:
  mg NA's 
   2 1096 

patient.drugs.drug.11.tx_on_clinical_trial:
  no  yes NA's 
   1    1 1096 

patient.drugs.drug.11.year_of_form_completion:
2011 2012 2013 2014 NA's 
   1    1    1    1 1094 

patient.drugs.drug.12.bcr_drug_barcode:
tcga-a2-a0ew-d59523 tcga-ao-a03n-d20923 tcga-gm-a2da-d47225                NA's 
                  1                   1                   1                1095 

patient.drugs.drug.12.bcr_drug_uuid:
105c9e04-6f4f-4bc9-98d4-c5e70d9bc254 9faae45e-9014-4cd8-bbba-f327c60e65d7 
                                   1                                    1 
d8b2fed7-8269-47d8-90dc-e1a16e08e05f                                 NA's 
                                   1                                 1095 

patient.drugs.drug.12.day_of_form_completion:
  14   23   24 NA's 
   1    1    1 1095 

patient.drugs.drug.12.days_to_drug_therapy_end:
1667 1787 4494 NA's 
   1    1    1 1095 

patient.drugs.drug.12.days_to_drug_therapy_start:
1639 1696 4470 NA's 
   1    1    1 1095 

patient.drugs.drug.12.drug_name:
doxorubicin liposome          fulvestrant             yondelis 
                   1                    1                    1 
                NA's 
                1095 

patient.drugs.drug.12.measure_of_response:
clinical progressive disease                         NA's 
                           2                         1096 

patient.drugs.drug.12.month_of_form_completion:
   1    5    8 NA's 
   1    1    1 1095 

patient.drugs.drug.12.number_cycles:
   3 NA's 
   1 1097 

patient.drugs.drug.12.prescribed_dose:
250/5  NA's 
    1  1097 

patient.drugs.drug.12.prescribed_dose_units:
mg/ml  NA's 
    1  1097 

patient.drugs.drug.12.regimen_indication:
recurrence       NA's 
         1       1097 

patient.drugs.drug.12.regimen_number:
   8 NA's 
   1 1097 

patient.drugs.drug.12.route_of_administrations.route_of_administration:
intramuscular (im)               NA's 
                 1               1097 

patient.drugs.drug.12.therapy_ongoing:
  no NA's 
   3 1095 

patient.drugs.drug.12.therapy_types.therapy_type:
   chemotherapy hormone therapy            NA's 
              2               1            1095 

patient.drugs.drug.12.total_dose:
1500 NA's 
   1 1097 

patient.drugs.drug.12.total_dose_units:
  mg NA's 
   1 1097 

patient.drugs.drug.12.tx_on_clinical_trial:
  no  yes NA's 
   1    1 1096 

patient.drugs.drug.12.year_of_form_completion:
2012 2013 2014 NA's 
   1    1    1 1095 

patient.drugs.drug.13.bcr_drug_barcode:
tcga-ao-a03n-d20930 tcga-gm-a2da-d47228                NA's 
                  1                   1                1096 

patient.drugs.drug.13.bcr_drug_uuid:
a7b8633b-19cd-4008-887d-77be720af867 c4226dfd-8e7a-4898-acd6-2e907f30dd99 
                                   1                                    1 
                                NA's 
                                1096 

patient.drugs.drug.13.day_of_form_completion:
  23   24 NA's 
   1    1 1096 

patient.drugs.drug.13.days_to_drug_therapy_end:
1842 4720 NA's 
   1    1 1096 

patient.drugs.drug.13.days_to_drug_therapy_start:
1835 4515 NA's 
   1    1 1096 

patient.drugs.drug.13.drug_name:
gemcitabine      xeloda        NA's 
          1           1        1096 

patient.drugs.drug.13.measure_of_response:
stable disease           NA's 
             1           1097 

patient.drugs.drug.13.month_of_form_completion:
   1    8 NA's 
   1    1 1096 

patient.drugs.drug.13.number_cycles:
   2 NA's 
   1 1097 

patient.drugs.drug.13.prescribed_dose:
 800 NA's 
   1 1097 

patient.drugs.drug.13.prescribed_dose_units:
mg/m2  NA's 
    1  1097 

patient.drugs.drug.13.regimen_indication:
recurrence       NA's 
         1       1097 

patient.drugs.drug.13.regimen_number:
  11 NA's 
   1 1097 

patient.drugs.drug.13.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1             1097 

patient.drugs.drug.13.therapy_ongoing:
  no NA's 
   2 1096 

patient.drugs.drug.13.therapy_types.therapy_type:
chemotherapy         NA's 
           2         1096 

patient.drugs.drug.13.total_dose:
2800 NA's 
   1 1097 

patient.drugs.drug.13.total_dose_units:
  mg NA's 
   1 1097 

patient.drugs.drug.13.tx_on_clinical_trial:
  no NA's 
   1 1097 

patient.drugs.drug.13.year_of_form_completion:
2012 2013 NA's 
   1    1 1096 

patient.drugs.drug.14.bcr_drug_barcode:
tcga-gm-a2da-d47229                NA's 
                  1                1097 

patient.drugs.drug.14.bcr_drug_uuid:
e41e7973-fe0f-4d97-a8db-69bfe8b0d08c                                 NA's 
                                   1                                 1097 

patient.drugs.drug.14.day_of_form_completion:
  23 NA's 
   1 1097 

patient.drugs.drug.14.days_to_drug_therapy_end:
5015 NA's 
   1 1097 

patient.drugs.drug.14.days_to_drug_therapy_start:
4721 NA's 
   1 1097 

patient.drugs.drug.14.drug_name:
aromasin     NA's 
       1     1097 

patient.drugs.drug.14.measure_of_response:
stable disease           NA's 
             1           1097 

patient.drugs.drug.14.month_of_form_completion:
   8 NA's 
   1 1097 

patient.drugs.drug.14.therapy_ongoing:
  no NA's 
   1 1097 

patient.drugs.drug.14.therapy_types.therapy_type:
hormone therapy            NA's 
              1            1097 

patient.drugs.drug.14.tx_on_clinical_trial:
  no NA's 
   1 1097 

patient.drugs.drug.14.year_of_form_completion:
2013 NA's 
   1 1097 

patient.drugs.drug.15.bcr_drug_barcode:
tcga-gm-a2da-d47230                NA's 
                  1                1097 

patient.drugs.drug.15.bcr_drug_uuid:
f503b87e-a4f9-4fa8-9508-93b7658c1fda                                 NA's 
                                   1                                 1097 

patient.drugs.drug.15.day_of_form_completion:
  23 NA's 
   1 1097 

patient.drugs.drug.15.days_to_drug_therapy_end:
5455 NA's 
   1 1097 

patient.drugs.drug.15.days_to_drug_therapy_start:
5021 NA's 
   1 1097 

patient.drugs.drug.15.drug_name:
xeloda   NA's 
     1   1097 

patient.drugs.drug.15.measure_of_response:
stable disease           NA's 
             1           1097 

patient.drugs.drug.15.month_of_form_completion:
   8 NA's 
   1 1097 

patient.drugs.drug.15.therapy_ongoing:
  no NA's 
   1 1097 

patient.drugs.drug.15.therapy_types.therapy_type:
chemotherapy         NA's 
           1         1097 

patient.drugs.drug.15.tx_on_clinical_trial:
  no NA's 
   1 1097 

patient.drugs.drug.15.year_of_form_completion:
2013 NA's 
   1 1097 

patient.drugs.drug.16.bcr_drug_barcode:
tcga-gm-a2da-d47231                NA's 
                  1                1097 

patient.drugs.drug.16.bcr_drug_uuid:
b9f6747c-a8e8-4dff-b2ff-6378094f03e1                                 NA's 
                                   1                                 1097 

patient.drugs.drug.16.day_of_form_completion:
  23 NA's 
   1 1097 

patient.drugs.drug.16.days_to_drug_therapy_end:
5678 NA's 
   1 1097 

patient.drugs.drug.16.days_to_drug_therapy_start:
5455 NA's 
   1 1097 

patient.drugs.drug.16.drug_name:
abraxane     NA's 
       1     1097 

patient.drugs.drug.16.measure_of_response:
stable disease           NA's 
             1           1097 

patient.drugs.drug.16.month_of_form_completion:
   8 NA's 
   1 1097 

patient.drugs.drug.16.therapy_ongoing:
  no NA's 
   1 1097 

patient.drugs.drug.16.therapy_types.therapy_type:
chemotherapy         NA's 
           1         1097 

patient.drugs.drug.16.tx_on_clinical_trial:
  no NA's 
   1 1097 

patient.drugs.drug.16.year_of_form_completion:
2013 NA's 
   1 1097 

patient.drugs.drug.17.bcr_drug_barcode:
tcga-gm-a2da-d47232                NA's 
                  1                1097 

patient.drugs.drug.17.bcr_drug_uuid:
3411631a-3c34-4a06-a5e7-db6f0e49ca25                                 NA's 
                                   1                                 1097 

patient.drugs.drug.17.day_of_form_completion:
  23 NA's 
   1 1097 

patient.drugs.drug.17.days_to_drug_therapy_end:
5797 NA's 
   1 1097 

patient.drugs.drug.17.days_to_drug_therapy_start:
5684 NA's 
   1 1097 

patient.drugs.drug.17.drug_name:
ixabepilone        NA's 
          1        1097 

patient.drugs.drug.17.measure_of_response:
clinical progressive disease                         NA's 
                           1                         1097 

patient.drugs.drug.17.month_of_form_completion:
   8 NA's 
   1 1097 

patient.drugs.drug.17.therapy_ongoing:
  no NA's 
   1 1097 

patient.drugs.drug.17.therapy_types.therapy_type:
chemotherapy         NA's 
           1         1097 

patient.drugs.drug.17.tx_on_clinical_trial:
  no NA's 
   1 1097 

patient.drugs.drug.17.year_of_form_completion:
2013 NA's 
   1 1097 

patient.drugs.drug.18.bcr_drug_barcode:
tcga-gm-a2da-d47234                NA's 
                  1                1097 

patient.drugs.drug.18.bcr_drug_uuid:
bae1a1f1-6edc-4e7e-ba5e-632c9f3e6aa9                                 NA's 
                                   1                                 1097 

patient.drugs.drug.18.day_of_form_completion:
  23 NA's 
   1 1097 

patient.drugs.drug.18.days_to_drug_therapy_end:
6028 NA's 
   1 1097 

patient.drugs.drug.18.days_to_drug_therapy_start:
5797 NA's 
   1 1097 

patient.drugs.drug.18.drug_name:
herceptin      NA's 
        1      1097 

patient.drugs.drug.18.measure_of_response:
stable disease           NA's 
             1           1097 

patient.drugs.drug.18.month_of_form_completion:
   8 NA's 
   1 1097 

patient.drugs.drug.18.therapy_ongoing:
  no NA's 
   1 1097 

patient.drugs.drug.18.therapy_types.therapy_type:
immunotherapy          NA's 
            1          1097 

patient.drugs.drug.18.tx_on_clinical_trial:
  no NA's 
   1 1097 

patient.drugs.drug.18.year_of_form_completion:
2013 NA's 
   1 1097 

patient.drugs.drug.19.bcr_drug_barcode:
tcga-gm-a2da-d47236                NA's 
                  1                1097 

patient.drugs.drug.19.bcr_drug_uuid:
28b70e98-675a-4649-a171-1ccc398cfb9f                                 NA's 
                                   1                                 1097 

patient.drugs.drug.19.day_of_form_completion:
  23 NA's 
   1 1097 

patient.drugs.drug.19.days_to_drug_therapy_end:
6029 NA's 
   1 1097 

patient.drugs.drug.19.days_to_drug_therapy_start:
5909 NA's 
   1 1097 

patient.drugs.drug.19.drug_name:
tamoxifen      NA's 
        1      1097 

patient.drugs.drug.19.measure_of_response:
clinical progressive disease                         NA's 
                           1                         1097 

patient.drugs.drug.19.month_of_form_completion:
   8 NA's 
   1 1097 

patient.drugs.drug.19.therapy_ongoing:
  no NA's 
   1 1097 

patient.drugs.drug.19.therapy_types.therapy_type:
hormone therapy            NA's 
              1            1097 

patient.drugs.drug.19.tx_on_clinical_trial:
  no NA's 
   1 1097 

patient.drugs.drug.19.year_of_form_completion:
2013 NA's 
   1 1097 



patient.drugs.drug.2.clinical_trail_drug_classification:
 n/a NA's 
  23 1075 

patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   15.00   14.81   21.00   31.00     521 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-1386.0   113.5   154.0   236.7   205.0  3094.0     611 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-1428.0    50.0    74.5   137.6   130.2  2422.0     554 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease            complete response 
                           5                          162 
            partial response               stable disease 
                           4                            7 
                        NA's 
                         920 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.000   6.768  11.000  12.000     521 

patient.drugs.drug.2.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.00    4.00    5.63    5.00  174.00     871 






patient.drugs.drug.2.route_of_administrations.route_of_administration:
intramuscular (im)   intravenous (iv)               oral  subcutaneous (sc) 
                 5                218                 57                  3 
              NA's 
               815 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
 514   56  528 


patient.drugs.drug.2.therapy_types.therapy_type_notes:
             ancillary         bisphosphonate bisphosphonate therapy 
                     1                      2                      1 
                  NA's 
                  1094 



patient.drugs.drug.2.tx_on_clinical_trial:
  no  yes NA's 
 250   20  828 

patient.drugs.drug.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2015     521 

patient.drugs.drug.20.bcr_drug_barcode:
tcga-gm-a2da-d47237                NA's 
                  1                1097 

patient.drugs.drug.20.bcr_drug_uuid:
d2f15985-953a-4b07-b205-27967c46571d                                 NA's 
                                   1                                 1097 

patient.drugs.drug.20.day_of_form_completion:
  23 NA's 
   1 1097 

patient.drugs.drug.20.days_to_drug_therapy_end:
6213 NA's 
   1 1097 

patient.drugs.drug.20.days_to_drug_therapy_start:
6051 NA's 
   1 1097 

patient.drugs.drug.20.drug_name:
xeloda   NA's 
     1   1097 

patient.drugs.drug.20.measure_of_response:
stable disease           NA's 
             1           1097 

patient.drugs.drug.20.month_of_form_completion:
   8 NA's 
   1 1097 

patient.drugs.drug.20.therapy_ongoing:
  no NA's 
   1 1097 

patient.drugs.drug.20.therapy_types.therapy_type:
chemotherapy         NA's 
           1         1097 

patient.drugs.drug.20.tx_on_clinical_trial:
  no NA's 
   1 1097 

patient.drugs.drug.20.year_of_form_completion:
2013 NA's 
   1 1097 

patient.drugs.drug.21.bcr_drug_barcode:
tcga-gm-a2da-d47238                NA's 
                  1                1097 

patient.drugs.drug.21.bcr_drug_uuid:
89719fba-e180-4f48-ae3b-75cbfb127146                                 NA's 
                                   1                                 1097 

patient.drugs.drug.21.day_of_form_completion:
  23 NA's 
   1 1097 

patient.drugs.drug.21.days_to_drug_therapy_end:
6536 NA's 
   1 1097 

patient.drugs.drug.21.days_to_drug_therapy_start:
6051 NA's 
   1 1097 

patient.drugs.drug.21.drug_name:
lapatinib      NA's 
        1      1097 

patient.drugs.drug.21.measure_of_response:
stable disease           NA's 
             1           1097 

patient.drugs.drug.21.month_of_form_completion:
   8 NA's 
   1 1097 

patient.drugs.drug.21.therapy_ongoing:
  no NA's 
   1 1097 

patient.drugs.drug.21.therapy_types.therapy_type:
chemotherapy         NA's 
           1         1097 

patient.drugs.drug.21.tx_on_clinical_trial:
  no NA's 
   1 1097 

patient.drugs.drug.21.year_of_form_completion:
2013 NA's 
   1 1097 

patient.drugs.drug.22.bcr_drug_barcode:
tcga-gm-a2da-d47240                NA's 
                  1                1097 

patient.drugs.drug.22.bcr_drug_uuid:
4cdd3f8e-eff0-49c7-972e-40ae2cab2b7a                                 NA's 
                                   1                                 1097 

patient.drugs.drug.22.day_of_form_completion:
  23 NA's 
   1 1097 

patient.drugs.drug.22.days_to_drug_therapy_end:
6422 NA's 
   1 1097 

patient.drugs.drug.22.days_to_drug_therapy_start:
6372 NA's 
   1 1097 

patient.drugs.drug.22.drug_name:
navelbine      NA's 
        1      1097 

patient.drugs.drug.22.measure_of_response:
clinical progressive disease                         NA's 
                           1                         1097 

patient.drugs.drug.22.month_of_form_completion:
   8 NA's 
   1 1097 

patient.drugs.drug.22.therapy_ongoing:
  no NA's 
   1 1097 

patient.drugs.drug.22.therapy_types.therapy_type:
chemotherapy         NA's 
           1         1097 

patient.drugs.drug.22.tx_on_clinical_trial:
  no NA's 
   1 1097 

patient.drugs.drug.22.year_of_form_completion:
2013 NA's 
   1 1097 

patient.drugs.drug.23.bcr_drug_barcode:
tcga-gm-a2da-d47241                NA's 
                  1                1097 

patient.drugs.drug.23.bcr_drug_uuid:
eebdc3b3-bcf1-4629-9c08-8129305599b7                                 NA's 
                                   1                                 1097 

patient.drugs.drug.23.day_of_form_completion:
  23 NA's 
   1 1097 

patient.drugs.drug.23.days_to_drug_therapy_end:
6536 NA's 
   1 1097 

patient.drugs.drug.23.days_to_drug_therapy_start:
6372 NA's 
   1 1097 

patient.drugs.drug.23.drug_name:
herceptin      NA's 
        1      1097 

patient.drugs.drug.23.measure_of_response:
stable disease           NA's 
             1           1097 

patient.drugs.drug.23.month_of_form_completion:
   8 NA's 
   1 1097 

patient.drugs.drug.23.therapy_ongoing:
  no NA's 
   1 1097 

patient.drugs.drug.23.therapy_types.therapy_type:
immunotherapy          NA's 
            1          1097 

patient.drugs.drug.23.tx_on_clinical_trial:
  no NA's 
   1 1097 

patient.drugs.drug.23.year_of_form_completion:
2013 NA's 
   1 1097 



patient.drugs.drug.3.clinical_trail_drug_classification:
biological therapy/monoclonal antibody                                    n/a 
                                     1                                     21 
                                 taxol                                   NA's 
                                     1                                   1075 

patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   14.00   14.72   21.00   31.00     633 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -45.0   128.0   168.0   229.5   221.0  2331.0     711 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -49.00   59.75   98.00  144.28  164.25 1918.00     658 


patient.drugs.drug.3.measure_of_response:
clinical progressive disease            complete response 
                           4                          122 
            partial response               stable disease 
                           2                            4 
                        NA's 
                         966 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   8.000   6.944  11.000  12.000     633 

patient.drugs.drug.3.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.00    4.00    5.91    5.00  208.00     908 




patient.drugs.drug.3.regimen_indication_notes:
cancer vaccine trial                 NA's 
                   1                 1097 


patient.drugs.drug.3.route_of_administrations.route_of_administration:
 intravenous (iv)              oral subcutaneous (sc)              NA's 
              188                57                 2               851 

patient.drugs.drug.3.therapy_ongoing:
  no  yes NA's 
 400   63  635 


patient.drugs.drug.3.therapy_types.therapy_type_notes:
aromatase inhibitor      bisphosphonate                NA's 
                  1                   2                1095 

patient.drugs.drug.3.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1     478    1381    2436    4143   10000     934 

patient.drugs.drug.3.total_dose_units:
    mg mg/day  mg/m2     ug   NA's 
   165      7      2      1    923 

patient.drugs.drug.3.tx_on_clinical_trial:
  no  yes NA's 
 188   18  892 

patient.drugs.drug.3.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2015     633 



patient.drugs.drug.4.clinical_trail_drug_classification:
 n/a NA's 
  19 1079 

patient.drugs.drug.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   15.00   15.15   22.00   31.00     794 

patient.drugs.drug.4.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   45.0   143.0   189.0   333.1   280.5  2422.0     886 

patient.drugs.drug.4.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -1.0    78.5   142.0   213.3   231.0  2332.0     806 


patient.drugs.drug.4.measure_of_response:
clinical progressive disease            complete response 
                           6                           53 
            partial response               stable disease 
                           1                            5 
                        NA's 
                        1033 

patient.drugs.drug.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   8.000   7.023  11.000  12.000     794 





patient.drugs.drug.4.regimen_indication_notes:
cancer vaccine trial         neo-adjuvant                 NA's 
                   1                    1                 1096 


patient.drugs.drug.4.route_of_administrations.route_of_administration:
 intravenous (iv)              oral subcutaneous (sc)              NA's 
              118                45                 1               934 

patient.drugs.drug.4.therapy_ongoing:
  no  yes NA's 
 225   78  795 


patient.drugs.drug.4.therapy_types.therapy_type_notes:
                                      bisphosphonate 
                                                   3 
clinical trial - bisphosphonates as adjuvant therapy 
                                                   2 
                            phase iii clinical trial 
                                                   2 
                                                NA's 
                                                1091 

patient.drugs.drug.4.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1     457    1372    3110    3906   75200     990 

patient.drugs.drug.4.total_dose_units:
    mg mg/day  mg/wk     ug   NA's 
   108      6      1      1    982 

patient.drugs.drug.4.tx_on_clinical_trial:
  no  yes NA's 
 119   15  964 

patient.drugs.drug.4.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2015     794 



patient.drugs.drug.5.clinical_trail_drug_classification:
 n/a NA's 
  12 1086 

patient.drugs.drug.5.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   13.00   13.97   20.00   31.00     961 

patient.drugs.drug.5.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   42.0   155.0   254.0   508.4   608.0  2881.0    1007 

patient.drugs.drug.5.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   109.0   203.0   364.9   335.0  2853.0     969 


patient.drugs.drug.5.measure_of_response:
clinical progressive disease            complete response 
                           6                           16 
            partial response                         NA's 
                           1                         1075 

patient.drugs.drug.5.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   8.000   7.146  11.000  12.000     961 

patient.drugs.drug.5.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    4.00   10.52    4.75  245.00    1048 



patient.drugs.drug.5.regimen_indication:
               adjuvant other: specify in notes             progression 
                     70                       3                       3 
             recurrence                    NA's 
                      1                    1021 

patient.drugs.drug.5.regimen_indication_notes:
                                    cancer vaccine trial 
                                                       2 
estrogen receptor antagonist in metastatic breast cancer 
                                                       1 
                                                    NA's 
                                                    1095 


patient.drugs.drug.5.route_of_administrations.route_of_administration:
intramuscular (im)   intravenous (iv)               oral               NA's 
                 1                 44                 25               1028 

patient.drugs.drug.5.therapy_ongoing:
  no  yes NA's 
  98   39  961 


patient.drugs.drug.5.therapy_types.therapy_type_notes:
         biphosphonate bisphosphonate therapy                   NA's 
                     1                      1                   1096 

patient.drugs.drug.5.total_dose:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
     1.0    243.8   1500.0  15303.3   4332.0 571500.0     1054 

patient.drugs.drug.5.total_dose_units:
    mg mg/day  mg/wk     ug   NA's 
    41      2      1      2   1052 

patient.drugs.drug.5.tx_on_clinical_trial:
  no  yes NA's 
  52    8 1038 

patient.drugs.drug.5.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2015     961 



patient.drugs.drug.6.clinical_trail_drug_classification:
 n/a NA's 
   5 1093 

patient.drugs.drug.6.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   15.00   15.68   22.25   31.00    1030 

patient.drugs.drug.6.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   22.0   181.2   432.5   605.2   793.2  3131.0    1052 

patient.drugs.drug.6.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   22.0   137.0   287.0   454.6   497.2  3061.0    1032 


patient.drugs.drug.6.measure_of_response:
clinical progressive disease            complete response 
                           7                            7 
              stable disease                         NA's 
                           2                         1082 

patient.drugs.drug.6.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   8.000   6.941   9.000  12.000    1030 

patient.drugs.drug.6.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.500   4.286   4.000  22.000    1084 



patient.drugs.drug.6.regimen_indication:
   adjuvant progression        NA's 
         32           1        1065 

patient.drugs.drug.6.regimen_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.067   4.000   7.000    1068 

patient.drugs.drug.6.route_of_administrations.route_of_administration:
intramuscular (im)   intravenous (iv)               oral               NA's 
                 3                 16                 14               1065 

patient.drugs.drug.6.therapy_ongoing:
  no  yes NA's 
  49   19 1030 


patient.drugs.drug.6.therapy_types.therapy_type_notes:
phase iii clinical trial                     NA's 
                       1                     1097 

patient.drugs.drug.6.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0   386.2  2886.0  3037.8  4186.0 10037.0    1080 

patient.drugs.drug.6.total_dose_units:
    mg mg/day   NA's 
    18      2   1078 

patient.drugs.drug.6.tx_on_clinical_trial:
  no  yes NA's 
  30    5 1063 

patient.drugs.drug.6.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2015    1030 



patient.drugs.drug.7.clinical_trail_drug_classification:
antimetabolite            n/a           NA's 
             1              1           1096 

patient.drugs.drug.7.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.50   13.00   14.22   19.00   28.00    1071 

patient.drugs.drug.7.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   79.0   164.2   400.0   659.0   746.8  3131.0    1074 

patient.drugs.drug.7.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   29.0    78.0   208.0   562.4   577.0  3061.0    1071 


patient.drugs.drug.7.measure_of_response:
clinical progressive disease            complete response 
                           5                            2 
              stable disease                         NA's 
                           1                         1090 

patient.drugs.drug.7.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.500   8.000   6.815   9.000  12.000    1071 

patient.drugs.drug.7.number_cycles:
   1    3    4   15 NA's 
   2    1    6    1 1088 

patient.drugs.drug.7.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    30.0   136.0   286.6   600.0  1000.0    1084 

patient.drugs.drug.7.prescribed_dose_units:
       mg    mg/day     mg/m2 mg/m2/day      NA's 
        7         2         4         1      1084 

patient.drugs.drug.7.regimen_indication:
   adjuvant  palliative progression        NA's 
         14           1           1        1082 

patient.drugs.drug.7.regimen_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.786   4.750   6.000    1084 

patient.drugs.drug.7.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
              10                6             1082 

patient.drugs.drug.7.therapy_ongoing:
  no  yes NA's 
  24    3 1071 

patient.drugs.drug.7.therapy_types.therapy_type:
      ancillary    chemotherapy hormone therapy   immunotherapy            NA's 
              1              14              11               1            1071 

patient.drugs.drug.7.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  100.0   243.8  2092.0  2457.0  4675.0  5560.0    1088 

patient.drugs.drug.7.total_dose_units:
    mg mg/day   NA's 
     9      1   1088 

patient.drugs.drug.7.tx_on_clinical_trial:
  no  yes NA's 
   9    2 1087 

patient.drugs.drug.7.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2015    1071 



patient.drugs.drug.8.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   14.00   16.00   17.33   23.00   28.00    1083 

patient.drugs.drug.8.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  119.0   188.8   322.0   786.5  1032.5  3415.0    1086 

patient.drugs.drug.8.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   32.0   111.5   239.0   803.0  1099.0  3141.0    1083 


patient.drugs.drug.8.measure_of_response:
clinical progressive disease               stable disease 
                           3                            1 
                        NA's 
                        1094 

patient.drugs.drug.8.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     4.5     5.0     6.2     8.5    12.0    1083 

patient.drugs.drug.8.number_cycles:
   2    3    4    6 NA's 
   2    2    2    1 1091 

patient.drugs.drug.8.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    60.0   100.0   109.8   120.0   367.0    1089 

patient.drugs.drug.8.prescribed_dose_units:
    mg mg/day  mg/m2   NA's 
     6      1      2   1089 

patient.drugs.drug.8.regimen_indication:
   adjuvant progression        NA's 
          7           2        1089 

patient.drugs.drug.8.regimen_number:
   1    2    8   13 NA's 
   5    1    1    1 1090 

patient.drugs.drug.8.route_of_administrations.route_of_administration:
 intravenous (iv)              oral subcutaneous (sc)              NA's 
                6                 2                 1              1089 

patient.drugs.drug.8.therapy_ongoing:
  no  yes NA's 
  13    2 1083 


patient.drugs.drug.8.therapy_types.therapy_type_notes:
bone metastases            NA's 
              1            1097 

patient.drugs.drug.8.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  200.0   330.0   520.0   905.3  1377.5  2202.0    1091 

patient.drugs.drug.8.total_dose_units:
  mg NA's 
   7 1091 

patient.drugs.drug.8.tx_on_clinical_trial:
  no NA's 
   6 1092 

patient.drugs.drug.8.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2015    1083 



patient.drugs.drug.9.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   12.0    14.0    16.0    18.3    23.0    28.0    1088 

patient.drugs.drug.9.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   93.0   231.5   861.5  1244.4  1628.5  4307.0    1090 

patient.drugs.drug.9.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   32.0   189.2   725.0  1080.3  1702.0  3434.0    1088 


patient.drugs.drug.9.measure_of_response:
clinical progressive disease                         NA's 
                           3                         1095 

patient.drugs.drug.9.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.25    5.00    6.10    8.75   12.00    1088 

patient.drugs.drug.9.number_cycles:
   2   12 NA's 
   2    1 1095 

patient.drugs.drug.9.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1       4      70      59      80     140    1093 

patient.drugs.drug.9.prescribed_dose_units:
   mg mg/kg mg/m2  NA's 
    3     1     1  1093 

patient.drugs.drug.9.regimen_indication:
  adjuvant recurrence       NA's 
         4          1       1093 

patient.drugs.drug.9.regimen_number:
   1    2    7   10 NA's 
   1    1    1    1 1094 

patient.drugs.drug.9.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               4                1             1093 

patient.drugs.drug.9.therapy_ongoing:
  no  yes NA's 
   8    2 1088 

patient.drugs.drug.9.therapy_types.therapy_type:
      ancillary    chemotherapy hormone therapy   immunotherapy            NA's 
              1               4               4               1            1088 

patient.drugs.drug.9.total_dose:
 140  280  495 1680 NA's 
   1    1    1    1 1094 

patient.drugs.drug.9.total_dose_units:
  mg NA's 
   4 1094 

patient.drugs.drug.9.tx_on_clinical_trial:
  no NA's 
   5 1093 

patient.drugs.drug.9.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2015    1088 



patient.drugs.drug.clinical_trail_drug_classification:
 n/a NA's 
  37 1061 

patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.00   14.00   14.45   20.00   31.00     318 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-1386.0   117.0   155.0   244.6   207.2  3349.0     582 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-1428.0    49.0    74.0   120.1   126.0  3349.0     354 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                           7                          180 
            partial response               stable disease 
                           4                            8 
                        NA's 
                         899 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.658  11.000  12.000     318 





patient.drugs.drug.regimen_indication_notes:
given to induce menopause               neoadjuvant                      NA's 
                        1                         1                      1096 


patient.drugs.drug.route_of_administrations.route_of_administration:
intramuscular (im)   intravenous (iv)               oral  subcutaneous (sc) 
                 3                235                171                  1 
              NA's 
               688 

patient.drugs.drug.route_of_administrations.route_of_administration.2:
intravenous (iv)             oral             NA's 
               1                1             1096 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
 549  226  323 


patient.drugs.drug.therapy_types.therapy_type.2:
hormone therapy            NA's 
              2            1096 

patient.drugs.drug.therapy_types.therapy_type_notes:
   bisphosphonate therapy given to induce menopause  phase iii clinical trial 
                        1                         1                         2 
                     NA's 
                     1094 



patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
 318   23  757 

patient.drugs.drug.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2011    2012    2012    2013    2015     318 



patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    39                    885                    174 

patient.first_nonlymph_node_metastasis_anatomic_sites.first_nonlymph_node_metastasis_anatomic_site:
          bone          liver           lung other, specify           NA's 
            13              2              5              4           1074 

patient.first_nonlymph_node_metastasis_anatomic_sites.first_nonlymph_node_metastasis_anatomic_site.2:
          bone          liver other, specify           NA's 
             3              4              1           1090 

patient.first_nonlymph_node_metastasis_anatomic_sites.first_nonlymph_node_metastasis_anatomic_site.3:
liver  NA's 
    2  1096 

patient.first_nonlymph_node_metastasis_anatomic_sites.first_nonlymph_node_metastasis_anatomic_site.4:
other, specify           NA's 
             1           1097 



patient.follow_ups.follow_up.2.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    6 1090 

patient.follow_ups.follow_up.2.additional_radiation_therapy:
  no  yes NA's 
   5    3 1090 

patient.follow_ups.follow_up.2.additional_surgery_locoregional_procedure:
  no  yes NA's 
   9    3 1086 

patient.follow_ups.follow_up.2.additional_surgery_metastatic_procedure:
  no NA's 
   1 1097 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   14.00   15.16   24.00   31.00     796 

patient.follow_ups.follow_up.2.days_to_additional_surgery_metastatic_procedure:
 295  798 1066 NA's 
   1    1    1 1095 

patient.follow_ups.follow_up.2.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    385     759    1082    1966    2694    7455    1074 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  217.0   879.5  1532.0  1792.0  2308.5  8605.0     831 

patient.follow_ups.follow_up.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   83.0   180.0   387.0   687.8  1066.0  1922.0    1085 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             3                             50 
                          NA's 
                          1045 

patient.follow_ups.follow_up.2.her2_neu_and_centromere_17_copy_number_metastatic_breast_carcinoma_analysis_input_total_number_count:
  40 NA's 
   1 1097 

patient.follow_ups.follow_up.2.her2_neu_metastatic_breast_carcinoma_copy_analysis_input_total_number:
   4 NA's 
   1 1097 

patient.follow_ups.follow_up.2.lost_follow_up:
  no  yes NA's 
 202   18  878 

patient.follow_ups.follow_up.2.metastatic_breast_carcinoma_erbb2_immunohistochemistry_level_result:
   0   1+   2+ NA's 
   1    3    1 1093 

patient.follow_ups.follow_up.2.metastatic_breast_carcinoma_estrogen_receptor_level_cell_percent_category:
20-29% 90-99%   NA's 
     1      2   1095 

patient.follow_ups.follow_up.2.metastatic_breast_carcinoma_estrogen_receptor_status:
negative positive     NA's 
       4        4     1090 

patient.follow_ups.follow_up.2.metastatic_breast_carcinoma_fluorescence_in_situ_hybridization_diagnostic_proc_centromere_17_signal_result_range:
 2.4 NA's 
   1 1097 

patient.follow_ups.follow_up.2.metastatic_breast_carcinoma_her2_erbb_method_calculation_method_text:
3 point scale          NA's 
            1          1097 

patient.follow_ups.follow_up.2.metastatic_breast_carcinoma_her2_erbb_pos_finding_cell_percent_category:
  <10% 20-29%   NA's 
     1      1   1096 

patient.follow_ups.follow_up.2.metastatic_breast_carcinoma_her2_erbb_pos_finding_fluorescence_in_situ_hybridization_calculation_method_text:
ratio >2.2       NA's 
         1       1097 

patient.follow_ups.follow_up.2.metastatic_breast_carcinoma_her2_neu_chromosone_17_signal_ratio_value:
 1.3  1.7 NA's 
   1    1 1096 

patient.follow_ups.follow_up.2.metastatic_breast_carcinoma_immunohistochemistry_er_pos_cell_score:
  4+ NA's 
   1 1097 

patient.follow_ups.follow_up.2.metastatic_breast_carcinoma_immunohistochemistry_er_positive_finding_scale_type:
4 point scale          NA's 
            5          1093 

patient.follow_ups.follow_up.2.metastatic_breast_carcinoma_immunohistochemistry_pr_pos_cell_score:
  2+ NA's 
   1 1097 

patient.follow_ups.follow_up.2.metastatic_breast_carcinoma_immunohistochemistry_progesterone_receptor_positive_finding_scale_type:
4 point scale          NA's 
            5          1093 

patient.follow_ups.follow_up.2.metastatic_breast_carcinoma_lab_proc_her2_neu_immunohistochemistry_receptor_status:
equivocal  negative      NA's 
        2         6      1090 

patient.follow_ups.follow_up.2.metastatic_breast_carcinoma_lab_proc_her2_neu_in_situ_hybridization_outcome_type:
negative     NA's 
       2     1096 

patient.follow_ups.follow_up.2.metastatic_breast_carcinoma_pos_finding_her2_erbb2_other_measure_scale_text:
3 point scale          NA's 
            2          1096 

patient.follow_ups.follow_up.2.metastatic_breast_carcinoma_progesterone_receptor_level_cell_percent_category:
10-19% 60-69% 70-79% 80-89%   NA's 
     1      1      1      1   1094 

patient.follow_ups.follow_up.2.metastatic_breast_carcinoma_progesterone_receptor_status:
negative positive     NA's 
       3        5     1090 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.858  10.000  12.000     796 


patient.follow_ups.follow_up.2.new_neoplasm_event_type:
  distant metastasis locoregional disease    new primary tumor 
                   8                    2                    2 
                NA's 
                1086 

patient.follow_ups.follow_up.2.new_neoplasm_occurrence_anatomic_site_text:
                                   breast 
                                        1 
lung, bone, liver, brain and skin nodules 
                                        1 
                                     NA's 
                                     1096 

patient.follow_ups.follow_up.2.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  42   12 1044 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    3 1094 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no NA's 
   4 1094 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 524  648  790 1673 NA's 
   1    1    1    1 1094 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.metastatic_breast_carcinoma_erbb2_immunohistochemistry_level_result:
  1+ NA's 
   1 1097 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.metastatic_breast_carcinoma_estrogen_receptor_status:
positive     NA's 
       1     1097 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.metastatic_breast_carcinoma_immunohistochemistry_er_pos_cell_score:
  3+ NA's 
   1 1097 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.metastatic_breast_carcinoma_immunohistochemistry_er_positive_finding_scale_type:
3 point scale          NA's 
            1          1097 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.metastatic_breast_carcinoma_immunohistochemistry_pr_pos_cell_score:
  2+ NA's 
   1 1097 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.metastatic_breast_carcinoma_immunohistochemistry_progesterone_receptor_positive_finding_scale_type:
3 point scale          NA's 
            1          1097 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.metastatic_breast_carcinoma_lab_proc_her2_neu_immunohistochemistry_receptor_status:
negative     NA's 
       1     1097 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.metastatic_breast_carcinoma_progesterone_receptor_status:
positive     NA's 
       1     1097 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
         liver other, specify           NA's 
             2              1           1095 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      3                       1                    1094 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
omentum    NA's 
      1    1097 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no NA's 
   3 1095 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no NA's 
   1 1097 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no NA's 
   1 1097 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
 672 NA's 
   1 1097 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
liver  NA's 
    1  1097 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
distant metastasis               NA's 
                 1               1097 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1 1097 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   6   11 1081 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  12    5 1081 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  586.0   937.5  1446.0  2076.5  2683.5  5117.0    1092 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   21.0   591.5  1290.0  1405.6  1714.5  5117.0    1075 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.her2_neu_and_centromere_17_copy_number_metastatic_breast_carcinoma_analysis_input_total_number_count:
 200 NA's 
   1 1097 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.her2_neu_metastatic_breast_carcinoma_copy_analysis_input_total_number:
 2.1  4.4 NA's 
   1    1 1096 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_erbb2_immunohistochemistry_level_result:
  1+   2+ NA's 
   3    1 1094 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_estrogen_receptor_level_cell_percent_category:
  <10% 90-99%   NA's 
     1      1   1096 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_estrogen_receptor_status:
negative positive     NA's 
       1        6     1091 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_fluorescence_in_situ_hybridization_diagnostic_proc_centromere_17_signal_result_range:
 2.2  3.1 NA's 
   1    1 1096 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_her2_erbb_pos_finding_cell_percent_category:
<10% NA's 
   1 1097 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_her2_neu_chromosone_17_signal_ratio_value:
   1  4.1 NA's 
   1    1 1096 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_immunohistochemistry_er_pos_cell_score:
  2+   3+ NA's 
   1    4 1093 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_immunohistochemistry_er_positive_finding_scale_type:
3 point scale          NA's 
            5          1093 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_immunohistochemistry_pr_pos_cell_score:
  2+   3+ NA's 
   3    2 1093 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_immunohistochemistry_progesterone_receptor_positive_finding_scale_type:
3 point scale          NA's 
            5          1093 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_lab_proc_her2_neu_immunohistochemistry_receptor_status:
negative positive     NA's 
       3        1     1094 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_lab_proc_her2_neu_in_situ_hybridization_outcome_type:
negative     NA's 
       2     1096 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_progesterone_receptor_level_cell_percent_category:
  <10% 70-79%   NA's 
     1      1   1096 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_progesterone_receptor_status:
negative positive     NA's 
       2        4     1092 


patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                     11                       4                       4 
                   NA's 
                   1079 


patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  13    6 1079 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 209   23  866 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       240         28        830 

patient.follow_ups.follow_up.2.postoperative_rx_tx:
  no  yes NA's 
  17  211  870 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
 121  167  810 

patient.follow_ups.follow_up.2.targeted_molecular_therapy:
  no  yes NA's 
   1   54 1043 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
  267    24   807 

patient.follow_ups.follow_up.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2013    2013    2013    2014    2015     796 



patient.follow_ups.follow_up.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   21.00   30.00   24.26   30.00   30.00    1048 

patient.follow_ups.follow_up.3.days_to_death:
 548 1048 NA's 
   1    1 1096 

patient.follow_ups.follow_up.3.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    525    1462    2670    2565    3180    5062    1050 

patient.follow_ups.follow_up.3.lost_follow_up:
  no NA's 
  50 1048 

patient.follow_ups.follow_up.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    3.00    4.42    5.00   12.00    1048 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
 yes NA's 
   1 1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
 yes NA's 
   1 1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 102 NA's 
   1 1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
bone NA's 
   1 1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
distant metastasis               NA's 
                 1               1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1 1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
 yes NA's 
   1 1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no NA's 
   1 1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
 180 NA's 
   1 1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
lung NA's 
   1 1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
distant metastasis               NA's 
                 1               1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1 1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.4.additional_pharmaceutical_therapy:
 yes NA's 
   1 1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.4.additional_radiation_therapy:
  no NA's 
   1 1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
 313 NA's 
   1 1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site:
liver  NA's 
    1  1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
distant metastasis               NA's 
                 1               1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1 1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
 yes NA's 
   1 1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_radiation_therapy:
 yes NA's 
   1 1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
 179 NA's 
   1 1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
 179 2561 NA's 
   1    1 1096 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_estrogen_receptor_status:
negative     NA's 
       1     1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_her2_neu_chromosone_17_signal_ratio_value:
   1 NA's 
   1 1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_lab_proc_her2_neu_in_situ_hybridization_outcome_type:
negative     NA's 
       1     1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_progesterone_receptor_status:
negative     NA's 
       1     1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1           1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
locoregional recurrence                    NA's 
                      1                    1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
axilla   NA's 
     1   1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   1 1097 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  47    2 1049 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        46          4       1048 

patient.follow_ups.follow_up.3.postoperative_rx_tx:
  no  yes NA's 
   1   48 1049 

patient.follow_ups.follow_up.3.radiation_therapy:
  no  yes NA's 
  13   37 1048 

patient.follow_ups.follow_up.3.vital_status:
alive  dead  NA's 
   48     2  1048 

patient.follow_ups.follow_up.3.year_of_form_completion:
2013 2014 2015 NA's 
   3   12   35 1048 

patient.follow_ups.follow_up.4.bcr_followup_barcode:
tcga-ar-a24q-f65802 tcga-e2-a152-f72471                NA's 
                  1                   1                1096 

patient.follow_ups.follow_up.4.bcr_followup_uuid:
4516b9c5-d979-4a2d-837c-5e9079faad42 f8357f98-0281-4fe8-9b0b-288221e20b2d 
                                   1                                    1 
                                NA's 
                                1096 

patient.follow_ups.follow_up.4.day_of_form_completion:
  23   30 NA's 
   1    1 1096 

patient.follow_ups.follow_up.4.days_to_last_followup:
2128 3172 NA's 
   1    1 1096 

patient.follow_ups.follow_up.4.lost_follow_up:
  no NA's 
   2 1096 

patient.follow_ups.follow_up.4.month_of_form_completion:
   6    9 NA's 
   1    1 1096 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no NA's 
   1 1097 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   1 1097 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
1044 NA's 
   1 1097 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
 966 NA's 
   1 1097 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_erbb2_immunohistochemistry_level_result:
  3+ NA's 
   1 1097 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_estrogen_receptor_status:
negative     NA's 
       1     1097 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_lab_proc_her2_neu_immunohistochemistry_receptor_status:
positive     NA's 
       1     1097 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_progesterone_receptor_status:
negative     NA's 
       1     1097 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1           1097 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
locoregional recurrence                    NA's 
                      1                    1097 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
breast   NA's 
     1   1097 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   1 1097 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   1    1 1096 

patient.follow_ups.follow_up.4.person_neoplasm_cancer_status:
tumor free       NA's 
         2       1096 

patient.follow_ups.follow_up.4.postoperative_rx_tx:
 yes NA's 
   2 1096 

patient.follow_ups.follow_up.4.radiation_therapy:
  no  yes NA's 
   1    1 1096 

patient.follow_ups.follow_up.4.vital_status:
alive  NA's 
    2  1096 

patient.follow_ups.follow_up.4.year_of_form_completion:
2014 2015 NA's 
   1    1 1096 

patient.follow_ups.follow_up.additional_pharmaceutical_therapy:
  no  yes NA's 
  18   34 1046 

patient.follow_ups.follow_up.additional_radiation_therapy:
  no  yes NA's 
  36   15 1047 

patient.follow_ups.follow_up.additional_surgery_locoregional_procedure:
  no  yes NA's 
  33   23 1042 

patient.follow_ups.follow_up.additional_surgery_metastatic_procedure:
  no NA's 
  10 1088 



patient.follow_ups.follow_up.breast_carcinoma_estrogen_receptor_status:
negative positive     NA's 
       2        1     1095 

patient.follow_ups.follow_up.breast_carcinoma_immunohistochemistry_pos_cell_score:
  2+ NA's 
   1 1097 

patient.follow_ups.follow_up.breast_carcinoma_progesterone_receptor_status:
negative positive     NA's 
       2        1     1095 

patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   15.00   14.52   22.00   31.00      99 

patient.follow_ups.follow_up.days_to_additional_surgery_locoregional_procedure:
 179  295 1239 NA's 
   1    1    1 1095 

patient.follow_ups.follow_up.days_to_additional_surgery_metastatic_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  143.0   541.5  1018.5  1195.0  1380.5  3862.0    1078 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  158.0   540.5  1018.0  1470.7  2246.2  6593.0    1012 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -31.0   408.5   643.0  1030.2  1308.0  7126.0     187 

patient.follow_ups.follow_up.days_to_last_known_alive:
 735 2576 NA's 
   1    1 1096 

patient.follow_ups.follow_up.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   54.0   418.8   742.5   999.5  1360.2  3862.0    1038 

patient.follow_ups.follow_up.er_level_cell_percentage_category:
20-29%   NA's 
     1   1097 

patient.follow_ups.follow_up.fluorescence_in_situ_hybridization_diagnostic_procedure_chromosome_17_signal_result_range:
  49 NA's 
   1 1097 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             1                            466 
                          NA's 
                           631 

patient.follow_ups.follow_up.her2_erbb_method_calculation_method_text:
3+ positive        NA's 
          1        1097 

patient.follow_ups.follow_up.her2_erbb_pos_finding_cell_percent_category:
<10% NA's 
   1 1097 

patient.follow_ups.follow_up.her2_erbb_pos_finding_fluorescence_in_situ_hybridization_calculation_method_text:
>=2.0 positive           NA's 
             1           1097 

patient.follow_ups.follow_up.her2_immunohistochemistry_level_result:
   0 NA's 
   2 1096 

patient.follow_ups.follow_up.her2_neu_and_centromere_17_copy_number_analysis_input_total_number_count:
  31 NA's 
   1 1097 

patient.follow_ups.follow_up.her2_neu_and_centromere_17_copy_number_metastatic_breast_carcinoma_analysis_input_total_number_count:
  40 NA's 
   1 1097 

patient.follow_ups.follow_up.her2_neu_breast_carcinoma_copy_analysis_input_total_number:
  62 NA's 
   1 1097 

patient.follow_ups.follow_up.her2_neu_chromosone_17_signal_ratio_value:
   1  1.3 NA's 
   1    1 1096 

patient.follow_ups.follow_up.her2_neu_metastatic_breast_carcinoma_copy_analysis_input_total_number:
   4 NA's 
   1 1097 

patient.follow_ups.follow_up.immunohistochemistry_positive_cell_score:
  2+ NA's 
   1 1097 

patient.follow_ups.follow_up.lab_proc_her2_neu_immunohistochemistry_receptor_status:
negative     NA's 
       2     1096 

patient.follow_ups.follow_up.lab_procedure_her2_neu_in_situ_hybrid_outcome_type:
negative     NA's 
       2     1096 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
 372   46  680 

patient.follow_ups.follow_up.metastatic_breast_carcinoma_erbb2_immunohistochemistry_level_result:
  1+   2+ NA's 
   3    4 1091 

patient.follow_ups.follow_up.metastatic_breast_carcinoma_estrogen_receptor_detection_method_text:
allred score 0+0 = 0      image cytometry                 NA's 
                   1                    3                 1094 

patient.follow_ups.follow_up.metastatic_breast_carcinoma_estrogen_receptor_level_cell_percent_category:
  <10% 10-19% 90-99%   NA's 
     1      1      3   1093 

patient.follow_ups.follow_up.metastatic_breast_carcinoma_estrogen_receptor_status:
negative positive     NA's 
       7        8     1083 

patient.follow_ups.follow_up.metastatic_breast_carcinoma_fluorescence_in_situ_hybridization_diagnostic_proc_centromere_17_signal_result_range:
 2.4 NA's 
   1 1097 

patient.follow_ups.follow_up.metastatic_breast_carcinoma_her2_erbb_method_calculation_method_text:
3+ positive        NA's 
          3        1095 

patient.follow_ups.follow_up.metastatic_breast_carcinoma_her2_erbb_pos_finding_cell_percent_category:
  <10% 20-29% 30-39%   NA's 
     3      1      1   1093 

patient.follow_ups.follow_up.metastatic_breast_carcinoma_her2_erbb_pos_finding_fluorescence_in_situ_hybridization_calculation_method_text:
ratio > 2.2        NA's 
          1        1097 

patient.follow_ups.follow_up.metastatic_breast_carcinoma_her2_neu_chromosone_17_signal_ratio_value:
0.95  1.7 NA's 
   1    1 1096 

patient.follow_ups.follow_up.metastatic_breast_carcinoma_immunohistochemistry_er_pos_cell_score:
  4+ NA's 
   2 1096 

patient.follow_ups.follow_up.metastatic_breast_carcinoma_immunohistochemistry_er_positive_finding_scale_type:
other reporting scale                  NA's 
                    2                  1096 

patient.follow_ups.follow_up.metastatic_breast_carcinoma_immunohistochemistry_pr_pos_cell_score:
  1+   2+ NA's 
   1    2 1095 

patient.follow_ups.follow_up.metastatic_breast_carcinoma_immunohistochemistry_progesterone_receptor_positive_finding_scale_type:
other reporting scale                  NA's 
                    2                  1096 

patient.follow_ups.follow_up.metastatic_breast_carcinoma_lab_proc_her2_neu_immunohistochemistry_receptor_status:
equivocal  negative      NA's 
        3         7      1088 

patient.follow_ups.follow_up.metastatic_breast_carcinoma_lab_proc_her2_neu_in_situ_hybridization_outcome_type:
negative     NA's 
       4     1094 

patient.follow_ups.follow_up.metastatic_breast_carcinoma_pos_finding_progesterone_receptor_other_measure_scale_text:
     105 fmol/mg allred score = 0          fmol/mg             NA's 
               1                1                2             1094 

patient.follow_ups.follow_up.metastatic_breast_carcinoma_progesterone_receptor_detection_method_text:
allred score 0+0 = 0      image cytometry                 NA's 
                   1                    3                 1094 

patient.follow_ups.follow_up.metastatic_breast_carcinoma_progesterone_receptor_level_cell_percent_category:
10-19% 70-79% 80-89% 90-99%   NA's 
     1      1      1      1   1094 

patient.follow_ups.follow_up.metastatic_breast_carcinoma_progesterone_receptor_status:
negative positive     NA's 
       8        7     1083 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.474  10.000  12.000      99 


patient.follow_ups.follow_up.new_neoplasm_event_type:
  distant metastasis locoregional disease    new primary tumor 
                  32                    8                    9 
                NA's 
                1049 


patient.follow_ups.follow_up.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 413   52  633 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    4 1093 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
   4    1 1093 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
6359 NA's 
   1 1097 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    192     443     762    1081     813    4343    1089 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.metastatic_breast_carcinoma_estrogen_receptor_status:
negative     NA's 
       1     1097 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.metastatic_breast_carcinoma_lab_proc_her2_neu_immunohistochemistry_receptor_status:
negative     NA's 
       1     1097 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.metastatic_breast_carcinoma_progesterone_receptor_status:
negative     NA's 
       1     1097 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
          bone          liver other, specify           NA's 
             1              3              3           1091 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      4                       2                    1092 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
              chest wall, breast recurrence 
                                          1 
                   intrathoracic lymph node 
                                          1 
mediastinal and supraclavicular lymph nodes 
                                          1 
                                       NA's 
                                       1095 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   5    1 1092 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    2 1095 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no NA's 
   3 1095 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
 574 1351 6535 NA's 
   1    1    1 1095 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
          bone           lung other, specify           NA's 
             1              1              1           1095 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
distant metastasis               NA's 
                 3               1095 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_occurrence_anatomic_site_text:
intrathoracic lymph node                     NA's 
                       1                     1097 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no NA's 
   3 1095 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.additional_pharmaceutical_therapy:
  no NA's 
   1 1097 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.additional_radiation_therapy:
 yes NA's 
   1 1097 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
6536 NA's 
   1 1097 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site:
brain  NA's 
    1  1097 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
distant metastasis               NA's 
                 1               1097 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.4.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1 1097 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   7   10 1081 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
   8    9 1081 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  258.0   506.8   884.0   907.5  1192.2  1750.0    1090 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  174.0   422.8   785.5  1199.1  1349.0  4224.0    1064 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_erbb2_immunohistochemistry_level_result:
  1+   3+ NA's 
   1    1 1096 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_estrogen_receptor_level_cell_percent_category:
50-59% 90-99%   NA's 
     1      2   1095 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_estrogen_receptor_status:
negative positive     NA's 
       1        7     1090 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_her2_erbb_pos_finding_cell_percent_category:
50-59%   NA's 
     1   1097 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_her2_neu_chromosone_17_signal_ratio_value:
 1.2 NA's 
   1 1097 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_immunohistochemistry_er_pos_cell_score:
  2+   3+ NA's 
   1    1 1096 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_immunohistochemistry_er_positive_finding_scale_type:
3 point scale          NA's 
            3          1095 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_immunohistochemistry_pr_pos_cell_score:
  1+ NA's 
   2 1096 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_immunohistochemistry_progesterone_receptor_positive_finding_scale_type:
3 point scale          NA's 
            3          1095 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_lab_proc_her2_neu_immunohistochemistry_receptor_status:
negative positive     NA's 
       3        1     1094 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_lab_proc_her2_neu_in_situ_hybridization_outcome_type:
negative     NA's 
       1     1097 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_progesterone_receptor_level_cell_percent_category:
  <10% 10-19%   NA's 
     2      1   1095 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_progesterone_receptor_status:
negative positive     NA's 
       4        3     1091 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
          bone          brain           lung other, specify           NA's 
            12              1              5             10           1070 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                     18                       6                       5 
                   NA's 
                   1069 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  11    8 1079 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 330   34  734 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       833         91        174 

patient.follow_ups.follow_up.pos_finding_metastatic_breast_carcinoma_estrogen_receptor_other_measuremenet_scale_text:
     133 fmol/mg allred score = 0          fmol/mg             NA's 
               1                1                2             1094 

patient.follow_ups.follow_up.postoperative_rx_tx:
  no  yes NA's 
 102  299  697 

patient.follow_ups.follow_up.progesterone_receptor_level_cell_percent_category:
60-69%   NA's 
     1   1097 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
 433  552  113 

patient.follow_ups.follow_up.targeted_molecular_therapy:
  no  yes NA's 
  48  529  521 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
  912    87    99 

patient.follow_ups.follow_up.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2014    2015      99 

patient.gender:
female   male 
  1086     12 

patient.her2_and_centromere_17_positive_finding_other_measurement_scale_text:
                >2.2 positive                             1 
                            1                             1 
her2 amplification - negative       vysis autovysion system 
                            1                             1 
                         NA's 
                         1094 




patient.her2_immunohistochemistry_level_result:
   0   1+   2+   3+ NA's 
  61  272  199   90  476 

patient.her2_neu_and_centromere_17_copy_number_analysis_input_total_number_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   30.00   40.00   93.34   60.00  569.00     994 


patient.her2_neu_chromosone_17_signal_ratio_value:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.420   1.080   1.200   1.727   1.460  16.300     865 



patient.history_of_neoadjuvant_treatment:
  no  yes NA's 
1084   13    1 





patient.informed_consent_verified:
 yes 
1098 



patient.lab_proc_her2_neu_immunohistochemistry_receptor_status:
    equivocal indeterminate      negative      positive          NA's 
          179            12           565           164           178 

patient.lab_procedure_her2_neu_in_situ_hybrid_outcome_type:
    equivocal indeterminate      negative      positive          NA's 
            5             4           333            78           678 

patient.lymph_node_examined_count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    3.00    9.00   10.64   16.00   44.00     126 

patient.margin_status:
   close negative positive     NA's 
      31      920       79       68 

patient.menopause_status:
                                               indeterminate (neither pre or postmenopausal) 
                                                                                          34 
                                              peri (6-12 months since last menstrual period) 
                                                                                          39 
           post (prior bilateral ovariectomy or >12 mo since lmp with no prior hysterectomy) 
                                                                                         705 
pre (<6 months since lmp and no prior bilateral ovariectomy and not on estrogen replacement) 
                                                                                         230 
                                                                                        NA's 
                                                                                          90 

patient.metastatic_breast_carcinoma_erbb2_immunohistochemistry_level_result:
  1+   2+ NA's 
   4    1 1093 

patient.metastatic_breast_carcinoma_estrogen_receptor_level_cell_percent_category:
  <10% 70-79% 90-99%   NA's 
     2      2      2   1092 

patient.metastatic_breast_carcinoma_estrogen_receptor_status:
negative positive     NA's 
       3        9     1086 

patient.metastatic_breast_carcinoma_her2_erbb_pos_finding_cell_percent_category:
<10% NA's 
   3 1095 

patient.metastatic_breast_carcinoma_her2_neu_chromosone_17_signal_ratio_value:
1.09 2.47 NA's 
   1    1 1096 

patient.metastatic_breast_carcinoma_immunohistochemistry_er_pos_cell_score:
  3+ NA's 
   1 1097 

patient.metastatic_breast_carcinoma_immunohistochemistry_pr_pos_cell_score:
  1+ NA's 
   1 1097 

patient.metastatic_breast_carcinoma_lab_proc_her2_neu_immunohistochemistry_receptor_status:
equivocal  negative      NA's 
        1         8      1089 

patient.metastatic_breast_carcinoma_lab_proc_her2_neu_in_situ_hybridization_outcome_type:
negative     NA's 
       3     1095 

patient.metastatic_breast_carcinoma_progesterone_receptor_level_cell_percent_category:
<10% NA's 
   4 1094 

patient.metastatic_breast_carcinoma_progesterone_receptor_status:
negative positive     NA's 
       7        4     1087 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   6.000   6.674   9.000  12.000 

patient.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
 yes NA's 
   1 1097 

patient.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
 yes NA's 
   1 1097 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 301  667 NA's 
   1    1 1096 

patient.new_tumor_events.new_tumor_event.2.metastatic_breast_carcinoma_estrogen_receptor_status:
negative     NA's 
       1     1097 

patient.new_tumor_events.new_tumor_event.2.metastatic_breast_carcinoma_lab_proc_her2_neu_immunohistochemistry_receptor_status:
negative     NA's 
       1     1097 

patient.new_tumor_events.new_tumor_event.2.metastatic_breast_carcinoma_progesterone_receptor_status:
negative     NA's 
       1     1097 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
          lung other, specify           NA's 
             1              1           1096 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      1                       1                    1096 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
chest wall       NA's 
         1       1097 

patient.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1 1097 

patient.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
 yes NA's 
   1 1097 

patient.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no NA's 
   1 1097 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
 301  574 NA's 
   1    1 1096 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
 bone liver  NA's 
    1     1  1096 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
distant metastasis               NA's 
                 2               1096 

patient.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no NA's 
   1 1097 

patient.new_tumor_events.new_tumor_event.4.days_to_new_tumor_event_after_initial_treatment:
 301 NA's 
   1 1097 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_occurrence_anatomic_site:
bone NA's 
   1 1097 

patient.new_tumor_events.new_tumor_event.4.new_neoplasm_event_type:
distant metastasis               NA's 
                 1               1097 

patient.new_tumor_events.new_tumor_event.5.days_to_new_tumor_event_after_initial_treatment:
 301 NA's 
   1 1097 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1           1097 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_event_type:
distant metastasis               NA's 
                 1               1097 

patient.new_tumor_events.new_tumor_event.5.new_neoplasm_occurrence_anatomic_site_text:
adrenal glands           NA's 
             1           1097 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    6 1091 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
   4    3 1091 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
 181  596 NA's 
   1    1 1096 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  181.0   301.0   766.0   777.8   994.0  1543.0    1089 

patient.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_erbb2_immunohistochemistry_level_result:
  1+   2+ NA's 
   7    1 1090 

patient.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_estrogen_receptor_level_cell_percent_category:
  <10% 70-79% 90-99%   NA's 
     4      2      2   1090 

patient.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_estrogen_receptor_status:
negative positive     NA's 
       5       10     1083 

patient.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_her2_erbb_pos_finding_cell_percent_category:
<10% NA's 
   5 1093 

patient.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_her2_neu_chromosone_17_signal_ratio_value:
1.09 2.47 NA's 
   1    1 1096 

patient.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_immunohistochemistry_er_pos_cell_score:
  1+   3+ NA's 
   2    2 1094 

patient.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_immunohistochemistry_er_positive_finding_scale_type:
3 point scale 4 point scale          NA's 
            3             3          1092 

patient.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_immunohistochemistry_pr_pos_cell_score:
  1+   2+ NA's 
   3    1 1094 

patient.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_immunohistochemistry_progesterone_receptor_positive_finding_scale_type:
3 point scale 4 point scale          NA's 
            3             3          1092 

patient.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_lab_proc_her2_neu_immunohistochemistry_receptor_status:
equivocal  negative      NA's 
        1        11      1086 

patient.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_lab_proc_her2_neu_in_situ_hybridization_outcome_type:
negative     NA's 
       3     1095 

patient.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_pos_finding_progesterone_receptor_other_measure_scale_text:
allred score=5           NA's 
             1           1097 

patient.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_progesterone_receptor_level_cell_percent_category:
<10% NA's 
   6 1092 

patient.new_tumor_events.new_tumor_event.metastatic_breast_carcinoma_progesterone_receptor_status:
negative positive     NA's 
       9        5     1084 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
          bone           lung other, specify           NA's 
             3              2              4           1089 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                      5                       4                       1 
                   NA's 
                   1088 

patient.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
            breast  breast recurrence          chestwall malignant melanoma 
                 1                  1                  1                  1 
              NA's 
              1094 

patient.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   6    2 1090 

patient.new_tumor_events.new_tumor_event.pos_finding_metastatic_breast_carcinoma_estrogen_receptor_other_measuremenet_scale_text:
   `    8 NA's 
   1    1 1096 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 185   10  903 

patient.number_of_lymphnodes_positive_by_he:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    1.00    2.36    2.00   35.00     168 

patient.number_of_lymphnodes_positive_by_ihc:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.2667  0.0000  8.0000     753 


patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       879         94        125 



patient.pos_finding_metastatic_breast_carcinoma_estrogen_receptor_other_measuremenet_scale_text:
   ` NA's 
   1 1097 



patient.postoperative_rx_tx:
  no  yes NA's 
  38  127  933 

patient.primary_lymph_node_presentation_assessment:
  no  yes NA's 
  33  693  372 

patient.prior_dx:
  no  yes NA's 
1028   69    1 


patient.race:
american indian or alaska native                            asian 
                               1                               61 
       black or african american                            white 
                             183                              758 
                            NA's 
                              95 

patient.radiation_therapy:
  no  yes NA's 
  93   73  932 

patient.radiations.radiation.2.anatomic_treatment_site:
 distant recurrence        distant site primary tumor field       regional site 
                  4                   3                  21                  36 
               NA's 
               1034 



patient.radiations.radiation.2.course_number:
   1    2    3 NA's 
   4    3    2 1089 

patient.radiations.radiation.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.75   14.00   13.70   22.25   29.00    1034 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   53.0   190.2   257.5   389.6   303.0  5111.0    1034 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   41.0   166.8   224.0   359.4   264.2  5098.0    1034 

patient.radiations.radiation.2.measure_of_response:
               complete response                 partial response 
                              44                                1 
radiographic progressive disease                   stable disease 
                               4                                3 
                            NA's 
                            1046 

patient.radiations.radiation.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.312   8.000  12.000    1034 

patient.radiations.radiation.2.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   25.00   19.78   28.00   43.00    1053 

patient.radiations.radiation.2.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    50.4  2200.0  2707.5  5040.0  6040.0    1035 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no NA's 
  64 1034 

patient.radiations.radiation.2.radiation_type:
               external           external beam other: specify in notes 
                     54                       9                       1 
                   NA's 
                   1034 

patient.radiations.radiation.2.radiation_type_notes:
gamma knife radiasurgery                     NA's 
                       1                     1097 

patient.radiations.radiation.2.regimen_indication:
   adjuvant  palliative progression        NA's 
          8           1           1        1088 

patient.radiations.radiation.2.units:
 cgy   gy NA's 
  44   19 1035 

patient.radiations.radiation.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2014    2013    2014    2015    1034 




patient.radiations.radiation.3.course_number:
   2 NA's 
   3 1095 

patient.radiations.radiation.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1       6      14      14      22      28    1079 

patient.radiations.radiation.3.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   78.0   245.5   275.0   579.3   305.0  6052.0    1079 

patient.radiations.radiation.3.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   44.0   233.2   250.5   576.8   275.8  6048.0    1080 

patient.radiations.radiation.3.measure_of_response:
complete response    stable disease              NA's 
               14                 1              1083 

patient.radiations.radiation.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   4.895   6.500  11.000    1079 

patient.radiations.radiation.3.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    5.00    6.00   13.23   25.00   28.00    1085 

patient.radiations.radiation.3.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    72.5  1000.0  1543.2  1700.0  5040.0    1079 

patient.radiations.radiation.3.radiation_treatment_ongoing:
  no NA's 
  19 1079 

patient.radiations.radiation.3.radiation_type:
     external external beam          NA's 
           15             4          1079 

patient.radiations.radiation.3.regimen_indication:
  adjuvant palliative recurrence       NA's 
         2          1          1       1094 

patient.radiations.radiation.3.units:
 cgy   gy NA's 
  12    6 1080 

patient.radiations.radiation.3.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2012    2014    2013    2014    2015    1079 

patient.radiations.radiation.4.anatomic_treatment_site:
 distant recurrence        distant site primary tumor field       regional site 
                  1                   1                   3                   1 
               NA's 
               1092 



patient.radiations.radiation.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  14.00   18.00   21.50   20.50   22.75   26.00    1092 

patient.radiations.radiation.4.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    101     249     291    1492    1185    6552    1092 

patient.radiations.radiation.4.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   80.0   214.2   286.0  1474.8  1173.0  6538.0    1092 

patient.radiations.radiation.4.measure_of_response:
               complete response radiographic progressive disease 
                               4                                1 
                  stable disease                             NA's 
                               1                             1092 

patient.radiations.radiation.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    3.25    4.50    5.00    7.25    8.00    1092 

patient.radiations.radiation.4.numfractions:
   5   10   24 NA's 
   1    1    1 1095 

patient.radiations.radiation.4.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   30.75  516.50 1478.83 2500.00 4800.00    1092 

patient.radiations.radiation.4.radiation_treatment_ongoing:
  no NA's 
   6 1092 

patient.radiations.radiation.4.radiation_type:
external     NA's 
       6     1092 

patient.radiations.radiation.4.units:
 cgy   gy NA's 
   3    3 1092 

patient.radiations.radiation.4.year_of_form_completion:
2012 2013 2014 NA's 
   1    2    3 1092 

patient.radiations.radiation.5.anatomic_treatment_site:
regional site          NA's 
            1          1097 

patient.radiations.radiation.5.bcr_radiation_barcode:
tcga-e9-a5up-r57723                NA's 
                  1                1097 

patient.radiations.radiation.5.bcr_radiation_uuid:
5578f153-ccb2-4b38-9bd2-2df088dd9bc2                                 NA's 
                                   1                                 1097 

patient.radiations.radiation.5.day_of_form_completion:
  21 NA's 
   1 1097 

patient.radiations.radiation.5.days_to_radiation_therapy_end:
 101 NA's 
   1 1097 

patient.radiations.radiation.5.days_to_radiation_therapy_start:
  80 NA's 
   1 1097 

patient.radiations.radiation.5.measure_of_response:
complete response              NA's 
                1              1097 

patient.radiations.radiation.5.month_of_form_completion:
   3 NA's 
   1 1097 

patient.radiations.radiation.5.radiation_dosage:
  30 NA's 
   1 1097 

patient.radiations.radiation.5.radiation_treatment_ongoing:
  no NA's 
   1 1097 

patient.radiations.radiation.5.radiation_type:
external     NA's 
       1     1097 

patient.radiations.radiation.5.units:
  gy NA's 
   1 1097 

patient.radiations.radiation.5.year_of_form_completion:
2014 NA's 
   1 1097 




patient.radiations.radiation.course_number:
   1    2 NA's 
 253    2  843 

patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   14.00   14.95   21.00   31.00     570 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-1306.0   150.0   224.0   220.7   273.5  3342.0     591 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-1347.0   106.5   182.0   177.8   229.0  3349.0     592 

patient.radiations.radiation.measure_of_response:
               complete response                 partial response 
                             172                                3 
radiographic progressive disease                   stable disease 
                               8                                4 
                            NA's 
                             911 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   8.000   7.231  11.000  12.000     570 



patient.radiations.radiation.radiation_treatment_ongoing:
  no  yes NA's 
 518    4  576 


patient.radiations.radiation.radiation_type_notes:
                                 brachytherapy (iridium 192) 
                                                           1 
high-dose rate brachytherapy of intracavitary type mammosite 
                                                           1 
            partial breast irradiation using a savi catheter 
                                                           1 
                                   stereostatic radiotherapy 
                                                           1 
                                                        NA's 
                                                        1094 

patient.radiations.radiation.regimen_indication:
  adjuvant palliative recurrence       NA's 
       294          2          2        800 

patient.radiations.radiation.units:
 cgy   gy NA's 
 379   94  625 

patient.radiations.radiation.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2015     570 



patient.stage_event.tnm_categories.pathologic_categories.pathologic_m:
cm0 (i+)       m0       m1       mx 
       6      907       22      163 




patient.tissue_prospective_collection_indicator:
  no  yes NA's 
 732  362    4 

patient.tissue_retrospective_collection_indicator:
  no  yes NA's 
 362  732    4 


patient.tumor_tissue_site:
breast 
  1098 

patient.vital_status:
alive  dead 
  994   104 

patient.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   2010    2011    2011    2011    2011    2015 

patient.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1988    2007    2009    2008    2010    2013       2 


patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2:
case matched methylation control              germline comparison 
                             102                                6 
                            NA's 
                             990 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3:
germline comparison                NA's 
                 33                1065 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus 
             943              155 

patient.biospecimen_cqcf.consent_or_death_status:
consented  deceased    waiver      NA's 
     1055        41         1         1 


patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -256.0    63.0   295.0   979.6  1313.0  7852.0       9 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -57.00    1.00   21.00   33.75   36.00 2253.00       3 

patient.biospecimen_cqcf.digital_image_submitted:
  no  yes NA's 
 882   43  173 

patient.biospecimen_cqcf.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                     4                    148                    946 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site:
breast   NA's 
  1077     21 


patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                            1093 
yes, pharmaceutical treatment prior to resection 
                                               3 
                                            NA's 
                                               2 

patient.biospecimen_cqcf.laterality:
 left right  NA's 
  564   521    13 



patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -585.0     0.0    20.0   138.4    42.0  6244.0     939 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0330  0.1807  0.2735  0.2819  0.3505  0.6940    1056 


patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty:
  13   15   20 NA's 
   1    5   36 1056 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  29.00   57.35   72.00   90.51  111.00  400.00    1056 


patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_anatomic_site:
breast  other   NA's 
    80      2   1016 

patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           79          1019 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1    2    3 NA's 
 106  108    2  882 


patient.biospecimen_cqcf.normal_controls.normal_control.3.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  55.00   82.25  122.00  629.33 1174.25 1883.00    1092 

patient.biospecimen_cqcf.normal_controls.normal_control.3.method_of_normal_sample_procurement:
        blood draw surgical resection               NA's 
                 2                  4               1092 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_anatomic_site:
breast   NA's 
     3   1095 

patient.biospecimen_cqcf.normal_controls.normal_control.3.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            3          1095 

patient.biospecimen_cqcf.normal_controls.normal_control.3.vial_number:
   1    2    3 NA's 
   3    2    2 1091 

patient.biospecimen_cqcf.normal_controls.normal_control.4.bcr_sample_uuid:
6605bd50-5fc1-41de-9452-32ff093b6841 66de277d-0402-4e8c-9b77-a160e27d203b 
                                   1                                    1 
8e23740c-1ee6-4ccc-a5a4-2e89f3e3557d f8dadcb3-600c-458c-a6a5-a6a7bd663fec 
                                   1                                    1 
                                NA's 
                                1094 

patient.biospecimen_cqcf.normal_controls.normal_control.4.days_to_normal_sample_procurement:
  26   31  113  131 NA's 
   1    1    1    1 1094 

patient.biospecimen_cqcf.normal_controls.normal_control.4.method_of_normal_sample_procurement:
surgical resection    tumor resection               NA's 
                 3                  1               1094 

patient.biospecimen_cqcf.normal_controls.normal_control.4.normal_tissue.normal_tissue_anatomic_site:
breast   NA's 
     1   1097 

patient.biospecimen_cqcf.normal_controls.normal_control.4.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
            1          1097 

patient.biospecimen_cqcf.normal_controls.normal_control.4.vial_number:
   1    2 NA's 
   1    3 1094 

patient.biospecimen_cqcf.normal_controls.normal_control.5.bcr_sample_uuid:
d7da0785-ab1a-4813-9748-cacd97152a8d eecb3bda-a10d-427c-915e-f88b151b83ea 
                                   1                                    1 
fa7527f5-7ae6-408d-b7d2-6cccd66fb990                                 NA's 
                                   1                                 1095 

patient.biospecimen_cqcf.normal_controls.normal_control.5.days_to_normal_sample_procurement:
  31  131 1522 NA's 
   1    1    1 1095 

patient.biospecimen_cqcf.normal_controls.normal_control.5.method_of_normal_sample_procurement:
        blood draw surgical resection               NA's 
                 1                  2               1095 

patient.biospecimen_cqcf.normal_controls.normal_control.5.vial_number:
   2 NA's 
   3 1095 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-1430.0     1.0    22.0   102.2    42.0  6244.0     337 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
   0.0330    0.1685    0.2500   15.0377    0.4690 1247.4300       776 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.524  20.000  20.000  21.814  20.000 124.940     776 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   52.95   80.00  118.02  144.72  500.00     776 


patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_anatomic_site:
breast  other   NA's 
   188     25    885 

patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
          188           910 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
   1    2    3 NA's 
 956   58    3   81 



patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
  no  yes NA's 
   1 1096    1 

patient.biospecimen_cqcf.path_confirm_report_attached:
  no  yes 
  66 1032 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
 yes 
1098 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
  no  yes 
   1 1097 

patient.biospecimen_cqcf.race:
                    asian black or african american                     white 
                        5                       115                        88 
                     NA's 
                      890 

patient.biospecimen_cqcf.reason_path_confirm_diagnosis_not_matching:
                                    macrodissection performed at tss to select for a region containing an acceptable tcga diagnosis 
                                                                                                                                  3 
pathology review of frozen section for tcga determined histological subtype different from the pathology report (see note at right) 
                                                                                                                                  1 
                                                                                                                               NA's 
                                                                                                                               1094 

patient.biospecimen_cqcf.sample_prescreened:
 yes NA's 
1096    2 

patient.biospecimen_cqcf.site_of_disease:
breast   NA's 
  1085     13 

patient.biospecimen_cqcf.top_slide_submitted:
  no  yes NA's 
 119  807  172 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   9.033  28.750  30.000    1068 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  40.00   70.00   70.00   76.67   88.75   95.00    1068 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   50.0   107.5   200.0   242.4   325.0   670.0    1070 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   1    2 NA's 
  20   14 1064 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.bcr_sample_uuid:
786ae066-c934-4249-85f3-398dc4495dc3                                 NA's 
                                   1                                 1097 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_necrosis_percent:
   0 NA's 
   1 1097 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_nuclei_percent:
  70 NA's 
   1 1097 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_weight:
  50 NA's 
   1 1097 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.vial_number:
   3 NA's 
   1 1097 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   6.162   9.500  30.000       3 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   70.00   75.00   77.69   85.00  100.00       3 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   31.0   175.8   216.5   369.0   418.2  2990.0      58 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
   1    2 NA's 
1095    1    2 


patient.days_to_index:
   0 NA's 
1096    2 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
primary tumor and metastatic tumor(s)                                  NA's 
                                    7                                  1091 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   16.0   155.8   463.0  1070.7  1378.8  7858.0       2 

patient.samples.sample.2.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   40.0   140.0   230.0   311.8   400.0  1390.0    1005 

patient.samples.sample.2.is_ffpe:
  no  yes NA's 
1085   12    1 

patient.samples.sample.2.oct_embedded:
false  true 
 1050    48 



patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-bh-a0hx-10a-02x-a073-09                         NA's 
                           1                         1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
12d9e37e-8855-4473-ba54-89cdfe9e1f4f                                 NA's 
                                   1                                 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
99030759     NA's 
       1     1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.center_id:
   9 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.concentration:
 0.5 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   8 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   9 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.plate_column:
  10 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.plate_id:
a073 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   h NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.quantity:
  40 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.volume:
  80 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-bh-a0hx-10a-02x-a072-08                         NA's 
                           1                         1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
27f06d76-ce54-4be7-86e6-53ef35b42170                                 NA's 
                                   1                                 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
99036058     NA's 
       1     1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.center_id:
   8 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.concentration:
 0.5 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   8 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   9 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_column:
  10 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_id:
a072 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_row:
   h NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.quantity:
  40 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.volume:
  80 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.analyte_type:
repli-g x (qiagen) dna                   NA's 
                     1                   1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.analyte_type_id:
   x NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.bcr_analyte_barcode:
tcga-bh-a0hx-10a-02x                 NA's 
                   1                 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.bcr_analyte_uuid:
d681989d-73c9-4192-8528-f011e7533682                                 NA's 
                                   1                                 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                            1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   1 1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
repli-g x      NA's 
        1      1097 

patient.samples.sample.2.portions.portion.2.analytes.analyte.a260_a280_ratio:
1.74 1.79 1.88    2 NA's 
   1    1    1    1 1094 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
   2 1096 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
   2 1096 



patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99029910  99030756  99065486 112296899 132179171 132179172      1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.center_id:
   7    9   13 NA's 
   1    2    2 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.07    0.15    0.16    0.21    0.17    0.50    1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   8   13   19 NA's 
   2    2    1 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no  yes NA's 
   3    2 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   3    4    9 NA's 
   2    1    2 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_column:
   4   10   11 NA's 
   3    1    1 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_id:
a071 a075 a14d a27d NA's 
   1    1    1    2 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_row:
   a    b    e    h NA's 
   2    1    1    1 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.93    3.00    3.20   10.33    4.54   40.00    1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
   5 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.volume:
13.34    20  26.7    80  NA's 
    1     2     1     1  1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 2013 NA's 
   2    1    2 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   2 1096 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   2 1096 



patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99026893  99029939  99030749 117670350 145632075 145632092      1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.center_id:
   7    8   13 NA's 
   2    2    1 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.07    0.15    0.16    0.21    0.17    0.50    1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.day_of_shipment:
   8   14   19 NA's 
   2    2    1 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no  yes NA's 
   3    2 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.month_of_shipment:
   3    4    9 NA's 
   2    1    2 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_column:
   4   10   11 NA's 
   3    1    1 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_id:
a070 a074 a14c a277 NA's 
   1    1    1    2 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_row:
   a    b    e    h NA's 
   2    1    1    1 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   1.950   2.080   9.766   3.400  40.000    1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   5 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.volume:
  13   20   80 NA's 
   2    2    1 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 2013 NA's 
   2    1    2 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.amount:
 6.91  7.92 15.72 17.93  NA's 
    1     1     1     1  1094 

patient.samples.sample.2.portions.portion.2.analytes.analyte.analyte_type:
                 dna repli-g (qiagen) dna                  rna 
                   1                    1                    3 
                NA's 
                1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.analyte_type_id:
   d    r    w NA's 
   1    3    1 1093 



patient.samples.sample.2.portions.portion.2.analytes.analyte.concentration:
0.13 0.15 0.16 0.17 NA's 
   1    1    1    1 1094 

patient.samples.sample.2.portions.portion.2.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   2 1096 

patient.samples.sample.2.portions.portion.2.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   2 1096 

patient.samples.sample.2.portions.portion.2.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               5 
                                            NA's 
                                            1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.is_derived_from_ffpe:
  no  yes NA's 
   3    2 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.protocols.protocol.experimental_protocol_type:
                adna preparation type highpure mirna (allprep dna) ffpe rna 
                                    1                                     2 
            mirvana (allprep dna) rna                               repli-g 
                                    1                                     1 
                                 NA's 
                                 1093 

patient.samples.sample.2.portions.portion.2.analytes.analyte.rna.ratio_28s_18s:
   0  1.4 NA's 
   2    1 1095 

patient.samples.sample.2.portions.portion.2.analytes.analyte.rna.rinvalue:
 2.1  2.7    8 NA's 
   1    1    1 1095 

patient.samples.sample.2.portions.portion.2.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      4    1094 

patient.samples.sample.2.portions.portion.2.analytes.analyte.subportion_sequence:
   2 NA's 
   1 1097 



patient.samples.sample.2.portions.portion.2.day_of_creation:
   8   18   28 NA's 
   1    2    2 1093 

patient.samples.sample.2.portions.portion.2.is_ffpe:
  no  yes NA's 
   3    2 1093 

patient.samples.sample.2.portions.portion.2.month_of_creation:
   2    4    7 NA's 
   2    1    2 1093 

patient.samples.sample.2.portions.portion.2.portion_number:
   2    6   22 NA's 
   2    2    1 1093 

patient.samples.sample.2.portions.portion.2.portion_sequence:
   2    6 NA's 
   3    2 1093 

patient.samples.sample.2.portions.portion.2.slides.slide.2.bcr_slide_barcode:
tcga-a7-a13e-01b-06-bs6 tcga-a7-a26e-01b-06-bs6                    NA's 
                      1                       1                    1096 

patient.samples.sample.2.portions.portion.2.slides.slide.2.bcr_slide_uuid:
071b5ba3-05a2-46b2-a1d6-80e1f34b33c5 4796c3a1-71ed-4757-b9ea-9fec4e261d30 
                                   1                                    1 
                                NA's 
                                1096 

patient.samples.sample.2.portions.portion.2.slides.slide.2.image_file_name:
tcga-a7-a13e-01b-06-bs6.4796c3a1-71ed-4757-b9ea-9fec4e261d30.svs 
                                                               1 
tcga-a7-a26e-01b-06-bs6.071b5ba3-05a2-46b2-a1d6-80e1f34b33c5.svs 
                                                               1 
                                                            NA's 
                                                            1096 

patient.samples.sample.2.portions.portion.2.slides.slide.2.is_derived_from_ffpe:
 yes NA's 
   2 1096 

patient.samples.sample.2.portions.portion.2.slides.slide.2.percent_lymphocyte_infiltration:
  40 NA's 
   2 1096 

patient.samples.sample.2.portions.portion.2.slides.slide.2.percent_monocyte_infiltration:
  20 NA's 
   2 1096 

patient.samples.sample.2.portions.portion.2.slides.slide.2.percent_necrosis:
   0 NA's 
   2 1096 

patient.samples.sample.2.portions.portion.2.slides.slide.2.percent_neutrophil_infiltration:
  20 NA's 
   2 1096 

patient.samples.sample.2.portions.portion.2.slides.slide.2.percent_normal_cells:
   0 NA's 
   2 1096 

patient.samples.sample.2.portions.portion.2.slides.slide.2.percent_stromal_cells:
  20   30 NA's 
   1    1 1096 

patient.samples.sample.2.portions.portion.2.slides.slide.2.percent_tumor_cells:
  70   80 NA's 
   1    1 1096 

patient.samples.sample.2.portions.portion.2.slides.slide.2.percent_tumor_nuclei:
  70   80 NA's 
   1    1 1096 

patient.samples.sample.2.portions.portion.2.slides.slide.2.section_location:
bottom   NA's 
     2   1096 

patient.samples.sample.2.portions.portion.2.slides.slide.bcr_slide_barcode:
tcga-a7-a13e-01b-05-bs5 tcga-a7-a26e-01b-05-bs5 tcga-e2-a1lh-11a-02-tsb 
                      1                       1                       1 
                   NA's 
                   1095 

patient.samples.sample.2.portions.portion.2.slides.slide.bcr_slide_uuid:
02204231-855b-455e-b21a-b4db37703064 4266663c-c51a-4676-9a2a-6a1e5c909e8e 
                                   1                                    1 
af4878fb-892f-4907-b59c-8aa0e7630a1a                                 NA's 
                                   1                                 1095 

patient.samples.sample.2.portions.portion.2.slides.slide.image_file_name:
tcga-a7-a13e-01b-05-bs5.4266663c-c51a-4676-9a2a-6a1e5c909e8e.svs 
                                                               1 
tcga-a7-a26e-01b-05-bs5.af4878fb-892f-4907-b59c-8aa0e7630a1a.svs 
                                                               1 
tcga-e2-a1lh-11a-02-tsb.02204231-855b-455e-b21a-b4db37703064.svs 
                                                               1 
                                                            NA's 
                                                            1095 

patient.samples.sample.2.portions.portion.2.slides.slide.is_derived_from_ffpe:
  no  yes NA's 
   1    2 1095 

patient.samples.sample.2.portions.portion.2.slides.slide.percent_lymphocyte_infiltration:
   0   40 NA's 
   1    2 1095 

patient.samples.sample.2.portions.portion.2.slides.slide.percent_monocyte_infiltration:
   0   20 NA's 
   1    2 1095 

patient.samples.sample.2.portions.portion.2.slides.slide.percent_necrosis:
   0 NA's 
   3 1095 

patient.samples.sample.2.portions.portion.2.slides.slide.percent_neutrophil_infiltration:
   0   20 NA's 
   1    2 1095 

patient.samples.sample.2.portions.portion.2.slides.slide.percent_normal_cells:
   0   20 NA's 
   2    1 1095 

patient.samples.sample.2.portions.portion.2.slides.slide.percent_stromal_cells:
  20   40   80 NA's 
   1    1    1 1095 

patient.samples.sample.2.portions.portion.2.slides.slide.percent_tumor_cells:
   0   60   80 NA's 
   1    1    1 1095 

patient.samples.sample.2.portions.portion.2.slides.slide.percent_tumor_nuclei:
   0   60   80 NA's 
   1    1    1 1095 

patient.samples.sample.2.portions.portion.2.slides.slide.section_location:
 top NA's 
   3 1095 

patient.samples.sample.2.portions.portion.2.weight:
 100  200  400 NA's 
   1    2    2 1093 

patient.samples.sample.2.portions.portion.2.year_of_creation:
2010 2011 2013 NA's 
   2    1    2 1093 

patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.290   1.755   1.830   1.790   1.860   2.160    1023 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
   3 1095 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
   3 1095 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              7            1091 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 89490662  99037496 100473304 101733663 100485327 153880189       793 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7    8    9   13 NA's 
  61   89  141   14  793 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.5000  0.5000  0.4166  0.5000  0.5000     793 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00    9.00    9.00   16.92   29.00   31.00     793 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no  yes NA's 
 295   10  793 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   6.000   6.443   8.000  11.000     793 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.652   9.000  12.000     793 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.50   40.00   40.00   31.14   40.00   40.00     793 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
 305  793 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   80.00   80.00   66.37   80.00   80.00     793 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 2011 2012 2013 NA's 
 227   63   11    4  793 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-a7-a26j-01b-02r-a46x-36                         NA's 
                           1                         1097 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid:
6e99cc32-5846-4191-bc8a-0656edb0724f                                 NA's 
                                   1                                 1097 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
167814221      NA's 
        1      1097 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
  36 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.16 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
  26 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
 yes NA's 
   1 1097 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   8 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   6 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a46x NA's 
   1 1097 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   b NA's 
   1 1097 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
 0.8 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
   5 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2015 NA's 
   1 1097 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   3 1095 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   3 1095 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.amount:
  80 NA's 
   1 1097 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              7            1091 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97500631 100479047 108432544 113035736 113420338 174165689        38 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8    9   13 NA's 
  14  138  847   61   38 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.130   0.500   0.500   0.476   0.500   0.500      38 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   16.00   16.22   23.00   31.00      38 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no  yes NA's 
1049   10   39 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   6.000   6.144   9.000  11.000      38 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   8.000   7.203   9.000  12.000      38 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.95   40.00   40.00   37.32   40.00   40.00      39 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
1060   38 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   80.00   80.00   75.61   80.00   80.00      39 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014      38 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.33   11.88   18.78   23.11   28.39  102.64    1023 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                    rna 
                   946                     39                     75 
                  NA's 
                    38 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w    x NA's 
  75  946   39   38 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1607  0.1700  0.1800    1023 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
 985  113 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
 985  113 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                            1060 
                                            NA's 
                                              38 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no  yes NA's 
1049   10   39 


patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.300   1.500   1.343   1.700   2.100    1023 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.200   7.350   8.000   7.436   8.600   9.700    1023 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
     75    1023 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2    3    4 NA's 
  29   35   29    4 1001 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 89490643  99036265  99036326  99502440 100480770 100480835       871 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   8    9 NA's 
  90  137  871 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
 227  871 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
   8    9   29 NA's 
  45   92   90  871 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 227  871 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   6    8    9 NA's 
  90   92   45  871 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.872  10.000  12.000     871 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
a026 a054 a073 NA's 
  90   92   45  871 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
  40 NA's 
 227  871 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  23 NA's 
 227  871 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  80 NA's 
 227  871 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
 227  871 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 89490668  99034848 100473916 101244060 100480583 164081813       758 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8    9 NA's 
 137  203  758 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
 340  758 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00    9.00   15.21   29.00   30.00     758 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 340  758 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     6.0     8.0     7.2     9.0    10.0     758 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   8.000   7.282  10.000  12.000     758 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
 6.5   40 NA's 
   1  339  758 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
 340  758 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  13   80 NA's 
   1  339  758 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2010    2010    2010    2014     758 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                   NA's 
                    65                    275                    758 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id:
   w    x NA's 
  65  275  758 



patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
 340  758 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
 340  758 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             340 
                                            NA's 
                                             758 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 340  758 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
       66       183       849 

patient.samples.sample.2.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2    3    4 NA's 
  20   21   21    4 1032 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.480   1.880   1.940   1.927   2.000   2.500     308 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
   2 1096 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
   2 1096 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.amount:
6.67 NA's 
   1 1097 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              5            1093 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99015309  99059842 100489175 112556805 113423288 174191431         5 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    2    9 NA's 
 545   47  501    5 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.050   0.080   0.140   0.121   0.160   0.250       5 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   12.00   18.00   18.93   28.00   31.00       5 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no  yes NA's 
1085    7    6 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.446   7.000  12.000       5 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   8.000   7.274   9.000  12.000       5 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.780   1.070   1.130   1.777   2.800   4.800       6 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
1093    5 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.67   13.00   18.91   40.00   40.00       6 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014       5 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.additional_studies.additional_study.disease_code:
fppp NA's 
   2 1096 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.additional_studies.additional_study.project_code:
tcga NA's 
   2 1096 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.amount:
  26 NA's 
   1 1097 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              5            1093 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99013502  99051448 100488944 104224539 108474412 167787828       284 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000   9.000   7.419   9.000   9.000     284 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0600  0.0800  0.0800  0.0926  0.0900  0.2400     284 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00    9.00   14.00   16.92   23.00   31.00     284 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no  yes NA's 
 806    7  285 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.485   8.000  12.000     284 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   8.000   7.608  10.000  12.000     284 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.87    1.14    1.82    2.34    3.20    7.20     285 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
 814  284 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.34   26.00   27.85   40.00   80.00     285 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014     284 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              4            1094 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 89490286  99038218 100491627 104327738 108474852 153879541       723 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   9.000   6.883   9.000  26.000     723 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0600  0.0800  0.0800  0.1107  0.1500  0.1800     723 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    9.00   14.00   17.09   26.00   31.00     723 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no  yes NA's 
 370    5  723 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.500   8.000   6.875   8.000  11.000     723 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   7.000   6.413   9.000  12.000     723 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.320   1.070   1.950   2.312   4.010   7.530     723 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
 375  723 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   13.00   13.34   21.75   26.70  107.60     723 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2010    2010    2011    2015     723 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99054787 105040717 113388869 114424569 113416465 167811761      1045 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   9 NA's 
  53 1045 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.07 0.08 0.09 NA's 
  13   31    9 1045 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00   19.00   15.53   26.00   26.00    1045 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  53 1045 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   7.000   6.528  11.000  11.000    1045 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   4.000   5.245   7.000  12.000    1045 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.070   2.080   2.800   2.829   3.200   4.680    1045 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
  53 1045 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.40   26.00   40.00   35.35   40.00   52.00    1045 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2011 2013 2015 NA's 
  47    5    1 1045 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-b6-a0i6-10a-01d-a46a-09 tcga-b6-a0re-10a-01d-a46a-09 
                           1                            1 
tcga-bh-a1fc-11a-32d-a314-09                         NA's 
                           1                         1095 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
00b14c77-845c-432e-923c-c2c412f5df31 65957351-2f2c-47c6-b92b-a8e7a78a1364 
                                   1                                    1 
79c31fed-ddf0-4f77-98e7-420feae57046                                 NA's 
                                   1                                 1095 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
153879468 167811762 167811785      NA's 
        1         1         1      1095 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   9 NA's 
   3 1095 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.08 0.09 NA's 
   2    1 1095 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  21   26 NA's 
   2    1 1095 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   3 1095 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   1    6 NA's 
   2    1 1095 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   1    6    7 NA's 
   1    1    1 1095 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
a314 a46a NA's 
   1    2 1095 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   e    g NA's 
   1    2 1095 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
1.07  1.8 NA's 
   2    1 1095 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
   3 1095 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
13.4   20 NA's 
   2    1 1095 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2013 2015 NA's 
   1    2 1095 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   5 1093 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   5 1093 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.amount:
  13 NA's 
   1 1097 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              7            1091 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 99014640 100474571 100492162 112751013 113421586 174191240 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   2   9 
432 654  12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0400  0.1500  0.1600  0.1552  0.1700  0.2500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   12.00   16.00   17.84   25.00   31.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no  yes NA's 
1085   12    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   5.339   7.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   6.000   8.000   7.249   9.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.400   1.070   1.950   1.657   2.080   3.250       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  23 
1098 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.70   13.00   10.78   13.00   56.00       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2013 2014 
 342  496   88  117   55 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.11   14.57   23.14   30.09   37.20  338.66       3 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
 dna 
1098 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
   d 
1098 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0600  0.1500  0.1600  0.1559  0.1700  0.2500 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes 
1098 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes 
1098 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                            1098 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no  yes NA's 
1085   12    1 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type                  allprep ffpe dna 
                              701                                12 
pre-extracted dna received by tss                              NA's 
                              382                                 3 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
      304       794 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1    2    3    4 NA's 
  29   35   29    4 1001 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    9.00   16.00   15.97   23.00   31.00 

patient.samples.sample.2.portions.portion.is_ffpe:
  no  yes NA's 
1085   12    1 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   5.000   6.093   8.000  12.000 

patient.samples.sample.2.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   1.000   3.259   1.000  94.000 

patient.samples.sample.2.portions.portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.199   1.000   9.000     144 




patient.samples.sample.2.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no  yes NA's 
  58   12 1028 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.071   1.000  50.000    1028 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0     0.0     1.2     0.0    50.0    1028 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.8387  0.0000 15.0000    1036 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0     0.0     0.5     0.0    20.0    1028 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    4.00   19.00   45.64  100.00  100.00    1028 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   42.50   47.13   91.50  100.00    1036 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00   11.87    0.00   95.00    1028 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00   15.48    0.00   85.00    1036 

patient.samples.sample.2.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
    12     58   1028 




patient.samples.sample.2.portions.portion.slides.slide.3.is_derived_from_ffpe:
  no NA's 
  37 1061 

patient.samples.sample.2.portions.portion.slides.slide.3.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   6.528   1.000  70.000    1062 

patient.samples.sample.2.portions.portion.slides.slide.3.percent_monocyte_infiltration:
   0    1   10   50 NA's 
  27    6    2    1 1062 

patient.samples.sample.2.portions.portion.slides.slide.3.percent_necrosis:
   0   10 NA's 
  30    3 1065 

patient.samples.sample.2.portions.portion.slides.slide.3.percent_neutrophil_infiltration:
   0   10   20   30 NA's 
  32    2    1    1 1062 

patient.samples.sample.2.portions.portion.slides.slide.3.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   15.00   90.00   61.14  100.00  100.00    1061 

patient.samples.sample.2.portions.portion.slides.slide.3.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   10.00   42.67   88.00  100.00    1065 

patient.samples.sample.2.portions.portion.slides.slide.3.percent_tumor_cells:
   0 NA's 
  37 1061 

patient.samples.sample.2.portions.portion.slides.slide.3.percent_tumor_nuclei:
   0 NA's 
  33 1065 

patient.samples.sample.2.portions.portion.slides.slide.3.section_location:
 top NA's 
  37 1061 




patient.samples.sample.2.portions.portion.slides.slide.4.is_derived_from_ffpe:
  no NA's 
  20 1078 

patient.samples.sample.2.portions.portion.slides.slide.4.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00    9.21    1.00   80.00    1079 

patient.samples.sample.2.portions.portion.slides.slide.4.percent_monocyte_infiltration:
   0    1   10   50 NA's 
  16    1    1    1 1079 

patient.samples.sample.2.portions.portion.slides.slide.4.percent_necrosis:
   0    5   10 NA's 
  14    1    2 1081 

patient.samples.sample.2.portions.portion.slides.slide.4.percent_neutrophil_infiltration:
   0   10   20 NA's 
  16    2    1 1079 

patient.samples.sample.2.portions.portion.slides.slide.4.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    8.75  100.00   69.15  100.00  100.00    1078 

patient.samples.sample.2.portions.portion.slides.slide.4.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00   34.82   95.00  100.00    1081 

patient.samples.sample.2.portions.portion.slides.slide.4.percent_tumor_cells:
   0 NA's 
  20 1078 

patient.samples.sample.2.portions.portion.slides.slide.4.percent_tumor_nuclei:
   0 NA's 
  17 1081 

patient.samples.sample.2.portions.portion.slides.slide.4.section_location:
 top NA's 
  20 1078 




patient.samples.sample.2.portions.portion.slides.slide.5.is_derived_from_ffpe:
  no NA's 
   6 1092 

patient.samples.sample.2.portions.portion.slides.slide.5.percent_lymphocyte_infiltration:
   0    5 NA's 
   5    1 1092 

patient.samples.sample.2.portions.portion.slides.slide.5.percent_monocyte_infiltration:
   0    3 NA's 
   5    1 1092 

patient.samples.sample.2.portions.portion.slides.slide.5.percent_necrosis:
   0 NA's 
   3 1095 

patient.samples.sample.2.portions.portion.slides.slide.5.percent_neutrophil_infiltration:
   0    1 NA's 
   5    1 1092 

patient.samples.sample.2.portions.portion.slides.slide.5.percent_normal_cells:
   3  100 NA's 
   1    5 1092 

patient.samples.sample.2.portions.portion.slides.slide.5.percent_stromal_cells:
   0   97 NA's 
   2    1 1095 

patient.samples.sample.2.portions.portion.slides.slide.5.percent_tumor_cells:
   0 NA's 
   6 1092 

patient.samples.sample.2.portions.portion.slides.slide.5.percent_tumor_nuclei:
   0 NA's 
   3 1095 

patient.samples.sample.2.portions.portion.slides.slide.5.section_location:
 top NA's 
   6 1092 

patient.samples.sample.2.portions.portion.slides.slide.6.bcr_slide_barcode:
tcga-bh-a0dd-11a-05-tse tcga-e2-a1iu-11a-06-tsf                    NA's 
                      1                       1                    1096 

patient.samples.sample.2.portions.portion.slides.slide.6.bcr_slide_uuid:
4a8b32a0-22d7-4262-8139-81ca73b121ff 96609ca3-27d5-47b7-a3b2-9f658b24a26b 
                                   1                                    1 
                                NA's 
                                1096 

patient.samples.sample.2.portions.portion.slides.slide.6.image_file_name:
tcga-bh-a0dd-11a-05-tse.4a8b32a0-22d7-4262-8139-81ca73b121ff.svs 
                                                               1 
tcga-e2-a1iu-11a-06-tsf.96609ca3-27d5-47b7-a3b2-9f658b24a26b.svs 
                                                               1 
                                                            NA's 
                                                            1096 

patient.samples.sample.2.portions.portion.slides.slide.6.is_derived_from_ffpe:
  no NA's 
   2 1096 

patient.samples.sample.2.portions.portion.slides.slide.6.percent_lymphocyte_infiltration:
   0 NA's 
   2 1096 

patient.samples.sample.2.portions.portion.slides.slide.6.percent_monocyte_infiltration:
   0 NA's 
   2 1096 

patient.samples.sample.2.portions.portion.slides.slide.6.percent_neutrophil_infiltration:
   0 NA's 
   2 1096 

patient.samples.sample.2.portions.portion.slides.slide.6.percent_normal_cells:
 100 NA's 
   2 1096 

patient.samples.sample.2.portions.portion.slides.slide.6.percent_tumor_cells:
   0 NA's 
   2 1096 

patient.samples.sample.2.portions.portion.slides.slide.6.section_location:
 top NA's 
   2 1096 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no  yes NA's 
  93   12  993 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00    1.99    2.00   50.00     995 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.4757  0.0000 20.0000     995 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.4731  0.0000 15.0000    1005 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   0.534   0.000  40.000     995 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.75   20.00   47.14  100.00  100.00     994 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   20.00   40.76   90.00  100.00    1005 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00   15.02    0.00  100.00     994 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00   18.12    0.00   95.00    1005 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
bottom    top   NA's 
    12     93    993 

patient.samples.sample.2.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    2.00   92.71  150.00 1000.00     500 

patient.samples.sample.2.portions.portion.year_of_creation:
2010 2011 2012 2013 2014 
 500  362   91  114   31 


patient.samples.sample.2.portions.shipment_portion.center_id:
  20 NA's 
  23 1075 

patient.samples.sample.2.portions.shipment_portion.is_ffpe:
  no NA's 
  23 1075 

patient.samples.sample.2.portions.shipment_portion.plate_id:
a13d a24b a43o NA's 
   2    5   16 1075 

patient.samples.sample.2.portions.shipment_portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  21.00   21.00   41.00   37.52   51.00   71.00    1075 

patient.samples.sample.2.portions.shipment_portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00    3.50    3.50    4.75    6.00    1076 


patient.samples.sample.2.portions.shipment_portion.shipment_portion_day_of_shipment:
  14   22   24 NA's 
   5    2   16 1075 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_month_of_shipment:
   3    6   11 NA's 
   2   16    5 1075 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_year_of_shipment:
2011 2012 2014 NA's 
   2    5   16 1075 

patient.samples.sample.2.sample_type:
blood derived normal           metastatic        primary tumor 
                 993                    7                   15 
 solid tissue normal 
                  83 

patient.samples.sample.2.sample_type_id:
  1   6  10  11 
 15   7 993  83 

patient.samples.sample.2.vial_number:
   a    b    c    d 
1029   57   11    1 

patient.samples.sample.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
germline comparison                NA's 
                  3                1095 



patient.samples.sample.3.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   30.0    98.0   291.0   709.7   983.5  7498.0    1002 

patient.samples.sample.3.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   50.0   200.0   330.0   461.3   640.0  1830.0    1023 

patient.samples.sample.3.is_ffpe:
  no NA's 
  95 1003 

patient.samples.sample.3.oct_embedded:
false  true  NA's 
   51    45  1002 

patient.samples.sample.3.pathology_report_uuid:
2466f145-9621-4bd7-9b3f-04f0f810df94                                 NA's 
                                   1                                 1097 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-a7-a0db-10a-02d-a272-09 tcga-a7-a13e-10a-02d-a272-09 
                           1                            1 
                        NA's 
                        1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
9bd41c60-2384-4ea1-99a2-cc4a8ebf19cc b9e04a17-821c-4e9f-a161-459e74727817 
                                   1                                    1 
                                NA's 
                                1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2            1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
132158804 132158821      NA's 
        1         1      1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.center_id:
   9 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.concentration:
0.08 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.day_of_shipment:
  13 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.month_of_shipment:
   3 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_column:
   7 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_id:
a272 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_row:
   c    d NA's 
   1    1 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.quantity:
 1.6 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.volume:
  20 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.year_of_shipment:
2013 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.amount:
38.97    89  NA's 
    1     1  1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.analyte_type:
 dna NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.analyte_type_id:
   d NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.bcr_analyte_barcode:
tcga-a7-a0db-10a-02d tcga-a7-a13e-10a-02d                 NA's 
                   1                    1                 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.bcr_analyte_uuid:
2628fae2-d81e-4543-865b-4f8435c57ab9 8af8832c-f078-45d9-9112-92490caadbc0 
                                   1                                    1 
                                NA's 
                                1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.concentration:
0.15 0.16 NA's 
   1    1 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                            1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    2                  1096 

patient.samples.sample.3.portions.portion.2.analytes.analyte.spectrophotometer_method:
picogreen      NA's 
        2      1096 

patient.samples.sample.3.portions.portion.2.bcr_portion_barcode:
tcga-a7-a0db-10a-02 tcga-a7-a13e-10a-02                NA's 
                  1                   1                1096 

patient.samples.sample.3.portions.portion.2.bcr_portion_uuid:
2f71ade7-f6fe-4266-b22a-61aac78b3e66 fe97eba0-cb01-4bcc-9d94-df6948897ec2 
                                   1                                    1 
                                NA's 
                                1096 

patient.samples.sample.3.portions.portion.2.day_of_creation:
  27 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.is_ffpe:
  no NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.month_of_creation:
   2 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.portion_number:
   2 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.portion_sequence:
   2 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.weight:
   2 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.2.year_of_creation:
2013 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.380   1.750   1.790   1.743   1.830   2.100    1045 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.amount:
26.7 NA's 
   1 1097 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 94929329  99034272 100471717 102346801 100477333 167812914      1048 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7    8   13 NA's 
  32    2   16 1048 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1696  0.1675  0.5000    1048 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   12.00   22.50   20.48   29.00   31.00    1048 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  49 1049 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    5.00    5.52    9.00   12.00    1048 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    5.00    4.66    6.00   11.00    1048 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.860   3.200   4.010   5.239   4.270  40.000    1049 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
  50 1048 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
 6.6   20 26.7   80 NA's 
   1   16   30    2 1049 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 2011 2014 NA's 
  19   30    1 1048 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-bh-a0ba-11a-22r-a19e-07                         NA's 
                           1                         1097 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid:
3bfd1811-bd2a-439d-8dde-237abcf855af                                 NA's 
                                   1                                 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
113388773      NA's 
        1      1097 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.16 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
  25 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
  10 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   1 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a19e NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   a NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
2.08 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
  13 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2011 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.amount:
  20 NA's 
   1 1097 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99014248 100471174 100477173 108275995 108455182 167804413      1011 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    9   13 NA's 
  22   34   31 1011 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.1500  0.1700  0.2887  0.5000  0.5000    1011 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    8.00   22.00   18.63   29.00   31.00    1011 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  86 1012 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.494   9.000  12.000    1011 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   5.828   7.500  12.000    1011 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.67    3.20    4.01   17.92   40.00   40.00    1012 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
  87 1011 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  20 26.7   80 NA's 
  30   22   34 1012 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014    1011 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.47   10.19   15.78   27.95   23.40  214.20    1045 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                  34                   53                 1011 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
  53   34 1011 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.1500  0.1600  0.1532  0.1600  0.1800    1045 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
  34 1064 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
  34 1064 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              87 
                                            NA's 
                                            1011 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
  86 1012 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
   allprep rna extraction mirvana (allprep dna) rna                   repli-g 
                        2                        51                        32 
                     NA's 
                     1013 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.100   1.300   1.500   1.494   1.700   2.100    1045 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  6.500   7.500   8.100   8.077   8.600   9.600    1045 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
     53    1045 

patient.samples.sample.3.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2    3    4 NA's 
  17   23   24    4 1030 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-a7-a0db-10a-01x-a026-08 tcga-a7-a0dc-10a-01x-a026-08 
                           1                            1 
                        NA's 
                        1096 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_uuid:
0e4b4a82-57f1-4b8d-bc9c-fdc381437614 1359d258-7be3-4750-b776-b794ac2c7706 
                                   1                                    1 
                                NA's 
                                1096 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
99035771 99035826     NA's 
       1        1     1096 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   8 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
  29 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   2 1096 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   6 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
  11 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
a026 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_row:
   c    d NA's 
   1    1 1096 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
  40 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  23 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  80 NA's 
   2 1096 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
   2 1096 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99012875  99022646 100471230 101673987 100479098 108433023      1060 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   9 NA's 
  38 1060 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
  38 1060 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    8.00   12.00   13.58   23.00   29.00    1060 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  38 1060 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.000  10.000   7.421  10.000  11.000    1060 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.250   5.500   5.895  11.000  12.000    1060 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
 6.5   40 NA's 
  15   23 1060 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
  38 1060 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  13   80 NA's 
  15   23 1060 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
  24   14 1060 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                   NA's 
                    36                      2                   1060 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id:
   w    x NA's 
  36    2 1060 



patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
  38 1060 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
  38 1060 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              38 
                                            NA's 
                                            1060 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
  38 1060 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
     36    1062 

patient.samples.sample.3.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2    3    4 NA's 
   9   16    9    2 1062 



patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99013060  99016418  99050855  99554705 100471201 100489924      1076 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   9 NA's 
  22 1076 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
  22 1076 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   8   12 NA's 
   7   15 1076 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  22 1076 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
  10   11 NA's 
  15    7 1076 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     2.0     2.5     4.0     5.0    12.0    1076 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id:
a100 a10f NA's 
  15    7 1076 


patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
 6.5   40 NA's 
  15    7 1076 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  23 NA's 
  22 1076 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  13   80 NA's 
  15    7 1076 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2010 NA's 
  22 1076 

patient.samples.sample.3.portions.portion.analytes.analyte.4.analyte_type:
repli-g x (qiagen) dna                   NA's 
                    22                   1076 

patient.samples.sample.3.portions.portion.analytes.analyte.4.analyte_type_id:
   x NA's 
  22 1076 



patient.samples.sample.3.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
  22 1076 

patient.samples.sample.3.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
 yes NA's 
  22 1076 

patient.samples.sample.3.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              22 
                                            NA's 
                                            1076 

patient.samples.sample.3.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
  22 1076 

patient.samples.sample.3.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g x      NA's 
       22      1076 

patient.samples.sample.3.portions.portion.analytes.analyte.4.subportion_sequence:
   1    2    3 NA's 
   7    8    7 1076 

patient.samples.sample.3.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.590   1.890   1.935   1.945   1.995   2.440    1016 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.amount:
6.67 NA's 
   1 1097 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
germline comparison                NA's 
                  1                1097 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99015308 100471156 100489952 109568891 108477522 174189221      1025 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    5    7    9 NA's 
  42    6    1   24 1025 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.1500  0.1307  0.1600  0.1800    1025 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   12.00   25.00   21.37   29.00   31.00    1025 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  72 1026 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   4.863   9.000  12.000    1025 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.356  11.000  12.000    1025 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.070   1.130   1.858   2.800   4.810    1026 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
  73 1025 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.67    6.67   17.51   26.70   40.00    1026 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014    1025 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.amount:
  26 NA's 
   1 1097 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 89490684  99017983 100471213 102044182 106446222 113423695      1040 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   1    5    8    9 NA's 
   8   16    2   32 1040 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.070   0.080   0.085   0.111   0.150   0.170    1040 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00    8.00   22.00   18.98   29.00   31.00    1040 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  57 1041 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   5.328   9.000  12.000    1040 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.250   6.000   5.845  11.000  12.000    1040 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   2.080   3.600   3.445   4.540   6.400    1041 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
  58 1040 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   26.00   26.70   35.11   40.00   80.00    1041 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 NA's 
  25   33 1040 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.additional_studies.additional_study.disease_code:
fppp NA's 
   3 1095 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.additional_studies.additional_study.project_code:
tcga NA's 
   3 1095 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.amount:
26.7 NA's 
   1 1097 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3            1095 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99015292 100119968 100488842 105009798 108431634 153879469      1050 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   2    5    9 NA's 
   1   25   22 1050 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.1400  0.1208  0.1525  0.1800    1050 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   12.00   12.00   13.38   13.25   31.00    1050 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  47 1051 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.500   6.062  10.000  12.000    1050 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.750   6.500   7.125  11.250  12.000    1050 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   2.080   3.470   3.112   4.010   4.810    1051 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
  48 1050 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   26.00   26.70   25.83   26.70   52.00    1051 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 2013 NA's 
  21   23    4 1050 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.disease_code:
fppp NA's 
   2 1096 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.project_code:
tcga NA's 
   2 1096 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2            1096 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100476630 100476651 100476680 108045746 100476740 153879494      1079 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   1    5    9 NA's 
   2   15    2 1079 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0800  0.1500  0.1600  0.1505  0.1650  0.1800    1079 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  14   20   26 NA's 
   2   15    2 1079 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  19 1079 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   1    3    6   12 NA's 
   1    2    1   15 1079 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   7.000   8.000   7.895   9.000  12.000    1079 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a10q a128 a275 a314 NA's 
  15    1    2    1 1079 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.010   2.210   4.010   3.302   4.270   4.540    1079 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
  19 1079 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   16.50   26.70   22.46   26.70   40.00    1079 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2011 2013 NA's 
  15    1    3 1079 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-a7-a0ce-11a-21d-a12l-09 tcga-a7-a26j-10a-01d-a46w-08 
                           1                            1 
tcga-bh-a0b3-11b-21d-a128-09                         NA's 
                           1                         1095 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
4c51f446-5d3a-4af1-b10e-c9b2e1d086aa ce23b4fb-a727-4ccb-b4d7-c2e8407abd7e 
                                   1                                    1 
efcab56b-6f30-4443-801d-4bb8e30ccad5                                 NA's 
                                   1                                 1095 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
105041459 108431795 145674288      NA's 
        1         1         1      1095 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   8    9 NA's 
   1    2 1095 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.08 0.09 NA's 
   2    1 1095 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   3   15   26 NA's 
   1    1    1 1095 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   3 1095 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   1    2    8 NA's 
   1    1    1 1095 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   1    8   12 NA's 
   1    1    1 1095 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
a128 a12l a46w NA's 
   1    1    1 1095 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   a    b NA's 
   1    2 1095 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
 1.8  3.2 NA's 
   1    2 1095 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
   3 1095 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
  20   40 NA's 
   1    2 1095 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2011 2015 NA's 
   2    1 1095 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-a7-a0ce-11a-21d-a148-05                         NA's 
                           1                         1097 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid:
805f2c82-49d5-4994-a2a5-98dd1b832f0b                                 NA's 
                                   1                                 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
99027795     NA's 
       1     1097 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   5 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
0.17 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
  13 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   4 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   3 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id:
a148 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_row:
   b NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
11.34  NA's 
    1  1097 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  23 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
66.7 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2011 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.amount:
  13 NA's 
   1 1097 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
germline comparison                NA's 
                  1                1097 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99014649 100120254 100488584 107889314 108475066 174191241      1002 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   2.688   2.750  13.000    1002 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0800  0.1500  0.1600  0.1568  0.1700  0.1800    1002 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   11.00   22.00   19.82   29.00   31.00    1002 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  95 1003 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.677   9.000  12.000    1002 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.000   6.406   8.000  12.000    1002 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.755   2.080   2.381   2.770   4.810    1003 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
  96 1002 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.00   13.00   15.21   16.50   40.00    1003 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014    1002 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.25   11.94   19.09   27.31   32.28  210.45    1002 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna  rna NA's 
  95    1 1002 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d    r NA's 
  95    1 1002 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1583  0.1700  0.1800    1002 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
  95 1003 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
  95 1003 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              96 
                                            NA's 
                                            1002 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
  95 1003 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type         mirvana (allprep dna) rna 
                               88                                 1 
pre-extracted dna received by tss                              NA's 
                                5                              1004 

patient.samples.sample.3.portions.portion.analytes.analyte.rna.ratio_28s_18s:
 1.3 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.rna.rinvalue:
 8.2 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
       14        82      1002 

patient.samples.sample.3.portions.portion.analytes.analyte.subportion_sequence:
   1    2    3    4 NA's 
  20   26   25    4 1023 



patient.samples.sample.3.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   10.00   17.00   16.33   23.25   30.00    1002 

patient.samples.sample.3.portions.portion.is_ffpe:
  no NA's 
  95 1003 

patient.samples.sample.3.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.792   8.000  12.000    1002 

patient.samples.sample.3.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   11.75   22.00   24.46   33.00   73.00    1002 

patient.samples.sample.3.portions.portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.554   3.000   7.000    1006 




patient.samples.sample.3.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
  59 1039 

patient.samples.sample.3.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   2.138   2.000  20.000    1040 

patient.samples.sample.3.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.3276  0.0000  5.0000    1040 

patient.samples.sample.3.portions.portion.slides.slide.2.percent_necrosis:
   0 NA's 
  43 1055 

patient.samples.sample.3.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   0    1    5 NA's 
  56    1    1 1040 

patient.samples.sample.3.portions.portion.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   11.50  100.00   68.23  100.00  100.00    1038 

patient.samples.sample.3.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00   43.28   96.00  100.00    1055 

patient.samples.sample.3.portions.portion.slides.slide.2.percent_tumor_cells:
   0  100 NA's 
  59    1 1038 

patient.samples.sample.3.portions.portion.slides.slide.2.percent_tumor_nuclei:
   0 NA's 
  43 1055 

patient.samples.sample.3.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
     2     58   1038 




patient.samples.sample.3.portions.portion.slides.slide.3.is_derived_from_ffpe:
  no NA's 
  32 1066 

patient.samples.sample.3.portions.portion.slides.slide.3.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.8333  1.7500  5.0000    1068 

patient.samples.sample.3.portions.portion.slides.slide.3.percent_monocyte_infiltration:
   0    1    2 NA's 
  24    4    2 1068 

patient.samples.sample.3.portions.portion.slides.slide.3.percent_necrosis:
   0 NA's 
  30 1068 

patient.samples.sample.3.portions.portion.slides.slide.3.percent_neutrophil_infiltration:
   0    1 NA's 
  27    3 1068 

patient.samples.sample.3.portions.portion.slides.slide.3.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.00   20.00   48.12  100.00  100.00    1066 

patient.samples.sample.3.portions.portion.slides.slide.3.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   82.50   55.33   98.00  100.00    1068 

patient.samples.sample.3.portions.portion.slides.slide.3.percent_tumor_cells:
   0 NA's 
  32 1066 

patient.samples.sample.3.portions.portion.slides.slide.3.percent_tumor_nuclei:
   0 NA's 
  30 1068 

patient.samples.sample.3.portions.portion.slides.slide.3.section_location:
 top NA's 
  32 1066 




patient.samples.sample.3.portions.portion.slides.slide.4.is_derived_from_ffpe:
  no NA's 
  17 1081 

patient.samples.sample.3.portions.portion.slides.slide.4.percent_lymphocyte_infiltration:
   0    1    2 NA's 
   8    6    1 1083 

patient.samples.sample.3.portions.portion.slides.slide.4.percent_monocyte_infiltration:
   0    1 NA's 
  14    1 1083 

patient.samples.sample.3.portions.portion.slides.slide.4.percent_necrosis:
   0 NA's 
  15 1083 

patient.samples.sample.3.portions.portion.slides.slide.4.percent_neutrophil_infiltration:
   0 NA's 
  15 1083 

patient.samples.sample.3.portions.portion.slides.slide.4.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    3.00   15.00   49.47  100.00  100.00    1081 

patient.samples.sample.3.portions.portion.slides.slide.4.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   90.00   57.27   98.50  100.00    1083 

patient.samples.sample.3.portions.portion.slides.slide.4.percent_tumor_cells:
   0 NA's 
  17 1081 

patient.samples.sample.3.portions.portion.slides.slide.4.percent_tumor_nuclei:
   0 NA's 
  15 1083 

patient.samples.sample.3.portions.portion.slides.slide.4.section_location:
bottom    top   NA's 
     2     15   1081 




patient.samples.sample.3.portions.portion.slides.slide.5.is_derived_from_ffpe:
  no NA's 
   6 1092 

patient.samples.sample.3.portions.portion.slides.slide.5.percent_lymphocyte_infiltration:
   0    1 NA's 
   2    3 1093 

patient.samples.sample.3.portions.portion.slides.slide.5.percent_monocyte_infiltration:
   0 NA's 
   5 1093 

patient.samples.sample.3.portions.portion.slides.slide.5.percent_necrosis:
   0 NA's 
   5 1093 

patient.samples.sample.3.portions.portion.slides.slide.5.percent_neutrophil_infiltration:
   0 NA's 
   5 1093 

patient.samples.sample.3.portions.portion.slides.slide.5.percent_normal_cells:
   0  100 NA's 
   1    5 1092 

patient.samples.sample.3.portions.portion.slides.slide.5.percent_stromal_cells:
   0  100 NA's 
   4    1 1093 

patient.samples.sample.3.portions.portion.slides.slide.5.percent_tumor_cells:
   0 NA's 
   6 1092 

patient.samples.sample.3.portions.portion.slides.slide.5.percent_tumor_nuclei:
   0 NA's 
   5 1093 

patient.samples.sample.3.portions.portion.slides.slide.5.section_location:
 top NA's 
   6 1092 

patient.samples.sample.3.portions.portion.slides.slide.6.bcr_slide_barcode:
tcga-a7-a4se-11a-06-ts6                    NA's 
                      1                    1097 

patient.samples.sample.3.portions.portion.slides.slide.6.bcr_slide_uuid:
eb9f7edd-e1b2-482e-b411-1ceea71502b3                                 NA's 
                                   1                                 1097 

patient.samples.sample.3.portions.portion.slides.slide.6.image_file_name:
tcga-a7-a4se-11a-06-ts6.eb9f7edd-e1b2-482e-b411-1ceea71502b3.svs 
                                                               1 
                                                            NA's 
                                                            1097 

patient.samples.sample.3.portions.portion.slides.slide.6.is_derived_from_ffpe:
  no NA's 
   1 1097 

patient.samples.sample.3.portions.portion.slides.slide.6.percent_lymphocyte_infiltration:
   0 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.slides.slide.6.percent_monocyte_infiltration:
   0 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.slides.slide.6.percent_necrosis:
   0 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.slides.slide.6.percent_neutrophil_infiltration:
   0 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.slides.slide.6.percent_normal_cells:
   0 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.slides.slide.6.percent_stromal_cells:
 100 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.slides.slide.6.percent_tumor_cells:
   0 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.slides.slide.6.percent_tumor_nuclei:
   0 NA's 
   1 1097 

patient.samples.sample.3.portions.portion.slides.slide.6.section_location:
 top NA's 
   1 1097 




patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  75 1023 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   3.286   1.750  70.000    1028 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.3857  0.0000 10.0000    1028 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
  52 1046 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0    1   10 NA's 
  67    2    1 1028 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.50  100.00   60.63  100.00  100.00    1027 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   72.50   49.92   99.25  100.00    1046 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells:
   0  100 NA's 
  71    1 1026 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei:
   0 NA's 
  52 1046 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
bottom    top   NA's 
    19     57   1022 

patient.samples.sample.3.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.5    20.0    80.0   118.2   230.0   300.0    1010 

patient.samples.sample.3.portions.portion.year_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014    1002 


patient.samples.sample.3.portions.shipment_portion.center_id:
  20 NA's 
  28 1070 

patient.samples.sample.3.portions.shipment_portion.is_ffpe:
  no NA's 
  28 1070 

patient.samples.sample.3.portions.shipment_portion.plate_id:
a43o NA's 
  28 1070 

patient.samples.sample.3.portions.shipment_portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  21.00   41.00   51.00   46.36   51.00   81.00    1070 

patient.samples.sample.3.portions.shipment_portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   4.536   5.000   8.000    1070 


patient.samples.sample.3.portions.shipment_portion.shipment_portion_day_of_shipment:
  24 NA's 
  28 1070 

patient.samples.sample.3.portions.shipment_portion.shipment_portion_month_of_shipment:
   6 NA's 
  28 1070 

patient.samples.sample.3.portions.shipment_portion.shipment_portion_year_of_shipment:
2014 NA's 
  28 1070 

patient.samples.sample.3.sample_type:
blood derived normal  solid tissue normal                 NA's 
                  20                   76                 1002 

patient.samples.sample.3.sample_type_id:
  10   11 NA's 
  20   76 1002 

patient.samples.sample.3.vial_number:
   a    b NA's 
  92    4 1002 

patient.samples.sample.4.bcr_sample_barcode:
tcga-a7-a0db-11a tcga-a7-a0dc-11a tcga-a7-a13e-11a tcga-e2-a15k-11a 
               1                1                1                1 
            NA's 
            1094 

patient.samples.sample.4.bcr_sample_uuid:
1eb11f10-2aca-4ca1-b6fc-bbc97cc485fd 238e4064-4aac-4c75-a506-b19bc7f3fc3c 
                                   1                                    1 
5bfd69f5-406a-4b28-b4f4-a6655e03e4db e2566ada-c2bf-4c25-bcd0-9f49f2d0b4a2 
                                   1                                    1 
                                NA's 
                                1094 

patient.samples.sample.4.days_to_collection:
 167  220  238  309 NA's 
   1    1    1    1 1094 

patient.samples.sample.4.initial_weight:
 120  400  470  770 NA's 
   1    1    1    1 1094 

patient.samples.sample.4.is_ffpe:
  no NA's 
   4 1094 

patient.samples.sample.4.oct_embedded:
false  NA's 
    4  1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.a260_a280_ratio:
1.77 1.82 1.86 1.89 NA's 
   1    1    1    1 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-a7-a0db-11a-33r-a090-13 tcga-a7-a0dc-11a-41r-a090-13 
                           1                            1 
tcga-a7-a13e-11a-61r-a12p-07 tcga-e2-a15k-11a-13r-a12p-07 
                           1                            1 
                        NA's 
                        1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
01d11e4c-038d-4408-93dd-f714b6489866 49a33b4d-4b1e-4b30-b0ba-a7c7d594db41 
                                   1                                    1 
80efdbaa-067b-4018-89b2-b8869bfb9163 9458ec78-c1cd-45a2-b718-955ad1237cac 
                                   1                                    1 
                                NA's 
                                1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
 94929380  99027340 100471204 108432005      NA's 
        1         1         1         1      1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.13 0.15 0.16 0.17 NA's 
   1    1    1    1 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  22   29 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   2    9 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   2    6    8 NA's 
   2    1    1 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
a090 a12p NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   a    b    c NA's 
   1    2    1 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   3  3.2 3.47 4.54 NA's 
   1    1    1    1 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  20 26.7 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 2011 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-a7-a0db-11a-33r-a089-07 tcga-a7-a0dc-11a-41r-a089-07 
                           1                            1 
tcga-a7-a13e-11a-61r-a12o-13 tcga-e2-a15k-11a-13r-a12o-13 
                           1                            1 
                        NA's 
                        1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
3f3bdfbb-3490-42e4-9511-9df4b8a126a3 bec26d26-258e-4ba0-9956-5402096e580b 
                                   1                                    1 
cb3f974e-6bce-4757-a350-aef8e7925086 cd560004-50be-4056-be0b-3510c7029491 
                                   1                                    1 
                                NA's 
                                1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
 99036533 100489402 100491458 108431381      NA's 
        1         1         1         1      1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7   13 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.13 0.15 0.16 0.17 NA's 
   1    1    1    1 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
  22   29 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   2    9 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   2    6    8 NA's 
   2    1    1 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
a089 a12o NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_row:
   a    b    c NA's 
   1    2    1 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
 2.6  3.4 4.01 4.27 NA's 
   1    1    1    1 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  20 26.7 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.amount:
 7.04  13.1 17.74  22.8  NA's 
    1     1     1     1  1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type:
 rna NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type_id:
   r NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.bcr_analyte_barcode:
tcga-a7-a0db-11a-33r tcga-a7-a0dc-11a-41r tcga-a7-a13e-11a-61r 
                   1                    1                    1 
tcga-e2-a15k-11a-13r                 NA's 
                   1                 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.bcr_analyte_uuid:
136955c1-9e77-4f4d-b1be-22ba9c238069 a669a954-3d19-428a-b11a-617dcc00137a 
                                   1                                    1 
d748b6f4-b13c-427e-a330-f341a5151c7c f58413d7-88d1-44d4-9076-f4bec4ba8f28 
                                   1                                    1 
                                NA's 
                                1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.concentration:
0.13 0.15 0.16 0.17 NA's 
   1    1    1    1 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                            1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                      NA's 
                        4                      1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
 1.5  1.6 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.rna.rinvalue:
 7.1  7.2  7.9 NA's 
   1    1    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      4    1094 

patient.samples.sample.4.portions.portion.analytes.analyte.2.subportion_sequence:
   1    3 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-a7-a0db-11a-33w-a100-09 tcga-a7-a0dc-11a-41w-a100-09 
                           1                            1 
tcga-a7-a13e-11a-61w-a14o-09 tcga-e2-a15k-11a-13w-a14o-09 
                           1                            1 
                        NA's 
                        1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
1ac6ee06-5e18-4757-bb55-94b8303af335 adc233b1-6236-4994-8fdd-f6e25087877d 
                                   1                                    1 
edde29ab-1a7a-4be0-870c-e4256460754e fa389599-4fc1-494a-a28c-004725235ac8 
                                   1                                    1 
                                NA's 
                                1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
 99026267  99027163 100476718 100476755      NA's 
        1         1         1         1      1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   9 NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   3   12 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   5   10 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   9   10   12 NA's 
   1    1    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
a100 a14o NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   a    c    e NA's 
   2    1    1 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
 6.5   40 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  13   80 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                   4                 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.bcr_analyte_barcode:
tcga-a7-a0db-11a-33w tcga-a7-a0dc-11a-41w tcga-a7-a13e-11a-61w 
                   1                    1                    1 
tcga-e2-a15k-11a-13w                 NA's 
                   1                 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.bcr_analyte_uuid:
0c696c3d-023b-4980-9b9c-74a3d5f4fb77 0fba8413-539b-4e55-b663-1acd3d8bab3b 
                                   1                                    1 
160c89c6-b3c1-4ac6-98f9-10506c8d0260 ab4287c8-57ac-48b0-b3ad-d83073e91e40 
                                   1                                    1 
                                NA's 
                                1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                            1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      4    1094 

patient.samples.sample.4.portions.portion.analytes.analyte.3.subportion_sequence:
   1    3 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_barcode:
tcga-a7-a0db-11a-33x-a100-09 tcga-a7-a0dc-11a-41x-a100-09 
                           1                            1 
                        NA's 
                        1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_uuid:
eda887db-1059-44d3-a550-ed4f37c8c2c2 f44e6b5d-ecef-4424-b160-45c5f0d76939 
                                   1                                    1 
                                NA's 
                                1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
99027132 99066293     NA's 
       1        1     1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   9 NA's 
   2 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   2 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
  12 NA's 
   2 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
  10 NA's 
   2 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   9   10 NA's 
   1    1 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id:
a100 NA's 
   2 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_row:
   c NA's 
   2 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
 6.5 NA's 
   2 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  23 NA's 
   2 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  13 NA's 
   2 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2010 NA's 
   2 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.analyte_type:
repli-g x (qiagen) dna                   NA's 
                     2                   1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.analyte_type_id:
   x NA's 
   2 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.bcr_analyte_barcode:
tcga-a7-a0db-11a-33x tcga-a7-a0dc-11a-41x                 NA's 
                   1                    1                 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.bcr_analyte_uuid:
61c94a42-e5ac-4c39-8d40-fa252ee25565 c2b8dd40-0004-43cb-8328-34acee48e72c 
                                   1                                    1 
                                NA's 
                                1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   2 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
 yes NA's 
   2 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                            1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   2 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g x      NA's 
        2      1096 

patient.samples.sample.4.portions.portion.analytes.analyte.4.subportion_sequence:
   1    3 NA's 
   1    1 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.a260_a280_ratio:
1.91 1.93 1.98 2.05 NA's 
   1    1    1    1 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-a7-a0db-11a-33d-a092-01 tcga-a7-a0dc-11a-41d-a092-01 
                           1                            1 
tcga-a7-a13e-11a-61d-a12n-01 tcga-e2-a15k-11a-13d-a12n-01 
                           1                            1 
                        NA's 
                        1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
23ec1c8f-3d33-4e04-94df-271a9efe8a63 3480e631-b2bc-479a-9701-8da2ce30bd96 
                                   1                                    1 
b6e2f21b-1b95-4bff-a3aa-3883562c958a fd203984-44aa-49e0-ae0c-11643697d937 
                                   1                                    1 
                                NA's 
                                1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
 99029336 100489889 108431268 108431292      NA's 
        1         1         1         1      1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1 NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.15 0.16 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  22   29 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   2    9 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   2   12 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a092 a12n NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_row:
   a    b    c    e NA's 
   1    1    1    1 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   1 1.07 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
6.67 NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-a7-a0db-11a-33d-a093-05 tcga-a7-a0dc-11a-41d-a093-05 
                           1                            1 
tcga-a7-a13e-11a-61d-a12q-09 tcga-e2-a15k-11a-13d-a12q-09 
                           1                            1 
                        NA's 
                        1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
248bb641-7eef-46f5-ae81-5e9e626eccc9 26163687-7ede-43be-aeb7-21ad33133d45 
                                   1                                    1 
7b565538-7488-4654-9034-8a0e6fe65bf1 cc1f28ba-f095-4859-b8c6-920fb9f50d65 
                                   1                                    1 
                                NA's 
                                1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
100471568 100472100 108431844 108431868      NA's 
        1         1         1         1      1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5    9 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.08 0.15 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  22   29 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   2    9 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   2   12 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
a093 a12q NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_row:
   a    b    c    e NA's 
   1    1    1    1 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
 3.2 4.01 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
26.7   40 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-a7-a0db-11a-33d-a100-09 tcga-a7-a0dc-11a-41d-a100-09 
                           1                            1 
tcga-a7-a13e-11a-61d-a12r-05 tcga-e2-a15k-11a-13d-a12r-05 
                           1                            1 
                        NA's 
                        1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
1362c1ad-7658-49a2-b553-3ea94e1752cf 2e82d800-d0b7-4c0c-9271-9ac0852b6daa 
                                   1                                    1 
a9b112b9-e18b-4f7e-8bf4-132e41a7c568 f7315045-b78d-49b0-87a5-36e24fea92b5 
                                   1                                    1 
                                NA's 
                                1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
 99051415 100470380 108431652 108431676      NA's 
        1         1         1         1      1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   5    9 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.08 0.16 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  12   22 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   2   10 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   9   10   12 NA's 
   1    1    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
a100 a12r NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_row:
   c    e NA's 
   1    3 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
2.08 4.27 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  26 26.7 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-a7-a0dc-11a-41d-a10q-05 tcga-e2-a15k-11a-13d-a314-09 
                           1                            1 
                        NA's 
                        1096 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
36da685d-df49-495c-9200-28d5ee683604 b03d0f6b-8fa6-441b-aed6-9223d1975c7a 
                                   1                                    1 
                                NA's 
                                1096 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
100476656 153879470      NA's 
        1         1      1096 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   5    9 NA's 
   1    1 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.08 0.15 NA's 
   1    1 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  20   26 NA's 
   1    1 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   2 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   6   12 NA's 
   1    1 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   3    7 NA's 
   1    1 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a10q a314 NA's 
   1    1 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row:
   e    f NA's 
   1    1 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
0.96 4.01 NA's 
   1    1 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   2 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
  12 26.7 NA's 
   1    1 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2013 NA's 
   1    1 1096 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-a7-a0db-11a-33d-a091-02 tcga-a7-a0dc-11a-41d-a091-02 
                           1                            1 
tcga-a7-a13e-11a-61d-a12m-02 tcga-e2-a15k-11a-13d-a12m-02 
                           1                            1 
                        NA's 
                        1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
3a1cef18-0116-4cd1-9a20-646cdc9d3022 5bc539a0-e055-4d80-a6d0-b200d4489cbe 
                                   1                                    1 
79bc27fa-f40b-411a-bdc2-29e3a24110ea 832d7423-b532-43d0-87cb-2b9f396ed4f7 
                                   1                                    1 
                                NA's 
                                1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
100471583 100489907 108431364 108431388      NA's 
        1         1         1         1      1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   2 NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.15 0.16 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
  22   29 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   2    9 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   2   12 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a091 a12m NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_row:
   a    b    c    e NA's 
   1    1    1    1 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
1.95 2.08 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.volume:
  13 NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.amount:
 11.7 14.72 15.55 17.94  NA's 
    1     1     1     1  1094 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_barcode:
tcga-a7-a0db-11a-33d tcga-a7-a0dc-11a-41d tcga-a7-a13e-11a-61d 
                   1                    1                    1 
tcga-e2-a15k-11a-13d                 NA's 
                   1                 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_uuid:
159f3600-a388-4b9f-a5e9-324180ef2d70 387efbad-4b98-4ae5-85a5-4a312c3ef1ad 
                                   1                                    1 
392ef4f5-1183-4c8b-b719-15d3868e039d 53061db7-1900-4f28-9af7-376790fe245c 
                                   1                                    1 
                                NA's 
                                1094 

patient.samples.sample.4.portions.portion.analytes.analyte.concentration:
0.15 0.16 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                            1094 

patient.samples.sample.4.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   4 1094 

patient.samples.sample.4.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    4                  1094 

patient.samples.sample.4.portions.portion.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      4    1094 

patient.samples.sample.4.portions.portion.analytes.analyte.subportion_sequence:
   1    3 NA's 
   2    2 1094 

patient.samples.sample.4.portions.portion.bcr_portion_barcode:
tcga-a7-a0db-11a-33 tcga-a7-a0dc-11a-41 tcga-a7-a13e-11a-61 tcga-e2-a15k-11a-13 
                  1                   1                   1                   1 
               NA's 
               1094 

patient.samples.sample.4.portions.portion.bcr_portion_uuid:
14224bef-b6d3-4b9a-9c40-5673e6005459 201ed5d0-a472-4a76-99b6-04dc0b49e62e 
                                   1                                    1 
52d3f394-ee72-43b0-baa2-feb465bbcc61 8c648887-8495-42d3-bd65-9336af19068e 
                                   1                                    1 
                                NA's 
                                1094 

patient.samples.sample.4.portions.portion.day_of_creation:
  11   13   27 NA's 
   1    1    2 1094 

patient.samples.sample.4.portions.portion.is_ffpe:
  no NA's 
   4 1094 

patient.samples.sample.4.portions.portion.month_of_creation:
   1    2    8 NA's 
   1    1    2 1094 

patient.samples.sample.4.portions.portion.portion_number:
  13   33   41   61 NA's 
   1    1    1    1 1094 

patient.samples.sample.4.portions.portion.portion_sequence:
   1    3    4    6 NA's 
   1    1    1    1 1094 

patient.samples.sample.4.portions.portion.slides.slide.2.bcr_slide_barcode:
tcga-a7-a0db-11a-03-ts3                    NA's 
                      1                    1097 

patient.samples.sample.4.portions.portion.slides.slide.2.bcr_slide_uuid:
0a4e3580-9711-488f-8a18-54f72c429035                                 NA's 
                                   1                                 1097 

patient.samples.sample.4.portions.portion.slides.slide.2.image_file_name:
tcga-a7-a0db-11a-03-ts3.0a4e3580-9711-488f-8a18-54f72c429035.svs 
                                                               1 
                                                            NA's 
                                                            1097 

patient.samples.sample.4.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   1 1097 

patient.samples.sample.4.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   1 NA's 
   1 1097 

patient.samples.sample.4.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   0 NA's 
   1 1097 

patient.samples.sample.4.portions.portion.slides.slide.2.percent_necrosis:
   0 NA's 
   1 1097 

patient.samples.sample.4.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   0 NA's 
   1 1097 

patient.samples.sample.4.portions.portion.slides.slide.2.percent_normal_cells:
   0 NA's 
   1 1097 

patient.samples.sample.4.portions.portion.slides.slide.2.percent_stromal_cells:
 100 NA's 
   1 1097 

patient.samples.sample.4.portions.portion.slides.slide.2.percent_tumor_cells:
   0 NA's 
   1 1097 

patient.samples.sample.4.portions.portion.slides.slide.2.percent_tumor_nuclei:
   0 NA's 
   1 1097 

patient.samples.sample.4.portions.portion.slides.slide.2.section_location:
bottom   NA's 
     1   1097 

patient.samples.sample.4.portions.portion.slides.slide.bcr_slide_barcode:
tcga-a7-a0db-11a-02-bs2 tcga-a7-a0dc-11a-04-ts4 tcga-a7-a13e-11a-03-ts3 
                      1                       1                       1 
tcga-e2-a15k-11a-01-ts1                    NA's 
                      1                    1094 

patient.samples.sample.4.portions.portion.slides.slide.bcr_slide_uuid:
28656e31-f16f-431e-95d4-915892ecdff8 6041f153-cc5a-44d8-9e04-f082918268e6 
                                   1                                    1 
8d8e5cca-5222-4dc8-8b42-df64bdc7652a 927ae45d-b4aa-4dec-8e30-88902119bbe5 
                                   1                                    1 
                                NA's 
                                1094 

patient.samples.sample.4.portions.portion.slides.slide.image_file_name:
tcga-a7-a0db-11a-02-bs2.8d8e5cca-5222-4dc8-8b42-df64bdc7652a.svs 
                                                               1 
tcga-a7-a0dc-11a-04-ts4.6041f153-cc5a-44d8-9e04-f082918268e6.svs 
                                                               1 
tcga-a7-a13e-11a-03-ts3.927ae45d-b4aa-4dec-8e30-88902119bbe5.svs 
                                                               1 
tcga-e2-a15k-11a-01-ts1.28656e31-f16f-431e-95d4-915892ecdff8.svs 
                                                               1 
                                                            NA's 
                                                            1094 

patient.samples.sample.4.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   4 1094 

patient.samples.sample.4.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   0    1    2 NA's 
   2    1    1 1094 

patient.samples.sample.4.portions.portion.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   4 1094 

patient.samples.sample.4.portions.portion.slides.slide.percent_necrosis:
   0 NA's 
   3 1095 

patient.samples.sample.4.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   4 1094 

patient.samples.sample.4.portions.portion.slides.slide.percent_normal_cells:
   0    2  100 NA's 
   1    1    2 1094 

patient.samples.sample.4.portions.portion.slides.slide.percent_stromal_cells:
   0   98  100 NA's 
   1    1    1 1095 

patient.samples.sample.4.portions.portion.slides.slide.percent_tumor_cells:
   0 NA's 
   4 1094 

patient.samples.sample.4.portions.portion.slides.slide.percent_tumor_nuclei:
   0 NA's 
   3 1095 

patient.samples.sample.4.portions.portion.slides.slide.section_location:
 top NA's 
   4 1094 

patient.samples.sample.4.portions.portion.weight:
  30   90  160 NA's 
   2    1    1 1094 

patient.samples.sample.4.portions.portion.year_of_creation:
2010 2011 NA's 
   2    2 1094 

patient.samples.sample.4.portions.shipment_portion.bcr_shipment_portion_uuid:
e408026e-6c07-4f2e-a609-29f21e997938                                 NA's 
                                   1                                 1097 

patient.samples.sample.4.portions.shipment_portion.center_id:
  20 NA's 
   1 1097 

patient.samples.sample.4.portions.shipment_portion.is_ffpe:
  no NA's 
   1 1097 

patient.samples.sample.4.portions.shipment_portion.plate_id:
a43o NA's 
   1 1097 

patient.samples.sample.4.portions.shipment_portion.portion_number:
  71 NA's 
   1 1097 

patient.samples.sample.4.portions.shipment_portion.shipment_portion_bcr_aliquot_barcode:
tcga-a7-a13e-11a-71-a43o-20                        NA's 
                          1                        1097 

patient.samples.sample.4.portions.shipment_portion.shipment_portion_day_of_shipment:
  24 NA's 
   1 1097 

patient.samples.sample.4.portions.shipment_portion.shipment_portion_month_of_shipment:
   6 NA's 
   1 1097 

patient.samples.sample.4.portions.shipment_portion.shipment_portion_year_of_shipment:
2014 NA's 
   1 1097 

patient.samples.sample.4.sample_type:
solid tissue normal                NA's 
                  4                1094 

patient.samples.sample.4.sample_type_id:
  11 NA's 
   4 1094 

patient.samples.sample.4.vial_number:
   a NA's 
   4 1094 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     16     149     447    1064    1374    7858       2 



patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.3:
5f9215c3-e407-46f8-968e-503d7d14605c 66566db7-661d-4002-90c1-b15bf3425903 
                                   1                                    1 
bb83c7d4-f795-47e7-9a5c-7dbf0eb7fdaa e5d16dbf-cabd-4c96-a794-5f27bc305055 
                                   1                                    1 
                                NA's 
                                1094 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.4:
83b45d6c-e350-4436-812f-4155d9f7d331                                 NA's 
                                   1                                 1097 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0   130.0   210.0   300.1   370.0  2190.0       1 

patient.samples.sample.is_ffpe:
  no NA's 
1097    1 

patient.samples.sample.oct_embedded:
false  true 
  416   682 



patient.samples.sample.portions.portion.2.analytes.analyte.a260_a280_ratio:
1.83 1.88 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-a8-a07j-01a-21r-a057-13 tcga-e9-a1nc-01a-21r-a26b-07 
                           1                            1 
                        NA's 
                        1096 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
50f23d00-f6fd-4b3d-aed2-0705f7ae6a17 5c4f63ec-2d65-4baa-8bf2-13701fb69b10 
                                   1                                    1 
                                NA's 
                                1096 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
100482213 132160691      NA's 
        1         1      1096 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.center_id:
   7   13 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.concentration:
0.15 0.17 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.day_of_shipment:
  11   30 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.month_of_shipment:
   1    8 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_column:
   1    7 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_id:
a057 a26b NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_row:
   a    b NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.quantity:
2.21    3 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   2 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.volume:
  13   20 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2013 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.amount:
13.52  22.1  NA's 
    1     1  1096 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type:
 rna NA's 
   2 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type_id:
   r NA's 
   2 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.bcr_analyte_barcode:
tcga-a8-a07j-01a-21r tcga-e9-a1nc-01a-21r                 NA's 
                   1                    1                 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.bcr_analyte_uuid:
17efdb82-247b-4ef7-a96d-3432829f350f 4706cb04-7501-4c10-9553-62805456de09 
                                   1                                    1 
                                NA's 
                                1096 

patient.samples.sample.portions.portion.2.analytes.analyte.concentration:
0.15 0.17 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                            1096 

patient.samples.sample.portions.portion.2.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   2 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                      NA's 
                        2                      1096 

patient.samples.sample.portions.portion.2.analytes.analyte.rna.ratio_28s_18s:
1.48  1.8 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.rna.rinvalue:
 8.2  8.6 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      2    1096 

patient.samples.sample.portions.portion.2.analytes.analyte.subportion_sequence:
   1 NA's 
   2 1096 

patient.samples.sample.portions.portion.2.bcr_portion_barcode:
tcga-a8-a07j-01a-21 tcga-e9-a1nc-01a-21                NA's 
                  1                   1                1096 

patient.samples.sample.portions.portion.2.bcr_portion_uuid:
67de0659-1154-4ca8-b6ce-dc8e93ecc798 74485df8-1c52-4535-b197-a6f22d35fe51 
                                   1                                    1 
                                NA's 
                                1096 

patient.samples.sample.portions.portion.2.day_of_creation:
  18   29 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.is_ffpe:
  no NA's 
   2 1096 

patient.samples.sample.portions.portion.2.month_of_creation:
   4    7 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.portion_number:
  21 NA's 
   2 1096 

patient.samples.sample.portions.portion.2.portion_sequence:
   2 NA's 
   2 1096 

patient.samples.sample.portions.portion.2.slides.slide.2.bcr_slide_barcode:
tcga-a8-a07j-01a-02-bs2                    NA's 
                      1                    1097 

patient.samples.sample.portions.portion.2.slides.slide.2.bcr_slide_uuid:
5d44b586-7e36-46f9-bfc8-8f8afce34625                                 NA's 
                                   1                                 1097 

patient.samples.sample.portions.portion.2.slides.slide.2.image_file_name:
tcga-a8-a07j-01a-02-bs2.5d44b586-7e36-46f9-bfc8-8f8afce34625.svs 
                                                               1 
                                                            NA's 
                                                            1097 

patient.samples.sample.portions.portion.2.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   1 1097 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_lymphocyte_infiltration:
   2 NA's 
   1 1097 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_monocyte_infiltration:
   0 NA's 
   1 1097 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_necrosis:
   0 NA's 
   1 1097 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_neutrophil_infiltration:
   0 NA's 
   1 1097 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_normal_cells:
   4 NA's 
   1 1097 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_stromal_cells:
  21 NA's 
   1 1097 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_tumor_cells:
  75 NA's 
   1 1097 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_tumor_nuclei:
  75 NA's 
   1 1097 

patient.samples.sample.portions.portion.2.slides.slide.2.section_location:
bottom   NA's 
     1   1097 

patient.samples.sample.portions.portion.2.slides.slide.bcr_slide_barcode:
tcga-a8-a07j-01a-01-bs1 tcga-e9-a1nc-01a-02-tsb                    NA's 
                      1                       1                    1096 

patient.samples.sample.portions.portion.2.slides.slide.bcr_slide_uuid:
45067fa5-9305-45d9-a343-fccecf9babf4 c9f54364-bf94-4431-9d02-8c2ec1732d02 
                                   1                                    1 
                                NA's 
                                1096 

patient.samples.sample.portions.portion.2.slides.slide.image_file_name:
tcga-a8-a07j-01a-01-bs1.c9f54364-bf94-4431-9d02-8c2ec1732d02.svs 
                                                               1 
tcga-e9-a1nc-01a-02-tsb.45067fa5-9305-45d9-a343-fccecf9babf4.svs 
                                                               1 
                                                            NA's 
                                                            1096 

patient.samples.sample.portions.portion.2.slides.slide.is_derived_from_ffpe:
  no NA's 
   2 1096 

patient.samples.sample.portions.portion.2.slides.slide.percent_lymphocyte_infiltration:
   2   10 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.slides.slide.percent_monocyte_infiltration:
   0    1 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.slides.slide.percent_necrosis:
   0 NA's 
   2 1096 

patient.samples.sample.portions.portion.2.slides.slide.percent_neutrophil_infiltration:
   0    1 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.slides.slide.percent_normal_cells:
   0    5 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.slides.slide.percent_stromal_cells:
  25   30 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_cells:
  70 NA's 
   2 1096 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_nuclei:
  70 NA's 
   2 1096 

patient.samples.sample.portions.portion.2.slides.slide.section_location:
 top NA's 
   2 1096 

patient.samples.sample.portions.portion.2.weight:
  20   30 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.2.year_of_creation:
2010 2011 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.3.analytes.analyte.a260_a280_ratio:
1.71 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-a8-a06n-01a-12r-a056-07                         NA's 
                           1                         1097 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
b140fccd-5907-4b21-9ed9-a6bdaeb2de53                                 NA's 
                                   1                                 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
100475669      NA's 
        1      1097 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.center_id:
   7 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.concentration:
0.13 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.day_of_shipment:
  11 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.month_of_shipment:
   8 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.plate_column:
   7 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.plate_id:
a056 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.plate_row:
   b NA's 
   1 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.quantity:
3.47 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.volume:
26.7 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.amount:
20.85  NA's 
    1  1097 

patient.samples.sample.portions.portion.3.analytes.analyte.analyte_type:
 rna NA's 
   1 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.analyte_type_id:
   r NA's 
   1 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.bcr_analyte_barcode:
tcga-a8-a06n-01a-12r                 NA's 
                   1                 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.bcr_analyte_uuid:
65bf5932-5226-430d-8255-7956e01d9602                                 NA's 
                                   1                                 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.concentration:
0.13 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                            1097 

patient.samples.sample.portions.portion.3.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   1 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                      NA's 
                        1                      1097 

patient.samples.sample.portions.portion.3.analytes.analyte.rna.ratio_28s_18s:
 2.1 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.rna.rinvalue:
 9.7 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      1    1097 

patient.samples.sample.portions.portion.3.analytes.analyte.subportion_sequence:
   2 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.bcr_portion_barcode:
tcga-a8-a06n-01a-12                NA's 
                  1                1097 

patient.samples.sample.portions.portion.3.bcr_portion_uuid:
40702263-bf67-4940-baf1-078932feb7c5                                 NA's 
                                   1                                 1097 

patient.samples.sample.portions.portion.3.day_of_creation:
   8 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.is_ffpe:
  no NA's 
   1 1097 

patient.samples.sample.portions.portion.3.month_of_creation:
   6 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.portion_number:
  12 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.portion_sequence:
   1 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.2.bcr_slide_barcode:
tcga-a8-a06n-01a-01-ts1                    NA's 
                      1                    1097 

patient.samples.sample.portions.portion.3.slides.slide.2.bcr_slide_uuid:
0e8d431f-4df4-4e2e-a8d2-3febb0f1beb9                                 NA's 
                                   1                                 1097 

patient.samples.sample.portions.portion.3.slides.slide.2.image_file_name:
tcga-a8-a06n-01a-01-ts1.0e8d431f-4df4-4e2e-a8d2-3febb0f1beb9.svs 
                                                               1 
                                                            NA's 
                                                            1097 

patient.samples.sample.portions.portion.3.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_lymphocyte_infiltration:
   1 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_monocyte_infiltration:
   0 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_necrosis:
   7 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_neutrophil_infiltration:
   0 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_normal_cells:
   0 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_stromal_cells:
  40 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_tumor_cells:
  53 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_tumor_nuclei:
  90 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.2.section_location:
 top NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.bcr_slide_barcode:
tcga-a8-a06n-01a-01-bs1                    NA's 
                      1                    1097 

patient.samples.sample.portions.portion.3.slides.slide.bcr_slide_uuid:
12bf6cf9-767e-469a-ab07-accd6b3b134e                                 NA's 
                                   1                                 1097 

patient.samples.sample.portions.portion.3.slides.slide.image_file_name:
tcga-a8-a06n-01a-01-bs1.12bf6cf9-767e-469a-ab07-accd6b3b134e.svs 
                                                               1 
                                                            NA's 
                                                            1097 

patient.samples.sample.portions.portion.3.slides.slide.is_derived_from_ffpe:
  no NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.percent_lymphocyte_infiltration:
   7 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.percent_necrosis:
   2 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.percent_normal_cells:
   0 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.percent_stromal_cells:
  56 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.percent_tumor_cells:
  42 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.percent_tumor_nuclei:
  85 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.slides.slide.section_location:
bottom   NA's 
     1   1097 

patient.samples.sample.portions.portion.3.weight:
  20 NA's 
   1 1097 

patient.samples.sample.portions.portion.3.year_of_creation:
2010 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.880   1.780   1.830   1.834   1.880   2.270       5 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.amount:
26.7 NA's 
   1 1097 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 94929000  99051282 100477341 111737902 113423300 174189887         2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
 669  427    2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1585  0.1700  0.1800       2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   12.00   16.00   17.93   25.00   31.00       2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
1095    3 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.433   7.000  12.000       2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   4.078   6.000  12.000       2 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.690   3.000   3.470   3.527   4.270   4.810       3 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
1096    2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13   20 26.7 NA's 
 146  427  522    3 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014       2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.additional_studies.additional_study.disease_code:
fppp NA's 
   5 1093 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.additional_studies.additional_study.project_code:
tcga NA's 
   5 1093 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              5            1093 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97506797  97506844 100493075 124438074 145632090 167812940      1077 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7   13 NA's 
  12    9 1077 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.14 0.15 0.16 0.17 NA's 
   5    6    6    4 1077 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   2    9   14   28 NA's 
   2    3   14    2 1077 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  21 1077 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   3    7   10   12 NA's 
   5   11    2    3 1077 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1       3       6       5       6       9    1077 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.060   1.950   2.800   2.499   3.000   4.010    1077 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
  21 1077 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
 6.6   13   20 26.7 NA's 
   3    7    9    2 1077 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2010 2013 2014 NA's 
  11    7    3 1077 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.additional_studies.additional_study.disease_code:
fppp NA's 
   5 1093 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.additional_studies.additional_study.project_code:
tcga NA's 
   5 1093 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              5            1093 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
132179175 132179192 132179199 132179201 132179216 132179223      1093 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.center_id:
  13 NA's 
   5 1093 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.concentration:
0.15 0.16 0.17 NA's 
   1    3    1 1093 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
  13 NA's 
   5 1093 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   5 1093 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
   3 NA's 
   5 1093 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   6 NA's 
   5 1093 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_id:
a27d NA's 
   5 1093 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.quantity:
   3  3.2  3.4 NA's 
   1    3    1 1093 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.source_center:
  23 NA's 
   5 1093 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.volume:
  20 NA's 
   5 1093 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2013 NA's 
   5 1093 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-a7-a26j-01a-11r-a46x-36                         NA's 
                           1                         1097 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.bcr_aliquot_uuid:
e1006a79-6bd0-41d3-a3a4-b34146ce03b5                                 NA's 
                                   1                                 1097 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.biospecimen_barcode_bottom:
167814220      NA's 
        1      1097 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.center_id:
  36 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.concentration:
0.16 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.day_of_shipment:
  26 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.month_of_shipment:
   8 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.plate_column:
   5 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.plate_id:
a46x NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.plate_row:
   b NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.quantity:
 0.8 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.source_center:
  23 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.volume:
   5 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.year_of_shipment:
2015 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.amount:
  20 NA's 
   1 1097 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99011371  99051790 100477635 112146732 113423384 174191615         2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7   13 NA's 
 427  669    2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1585  0.1700  0.1800       2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   12.00   16.00   17.85   25.00   31.00       2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
1095    3 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.433   7.000  12.000       2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   4.078   6.000  12.000       2 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.950   3.000   3.200   3.266   3.600   4.540       3 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
1096    2 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   20 26.7 NA's 
 149  668  278    3 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014       2 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.14   17.32   29.67   33.69   46.58  120.39       5 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
 rna NA's 
1096    2 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
   r NA's 
1096    2 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1585  0.1700  0.1800       2 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                            1096 
                                            NA's 
                                               2 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
1095    3 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
   allprep rna extraction mirvana (allprep dna) rna                      NA's 
                        6                      1089                         3 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.400   1.600   1.641   1.800   3.500       2 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   8.000   8.700   8.581   9.200  10.000       2 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
   1096       2 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2    3 NA's 
 973  108    8    9 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 89490636  99035482  99035552  99586277 100470888 113419537       867 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   8    9 NA's 
 184   47  867 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
 231  867 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
   8    9   14   29 NA's 
  46   92    1   92  867 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 231  867 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   6    8    9 NA's 
  92   92   47  867 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.706   8.000  12.000     867 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
a020 a051 a071 a187 NA's 
  92   92   46    1  867 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
  40 NA's 
 231  867 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  23 NA's 
 231  867 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  80 NA's 
 231  867 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 2011 NA's 
 230    1  867 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-b6-a0i5-01a-11w-a100-09 tcga-b6-a0i6-01a-11w-a100-09 
                           1                            1 
                        NA's 
                        1096 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_uuid:
c4bc479e-97aa-4b67-bf96-52ebf695c33b f1139266-fade-4d27-ac67-60870e666295 
                                   1                                    1 
                                NA's 
                                1096 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.biospecimen_barcode_bottom:
100471825 100472080      NA's 
        1         1      1096 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.center_id:
   9 NA's 
   2 1096 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.concentration:
 0.5 NA's 
   2 1096 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.day_of_shipment:
  12 NA's 
   2 1096 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   2 1096 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.month_of_shipment:
  10 NA's 
   2 1096 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_column:
   3    4 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_id:
a100 NA's 
   2 1096 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_row:
   g NA's 
   2 1096 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.quantity:
 6.5 NA's 
   2 1096 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.source_center:
  23 NA's 
   2 1096 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.volume:
  13 NA's 
   2 1096 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.year_of_shipment:
2010 NA's 
   2 1096 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.amount:
  80 NA's 
   1 1097 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 89490683 100476731 108432441 112664518 113420304 167803513        37 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8    9 NA's 
  46 1015   37 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
1061   37 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     8.0    15.0    15.7    23.0    30.0      37 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
1060   38 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00    6.00    6.26    9.00   11.00      37 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.721   5.000  12.000      37 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
 6.5   40 NA's 
   1 1059   38 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
1061   37 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  13   80 NA's 
   1 1059   38 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014      37 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                   NA's 
                  1022                     39                     37 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
   w    x NA's 
1022   39   37 



patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
1061   37 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
1061   37 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                            1061 
                                            NA's 
                                              37 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
1060   38 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
      929        39       130 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2    3 NA's 
 946  100    8   44 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 89490284  99034356  99035710  99485734 100472229 100476263       868 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.center_id:
   8    9 NA's 
  92  138  868 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
 230  868 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
   8    9   29 NA's 
  46   92   92  868 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 230  868 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
   6    8    9 NA's 
  92   92   46  868 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.722   8.000  12.000     868 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_id:
a024 a049 a073 NA's 
  92   92   46  868 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.quantity:
  40 NA's 
 230  868 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.source_center:
  23 NA's 
 230  868 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.volume:
  80 NA's 
 230  868 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
 230  868 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99013507  99059851 100471308  99767501 100472556 100476246       820 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   8    9 NA's 
 138  140  820 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
 278  820 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   8    9   12   29 NA's 
  46   92   48   92  820 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 278  820 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   6    8    9   10 NA's 
  92   92   46   48  820 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.349   8.000  12.000     820 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
 6.5   40 NA's 
   1  277  820 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  23 NA's 
 278  820 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  13   80 NA's 
   1  277  820 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2010 NA's 
 278  820 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type:
repli-g x (qiagen) dna                   NA's 
                   278                    820 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type_id:
   x NA's 
 278  820 



patient.samples.sample.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
 278  820 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
 yes NA's 
 278  820 

patient.samples.sample.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             278 
                                            NA's 
                                             820 

patient.samples.sample.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
 278  820 

patient.samples.sample.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g x      NA's 
      185       913 

patient.samples.sample.portions.portion.analytes.analyte.4.subportion_sequence:
   1    2    3 NA's 
 258   17    2  821 

patient.samples.sample.portions.portion.analytes.analyte.5.a260_a280_ratio:
1.89 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.bcr_aliquot_barcode:
tcga-a7-a0cg-01a-12r-a056-07                         NA's 
                           1                         1097 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.bcr_aliquot_uuid:
2b0e2135-a1e4-4785-8b9f-b4cf7b36d938                                 NA's 
                                   1                                 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.biospecimen_barcode_bottom:
100489855      NA's 
        1      1097 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.center_id:
   7 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.concentration:
0.14 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.day_of_shipment:
  11 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.month_of_shipment:
   8 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_column:
   7 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_id:
a056 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_row:
   a NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.quantity:
3.74 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.source_center:
  23 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.volume:
26.7 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.year_of_shipment:
2010 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.amount:
11.7 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.analyte_type:
 rna NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.analyte_type_id:
   r NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.bcr_analyte_barcode:
tcga-a7-a0cg-01a-12r                 NA's 
                   1                 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.bcr_analyte_uuid:
baa14eb8-a1f1-461a-9089-fe79b32d45e6                                 NA's 
                                   1                                 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.concentration:
0.14 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                            1097 

patient.samples.sample.portions.portion.analytes.analyte.5.is_derived_from_ffpe:
  no NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                      NA's 
                        1                      1097 

patient.samples.sample.portions.portion.analytes.analyte.5.rna.ratio_28s_18s:
 1.6 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.rna.rinvalue:
 8.5 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.5.spectrophotometer_method:
uv spec    NA's 
      1    1097 

patient.samples.sample.portions.portion.analytes.analyte.5.subportion_sequence:
   2 NA's 
   1 1097 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
   0.710    1.880    1.940    4.137    2.000 1761.000      297 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.amount:
6.67 NA's 
   1 1097 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99015303  99054750 100490520 112412430 113423242 174191459         2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    2    5    9 NA's 
 595   92   17  392    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.06    0.08    0.15    0.13    0.16    0.18       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   12.00   16.00   17.88   25.00   31.00       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
1095    3 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.446   7.000  12.000       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.699   5.000  12.000       2 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.070   1.200   1.824   2.800   4.000       3 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
1096    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.67    6.67   18.15   40.00   40.00       3 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.amount:
  26 NA's 
   1 1097 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99013582 100471165 100487632 112270830 113423672 174190374         2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   1    2    5    9 NA's 
 206   55  429  406    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.1500  0.1287  0.1600  0.2100       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   12.00   19.00   19.04   28.00   31.00       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
1095    3 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.446   7.000  12.000       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.701   5.000  12.000       2 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.130   2.080   2.584   3.600   7.200       3 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
1096    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.00   13.34   23.36   40.00   80.00       3 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.amount:
26.7 NA's 
   1 1097 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 89490670  99050898 100478900 103646632 108474665 153879512       284 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   1    5    8    9 NA's 
   1  420  230  163  284 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.1400  0.1213  0.1600  0.1900     284 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00    9.00   14.00   16.99   23.00   31.00     284 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 813  285 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.506   8.000  12.000     284 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.275   6.000  12.000     284 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.600   3.470   2.998   4.270   4.810     285 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
 814  284 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   20.00   26.70   23.76   26.70   40.00     285 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 2013 NA's 
 344  469    1  284 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.disease_code:
fppp NA's 
   5 1093 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.project_code:
tcga NA's 
   5 1093 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              5            1093 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 89490664  99036799 100477270 103660980 100478323 153879542       749 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   1    2    5    9 NA's 
   3   87   49  210  749 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.0800  0.1074  0.1500  0.1700     749 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    9.00   19.00   17.62   26.00   31.00     749 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
 349  749 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000   8.000   8.289   9.000  12.000     749 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   5.613   9.000  12.000     749 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.070   1.820   2.157   3.010  17.000     749 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
 349  749 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.00   13.34   20.63   26.70  100.00     749 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2011 2013 NA's 
 273   60   16  749 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.additional_studies.additional_study.disease_code:
fppp NA's 
   2 1096 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.additional_studies.additional_study.project_code:
tcga NA's 
   2 1096 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2            1096 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99013036 100477217 100477263 107595374 113388420 167811774      1013 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   1    5    9 NA's 
   2   48   35 1013 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.1400  0.1211  0.1500  0.1700    1013 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   20.00   20.00   17.86   20.00   26.00    1013 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
  85 1013 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000  12.000   9.165  12.000  12.000    1013 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.776   4.000   7.000    1013 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.010   2.080   3.470   3.662   4.010  16.000    1013 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
  85 1013 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   26.00   26.70   30.48   26.70  100.00    1013 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2010 2011 2013 2015 NA's 
  45   34    5    1 1013 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.additional_studies.additional_study.disease_code:
fppp NA's 
   2 1096 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.additional_studies.additional_study.project_code:
tcga NA's 
   2 1096 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2            1096 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99027527 105041074 113388893 121033445 132159869 167811798      1083 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   5    9 NA's 
   3   12 1083 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.070   0.080   0.080   0.092   0.080   0.160    1083 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   2   13   21   26 NA's 
   5    3    1    6 1083 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
  15 1083 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   4.000   5.467  11.000  11.000    1083 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.500   2.000   2.867   3.000   7.000    1083 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.01    1.60    2.08    3.15    3.00   18.62    1083 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  23 NA's 
  15 1083 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   20.00   26.00   32.65   40.00  133.00    1083 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2011 2013 2015 NA's 
  10    4    1 1083 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_barcode:
tcga-a7-a26j-01a-11d-a46w-08 tcga-b6-a0re-01a-11d-a46a-09 
                           1                            1 
                        NA's 
                        1096 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_uuid:
5c823e40-9360-4409-a072-1a2b315f715f 844d82c5-d978-40e0-a7c6-16b52deb6702 
                                   1                                    1 
                                NA's 
                                1096 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
145674768 167811773      NA's 
        1         1      1096 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
   8    9 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
0.08 NA's 
   2 1096 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
   3   21 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   2 1096 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   1    8 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   6    7 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id:
a46a a46w NA's 
   1    1 1096 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_row:
   f    g NA's 
   1    1 1096 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
1.07  1.6 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  23 NA's 
   2 1096 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
13.4   20 NA's 
   1    1 1096 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2015 NA's 
   2 1096 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.amount:
  13 NA's 
   1 1097 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99014634 100473754 100490542 112034526 113421550 174191244         2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2    5 NA's 
 295  570  231    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1200  0.1500  0.1600  0.1582  0.1700  0.2100       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   12.00   16.00   17.86   25.00   31.00       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
1095    3 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.346   7.000  12.000       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.699   5.000  12.000       2 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.80    1.14    2.08    2.23    2.34    4.54       3 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
1096    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
 6.7    7   13 26.7 NA's 
 282   13  569  231    3 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014       2 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.95   22.75   36.45   40.80   56.99  116.67       6 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
1096    2 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
1096    2 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1200  0.1500  0.1600  0.1582  0.1700  0.2100       2 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
1096    2 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
1096    2 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                            1096 
                                            NA's 
                                               2 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
1095    3 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                 1096                     2 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
      291       805         2 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2    3 NA's 
 973  108    8    9 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   12.00   13.84   21.00   31.00       2 

patient.samples.sample.portions.portion.is_ffpe:
  no NA's 
1095    3 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.259   9.000  12.000       2 

patient.samples.sample.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   11.00   11.00   13.98   12.00   81.00       2 

patient.samples.sample.portions.portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.291   1.000   8.000      30 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
 446  652 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   2.000   6.794   6.000 100.000     651 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.054   0.000  30.000     651 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   1.877   2.000  30.000     651 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.9933  0.0000 30.0000     651 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   4.548   5.000  79.000     651 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   10.00   18.00   20.85   25.50   90.00     651 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    69.5    78.0    72.9    85.0   100.0     651 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   70.00   80.00   77.51   85.00   98.00     651 

patient.samples.sample.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
    28    419    651 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
1094    4 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   6.151   4.000 100.000       6 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.604   0.000  30.000       6 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.771   2.000  35.000       6 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.629   0.000  50.000       6 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00    3.86    3.25   89.00       6 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   10.00   20.00   21.05   30.00   95.00       6 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   64.75   75.00   73.28   85.00  100.00       6 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    70.0    80.0    77.3    85.0    98.0       6 

patient.samples.sample.portions.portion.slides.slide.section_location:
bottom    top   NA's 
   420    675      3 

patient.samples.sample.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   30.00   30.00   29.29   30.00  100.00       3 

patient.samples.sample.portions.portion.year_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014       2 


patient.samples.sample.portions.shipment_portion.2.center_id:
  20   30 NA's 
  12  112  974 

patient.samples.sample.portions.shipment_portion.2.is_ffpe:
  no NA's 
 124  974 


patient.samples.sample.portions.shipment_portion.2.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  12.00   22.00   41.00   33.26   41.00   61.00     974 

patient.samples.sample.portions.shipment_portion.2.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   3.193   4.000   6.000     974 


patient.samples.sample.portions.shipment_portion.2.shipment_portion_day_of_shipment:
   3   22 NA's 
 112   12  974 

patient.samples.sample.portions.shipment_portion.2.shipment_portion_month_of_shipment:
   3    4 NA's 
  12  112  974 

patient.samples.sample.portions.shipment_portion.2.shipment_portion_year_of_shipment:
2011 2012 NA's 
  12  112  974 


patient.samples.sample.portions.shipment_portion.center_id:
  20   30 NA's 
 873   14  211 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
 886  212 


patient.samples.sample.portions.shipment_portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   21.00   21.00   20.42   21.00   91.00     211 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   1.935   2.000   9.000     233 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00   22.00   14.85   22.00   24.00     211 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   3.000   6.000   5.944   8.000  11.000     211 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2011 2012 2013 2014 NA's 
 648   99   79   61  211 

patient.samples.sample.sample_type:
primary tumor 
         1098 

patient.samples.sample.sample_type_id:
   1 
1098 

patient.samples.sample.vial_number:
   a    b 
1088   10 

Gender:
FEMALE   MALE   NA's 
   809      8    281 

Age.at.Initial.Pathologic.Diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  26.00   48.00   58.00   58.06   67.00   90.00     281 



HER2.Final.Status:
    Equivocal      Negative Not Available      Positive          NA's 
           10           651            15           114           308 


Tumor..T1.Coded:
T_Other      T1    NA's 
    577     210     311 

Node:
  N0   N1   N2   N3 NA's 
 385  263   99   43  308 

Node.Coded:
Negative Positive     NA's 
     385      405      308 

Metastasis:
  M0   M1 NA's 
 770   14  314 

Metastasis.Coded:
Negative Positive     NA's 
     770       14      314 



Survival.Data.Form:
enrollment   followup       NA's 
       583        234        281 

Vital.Status:
DECEASED   LIVING     NA's 
      93      724      281 


Days.to.date.of.Death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    157     811    1563    1744    2520    4456    1005 

OS.event:
   0    1 NA's 
 724   93  281 

OS.Time:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   178.0   545.0   912.9  1295.0  7125.0     281 


SigClust.Unsupervised.mRNA:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-12.000  -7.000  -3.000  -4.789  -3.000   0.000     576 

SigClust.Intrinsic.mRNA:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-13.000 -12.000  -5.000  -6.764  -2.000   0.000     576 

miRNA.Clusters:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   4.176   6.000   7.000     404 

methylation.Clusters:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.111   4.000   5.000     299 


CN.Clusters:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   2.713   4.000   5.000     325 

Integrated.Clusters..with.PAM50.:
   1    2    3    4 NA's 
  39   72  161   76  750 

Integrated.Clusters..no.exp.:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.195   3.000   5.000     750 

Integrated.Clusters..unsup.exp.:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   2.641   3.000   5.000     750 

X60.Gene.classifier.Class.Assignment:
Immune-related  Proliferative  Reactive-like           NA's 
            50             27             50            971 

}}
\keyword{datasets}
