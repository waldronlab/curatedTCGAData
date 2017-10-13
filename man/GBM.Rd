\name{ GBM }
\alias{ GBM }
\docType{data}
\title{ Glioblastoma multiforme }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( GBM )
ExperimentList class object of length 16: 
 [1] CNACGH_CGH_hg_244a-20160128: RaggedExperiment with 81512 rows and 438 columns 
 [2] CNACGH_CGH_hg_415k_g4124a-20160128: RaggedExperiment with 57975 rows and 338 columns 
 [3] CNASNP-20160128: RaggedExperiment with 602338 rows and 1104 columns 
 [4] CNVSNP-20160128: RaggedExperiment with 146852 rows and 1104 columns 
 [5] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 577 columns 
 [6] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 577 columns 
 [7] Methylation_methyl27-20160128: SummarizedExperiment with 27578 rows and 285 columns 
 [8] Methylation_methyl450-20160128: SummarizedExperiment with 485577 rows and 154 columns 
 [9] miRNAArray-20160128: SummarizedExperiment with 534 rows and 565 columns 
 [10] miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 0 columns 
 [11] mRNAArray_huex-20160128: SummarizedExperiment with 18632 rows and 431 columns 
 [12] mRNAArray_TX_g4502a-20160128: SummarizedExperiment with 17814 rows and 502 columns 
 [13] mRNAArray_TX_ht_hg_u133a-20160128: SummarizedExperiment with 12042 rows and 528 columns 
 [14] Mutation-20160128: RaggedExperiment with 22073 rows and 290 columns 
 [15] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 166 columns 
 [16] RPPAArray-20160128: SummarizedExperiment with 208 rows and 244 columns 

> rownames( GBM )
CharacterList of length 16
[["CNACGH_CGH_hg_244a-20160128"]] character(0)
[["CNACGH_CGH_hg_415k_g4124a-20160128"]] character(0)
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["GISTIC_ThresholdedByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["Methylation_methyl27-20160128"]] cg00000292 cg00002426 ... cg27665659
[["Methylation_methyl450-20160128"]] cg00000029 cg00000108 ... rs9839873
[["miRNAArray-20160128"]] ebv-miR-BART1-3p ebv-miR-BART1-5p ... kshv-miR-K12-9*
[["miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
...
<6 more elements>

> colnames( GBM )
CharacterList of length 16
[["CNACGH_CGH_hg_244a-20160128"]] TCGA-02-0001-01C-01D-0185-02 ...
[["CNACGH_CGH_hg_415k_g4124a-20160128"]] TCGA-02-2466-01A-01D-0787-02 ...
[["CNASNP-20160128"]] TCGA-02-0001-01C-01D-0182-01 ...
[["CNVSNP-20160128"]] TCGA-02-0001-01C-01D-0182-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-02-0001-01C-01D-0182-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-02-0001-01C-01D-0182-01 ...
[["Methylation_methyl27-20160128"]] TCGA-02-0001-01C-01D-0186-05 ...
[["Methylation_methyl450-20160128"]] TCGA-06-0125-01A-01D-A45W-05 ...
[["miRNAArray-20160128"]] TCGA-02-0001-01C-01T-0179-07 ...
[["miRNASeqGene-20160128"]] character(0)
...
<6 more elements>

Sizes of each ExperimentList element:

                                assay size.Mb
1         CNACGH_CGH_hg_244a-20160128  2.3 Mb
2  CNACGH_CGH_hg_415k_g4124a-20160128  1.7 Mb
3                     CNASNP-20160128 16.4 Mb
4                     CNVSNP-20160128  4.2 Mb
5           GISTIC_AllByGene-20160128  4.4 Mb
6   GISTIC_ThresholdedByGene-20160128  4.4 Mb
7       Methylation_methyl27-20160128  4.4 Mb
8      Methylation_methyl450-20160128 67.4 Mb
9                 miRNAArray-20160128  0.1 Mb
10              miRNASeqGene-20160128  0.1 Mb
11            mRNAArray_huex-20160128  1.1 Mb
12       mRNAArray_TX_g4502a-20160128    1 Mb
13  mRNAArray_TX_ht_hg_u133a-20160128  0.7 Mb
14                  Mutation-20160128 29.2 Mb
15           RNASeq2GeneNorm-20160128  1.1 Mb
16                 RPPAArray-20160128    0 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   109 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
490.000 490.000   1.047   0.981   1.156 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   50.00   59.00   57.82   68.00   89.00       4 

vital_status:
   0    1 NA's 
 104  491    4 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0   170.5   382.0   504.5   609.2  3881.0     109 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   156.2   261.0   479.4   628.0  2818.0     497 

tumor_tissue_site:
brain  NA's 
  595     4 

gender:
female   male   NA's 
   230    365      4 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1989    2002    2006    2005    2009    2013       4 

radiation_therapy:
  no  yes NA's 
  78  489   32 

karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   70.00   80.00   77.04   80.00  100.00     157 

histological_type:
  glioblastoma multiforme (gbm)             treated primary gbm 
                             31                              20 
untreated primary (de novo) gbm                            NA's 
                            544                               4 

race:
                    asian black or african american                     white 
                       13                        51                       506 
                     NA's 
                       29 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    13                    489                     97 

admin.bcr:
nationwide children's hospital                           NA's 
                           595                              4 

admin.day_of_dcc_upload:
   1    7 NA's 
 570   25    4 

admin.disease_code:
 gbm NA's 
 595    4 


admin.month_of_dcc_upload:
  10 NA's 
 595    4 

admin.patient_withdrawal.withdrawn:
false  NA's 
  595     4 

admin.project_code:
tcga NA's 
 595    4 

admin.year_of_dcc_upload:
2015 NA's 
 595    4 

patient.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   50.00   59.00   57.82   68.00   89.00       4 


patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   16.00   14.59   19.00   31.00       4 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -32612  -25149  -21698  -21298  -18279   -3982       4 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0   150.8   357.0   476.6   584.0  3881.0     153 

patient.days_to_initial_pathologic_diagnosis:
   0 NA's 
 595    4 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   141.0   285.0   415.3   496.0  3881.0      38 



patient.drugs.drug.10.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.25   17.00   14.71   22.50   28.00     585 

patient.drugs.drug.10.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  148.0   413.2   569.0   679.7  1034.2  1468.0     585 

patient.drugs.drug.10.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   91.0   290.2   497.5   618.9  1025.5  1307.0     585 


patient.drugs.drug.10.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.250   5.000   5.786   6.000  11.000     585 

patient.drugs.drug.10.number_cycles:
   1    2    3    4 NA's 
   4    1    1    2  591 

patient.drugs.drug.10.prescribed_dose:
  30   50  200 NA's 
   1    1    1  596 

patient.drugs.drug.10.prescribed_dose_units:
   mg mg/kg mg/m2  NA's 
    9     1     3   586 

patient.drugs.drug.10.regimen_indication:
   adjuvant progression  recurrence        NA's 
          3           8           2         586 

patient.drugs.drug.10.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               2                8              589 

patient.drugs.drug.10.therapy_ongoing:
  no NA's 
  11  588 

patient.drugs.drug.10.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                         9                          1 
             immunotherapy targeted molecular therapy 
                         1                          3 
                      NA's 
                       585 

patient.drugs.drug.10.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0    37.5   110.0  7824.0   250.0 90400.0     586 

patient.drugs.drug.10.total_dose_units:
   mg mg/kg mg/m2  NA's 
    9     1     3   586 

patient.drugs.drug.10.tx_on_clinical_trial:
  no NA's 
   1  598 

patient.drugs.drug.10.year_of_form_completion:
2009 2010 2012 NA's 
  10    3    1  585 



patient.drugs.drug.11.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   10.50   17.00   16.55   24.00   28.00     588 

patient.drugs.drug.11.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   71.0   217.5   378.0   630.0  1015.0  1616.0     588 

patient.drugs.drug.11.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   29.0   184.5   327.0   573.6   952.5  1529.0     588 


patient.drugs.drug.11.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.000   5.000   5.545   6.000  11.000     588 

patient.drugs.drug.11.number_cycles:
   1    2 NA's 
   5    1  593 

patient.drugs.drug.11.prescribed_dose:
  10  120 NA's 
   1    1  597 

patient.drugs.drug.11.prescribed_dose_units:
{wafer}      mg   mg/kg   mg/m2    NA's 
      1       4       1       3     590 

patient.drugs.drug.11.regimen_indication:
   adjuvant progression  recurrence        NA's 
          5           4           1         589 

patient.drugs.drug.11.route_of_administrations.route_of_administration:
    intratumoral intravenous (iv)             oral             NA's 
               1                3                5              590 

patient.drugs.drug.11.therapy_ongoing:
  no NA's 
   8  591 

patient.drugs.drug.11.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                         6                          1 
targeted molecular therapy                       NA's 
                         4                        588 

patient.drugs.drug.11.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      8      75      90    1131     600    5300     590 

patient.drugs.drug.11.total_dose_units:
{wafer}      mg   mg/kg   mg/m2    NA's 
      1       4       1       3     590 

patient.drugs.drug.11.tx_on_clinical_trial:
  no NA's 
   1  598 

patient.drugs.drug.11.year_of_form_completion:
2009 2010 2012 NA's 
   8    2    1  588 



patient.drugs.drug.12.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   10.50   17.00   16.55   24.00   28.00     588 

patient.drugs.drug.12.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   72.0   196.2   551.0   645.1  1071.2  1526.0     589 

patient.drugs.drug.12.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   26.0   135.0   326.0   550.1   988.0  1468.0     588 


patient.drugs.drug.12.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.000   5.000   5.545   6.000  11.000     588 

patient.drugs.drug.12.number_cycles:
   1    2 NA's 
   1    2  596 

patient.drugs.drug.12.prescribed_dose:
   5   10 NA's 
   1    1  597 

patient.drugs.drug.12.prescribed_dose_units:
    mg mg/day  mg/m2   NA's 
     5      1      3    590 

patient.drugs.drug.12.regimen_indication:
   adjuvant progression  recurrence        NA's 
          5           4           1         589 

patient.drugs.drug.12.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               2                3              594 

patient.drugs.drug.12.therapy_ongoing:
  no  yes NA's 
   6    1  592 

patient.drugs.drug.12.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                         4                          1 
targeted molecular therapy                       NA's 
                         6                        588 

patient.drugs.drug.12.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0   100.0   125.0   676.4   700.0  3003.0     590 

patient.drugs.drug.12.total_dose_units:
    mg mg/day  mg/m2   NA's 
     5      1      3    590 

patient.drugs.drug.12.tx_on_clinical_trial:
  no NA's 
   1  598 

patient.drugs.drug.12.year_of_form_completion:
2009 2010 2012 NA's 
   8    2    1  588 



patient.drugs.drug.13.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    5.00   17.00   15.56   24.00   28.00     590 

patient.drugs.drug.13.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   37.0   484.0   602.0   749.3  1090.0  1526.0     590 

patient.drugs.drug.13.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   37.0   438.0   567.0   691.9  1073.0  1468.0     590 


patient.drugs.drug.13.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   4.556   5.000   8.000     590 

patient.drugs.drug.13.number_cycles:
   1    2 NA's 
   3    1  595 

patient.drugs.drug.13.prescribed_dose:
   8 NA's 
   1  598 


patient.drugs.drug.13.regimen_indication:
   adjuvant progression        NA's 
          3           6         590 

patient.drugs.drug.13.route_of_administrations.route_of_administration:
    intratumoral intravenous (iv)             oral             NA's 
               1                1                2              595 

patient.drugs.drug.13.therapy_ongoing:
  no NA's 
   5  594 

patient.drugs.drug.13.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                         4                          1 
targeted molecular therapy                       NA's 
                         4                        590 

patient.drugs.drug.13.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    61.6   150.0   874.6   400.0  5520.0     590 


patient.drugs.drug.13.year_of_form_completion:
2009 2010 NA's 
   7    2  590 



patient.drugs.drug.14.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    5.00    7.00   11.86   17.50   28.00     592 

patient.drugs.drug.14.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   56.0   621.5  1084.0   963.9  1374.0  1616.0     592 

patient.drugs.drug.14.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   55.0   542.5  1007.0   913.1  1331.0  1583.0     592 


patient.drugs.drug.14.month_of_form_completion:
   3    5    6 NA's 
   1    4    2  592 

patient.drugs.drug.14.number_cycles:
   1    3 NA's 
   1    1  597 

patient.drugs.drug.14.prescribed_dose:
  10 NA's 
   1  598 

patient.drugs.drug.14.prescribed_dose_units:
   mg mg/m2  NA's 
    3     4   592 

patient.drugs.drug.14.regimen_indication:
   adjuvant progression  recurrence        NA's 
          2           4           1         592 

patient.drugs.drug.14.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               1                4              594 

patient.drugs.drug.14.therapy_ongoing:
  no NA's 
   2  597 

patient.drugs.drug.14.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                         3                          1 
targeted molecular therapy                       NA's 
                         3                        592 

patient.drugs.drug.14.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     10      30     100     260     125    1400     592 

patient.drugs.drug.14.total_dose_units:
   mg mg/m2  NA's 
    3     4   592 

patient.drugs.drug.14.year_of_form_completion:
2009 2010 NA's 
   6    1  592 



patient.drugs.drug.15.day_of_form_completion:
   5   18   22   28 NA's 
   2    1    1    1  594 

patient.drugs.drug.15.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    203     688    1135    1004    1274    1722     594 

patient.drugs.drug.15.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  183.0   471.0  1133.0   913.4  1141.0  1639.0     594 

patient.drugs.drug.15.drug_name:
        06gb      avastin  chloroquine temozolomide         NA's 
           1            2            1            1          594 

patient.drugs.drug.15.month_of_form_completion:
   5    6 NA's 
   4    1  594 

patient.drugs.drug.15.number_cycles:
   1 NA's 
   1  598 

patient.drugs.drug.15.prescribed_dose:
 500 NA's 
   1  598 

patient.drugs.drug.15.prescribed_dose_units:
   mg mg/kg mg/m2  NA's 
    2     1     2   594 

patient.drugs.drug.15.regimen_indication:
   adjuvant progression  recurrence        NA's 
          1           3           1         594 

patient.drugs.drug.15.route_of_administrations.route_of_administration:
oral NA's 
   2  597 

patient.drugs.drug.15.therapy_ongoing:
  no NA's 
   3  596 

patient.drugs.drug.15.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         1                          4 
                      NA's 
                       594 

patient.drugs.drug.15.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     10     120     200    4361     475   21000     594 

patient.drugs.drug.15.total_dose_units:
   mg mg/kg mg/m2  NA's 
    2     1     2   594 

patient.drugs.drug.15.year_of_form_completion:
2009 2010 NA's 
   4    1  594 

patient.drugs.drug.16.bcr_drug_barcode:
tcga-12-0662-d23525 tcga-12-0670-d23536 tcga-12-0773-d23623 tcga-12-3644-d23784 
                  1                   1                   1                   1 
               NA's 
                595 

patient.drugs.drug.16.bcr_drug_uuid:
52230949-b4f3-4314-878c-6b19aa30ddc6 5c66bde0-39fb-444d-b4c3-4c1628a36fcf 
                                   1                                    1 
6b93f331-1d67-49d2-afbd-72b4fc799ea7 76066f9e-2cbd-433b-8e83-07dd811edc04 
                                   1                                    1 
                                NA's 
                                 595 

patient.drugs.drug.16.day_of_form_completion:
   5   18   22   28 NA's 
   1    1    1    1  595 

patient.drugs.drug.16.days_to_drug_therapy_end:
  56  448  486 1161 NA's 
   1    1    1    1  595 

patient.drugs.drug.16.days_to_drug_therapy_start:
  55  179  407 1113 NA's 
   1    1    1    1  595 

patient.drugs.drug.16.drug_name:
celbrex  cpt-11  mu81c6 temodar    NA's 
      1       1       1       1     595 

patient.drugs.drug.16.month_of_form_completion:
   5    6 NA's 
   3    1  595 

patient.drugs.drug.16.number_cycles:
   1   10 NA's 
   1    1  597 

patient.drugs.drug.16.prescribed_dose:
 200 NA's 
   1  598 

patient.drugs.drug.16.prescribed_dose_units:
   mg mg/m2  NA's 
    3     1   595 

patient.drugs.drug.16.regimen_indication:
   adjuvant progression        NA's 
          3           1         595 

patient.drugs.drug.16.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               1                2              596 

patient.drugs.drug.16.therapy_ongoing:
  no NA's 
   3  596 

patient.drugs.drug.16.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                         2                          1 
targeted molecular therapy                       NA's 
                         1                        595 

patient.drugs.drug.16.total_dose:
   10   125   400 17000  NA's 
    1     1     1     1   595 

patient.drugs.drug.16.total_dose_units:
   mg mg/m2  NA's 
    3     1   595 

patient.drugs.drug.16.year_of_form_completion:
2009 2010 NA's 
   3    1  595 

patient.drugs.drug.17.bcr_drug_barcode:
tcga-12-0662-d23528 tcga-12-0670-d23548 tcga-12-3644-d23802                NA's 
                  1                   1                   1                 596 

patient.drugs.drug.17.bcr_drug_uuid:
0dacad5b-d120-432b-ac72-3c591e4db8cd 37cd6430-be4c-4272-86c6-8e4aa67835ee 
                                   1                                    1 
67a7c6ef-2066-4046-a8de-bb8d1bbac08b                                 NA's 
                                   1                                  596 

patient.drugs.drug.17.day_of_form_completion:
   5   18   28 NA's 
   1    1    1  596 

patient.drugs.drug.17.days_to_drug_therapy_end:
 601 1135 1639 NA's 
   1    1    1  596 

patient.drugs.drug.17.days_to_drug_therapy_start:
 578 1133 1611 NA's 
   1    1    1  596 

patient.drugs.drug.17.drug_name:
    06gb celebrex  tarceva     NA's 
       1        1        1      596 

patient.drugs.drug.17.month_of_form_completion:
   5    6 NA's 
   2    1  596 

patient.drugs.drug.17.prescribed_dose:
 150 NA's 
   1  598 

patient.drugs.drug.17.prescribed_dose_units:
   mg mg/m2  NA's 
    1     2   596 

patient.drugs.drug.17.regimen_indication:
   adjuvant progression  recurrence        NA's 
          1           1           1         596 

patient.drugs.drug.17.route_of_administrations.route_of_administration:
oral NA's 
   2  597 

patient.drugs.drug.17.therapy_ongoing:
  no NA's 
   2  597 

patient.drugs.drug.17.therapy_types.therapy_type:
             immunotherapy targeted molecular therapy 
                         1                          2 
                      NA's 
                       596 

patient.drugs.drug.17.total_dose:
  30  125 4200 NA's 
   1    1    1  596 

patient.drugs.drug.17.total_dose_units:
   mg mg/m2  NA's 
    1     2   596 

patient.drugs.drug.17.year_of_form_completion:
2009 2010 NA's 
   2    1  596 

patient.drugs.drug.18.bcr_drug_barcode:
tcga-12-0670-d23537 tcga-12-3644-d23790                NA's 
                  1                   1                 597 

patient.drugs.drug.18.bcr_drug_uuid:
16253cb7-0d72-41ce-b650-b17eb6a9572b 9257d812-89d6-4b0b-b773-f791a03ad624 
                                   1                                    1 
                                NA's 
                                 597 

patient.drugs.drug.18.day_of_form_completion:
  18   28 NA's 
   1    1  597 

patient.drugs.drug.18.days_to_drug_therapy_end:
 516 1639 NA's 
   1    1  597 

patient.drugs.drug.18.days_to_drug_therapy_start:
 448 1611 NA's 
   1    1  597 

patient.drugs.drug.18.drug_name:
 hydroxyurea temozolomide         NA's 
           1            1          597 

patient.drugs.drug.18.month_of_form_completion:
   5    6 NA's 
   1    1  597 

patient.drugs.drug.18.number_cycles:
   1    2 NA's 
   1    1  597 

patient.drugs.drug.18.prescribed_dose:
 500 NA's 
   1  598 

patient.drugs.drug.18.prescribed_dose_units:
   mg mg/m2  NA's 
    1     1   597 

patient.drugs.drug.18.regimen_indication:
  adjuvant recurrence       NA's 
         1          1        597 

patient.drugs.drug.18.route_of_administrations.route_of_administration:
oral NA's 
   2  597 

patient.drugs.drug.18.therapy_ongoing:
  no NA's 
   2  597 

patient.drugs.drug.18.therapy_types.therapy_type:
chemotherapy         NA's 
           2          597 

patient.drugs.drug.18.total_dose:
  200 14000  NA's 
    1     1   597 

patient.drugs.drug.18.total_dose_units:
   mg mg/m2  NA's 
    1     1   597 

patient.drugs.drug.18.year_of_form_completion:
2009 2010 NA's 
   1    1  597 

patient.drugs.drug.19.bcr_drug_barcode:
tcga-12-0670-d23535 tcga-12-3644-d23801                NA's 
                  1                   1                 597 

patient.drugs.drug.19.bcr_drug_uuid:
0be12284-5c9f-4fbc-a4bc-16a33097e010 701b8631-b4d3-46b1-9472-1fd8f5d000e2 
                                   1                                    1 
                                NA's 
                                 597 

patient.drugs.drug.19.day_of_form_completion:
  18   28 NA's 
   1    1  597 

patient.drugs.drug.19.days_to_drug_therapy_end:
 378 1639 NA's 
   1    1  597 

patient.drugs.drug.19.days_to_drug_therapy_start:
 329 1611 NA's 
   1    1  597 

patient.drugs.drug.19.drug_name:
   cpt-11 rapamycin      NA's 
        1         1       597 

patient.drugs.drug.19.month_of_form_completion:
   5    6 NA's 
   1    1  597 

patient.drugs.drug.19.number_cycles:
   1 NA's 
   1  598 

patient.drugs.drug.19.prescribed_dose:
   5 NA's 
   1  598 

patient.drugs.drug.19.prescribed_dose_units:
   mg mg/m2  NA's 
    1     1   597 

patient.drugs.drug.19.regimen_indication:
  adjuvant recurrence       NA's 
         1          1        597 

patient.drugs.drug.19.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               1                1              597 

patient.drugs.drug.19.therapy_ongoing:
  no NA's 
   2  597 

patient.drugs.drug.19.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         1                          1 
                      NA's 
                       597 

patient.drugs.drug.19.total_dose:
 125  140 NA's 
   1    1  597 

patient.drugs.drug.19.total_dose_units:
   mg mg/m2  NA's 
    1     1   597 

patient.drugs.drug.19.year_of_form_completion:
2009 2010 NA's 
   1    1  597 



patient.drugs.drug.2.clinical_trail_drug_classification:
targeted molecular therapy                       NA's 
                         1                        598 

patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   17.00   16.55   24.00   31.00     269 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -6.00   86.75  184.00  328.97  402.00 3942.00     349 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -112.0    27.0    92.0   181.6   182.0  3779.0     300 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease               stable disease 
                           2                            5 
                        NA's 
                         592 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   6.076   9.000  12.000     269 

patient.drugs.drug.2.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   3.518   4.000  24.000     400 

patient.drugs.drug.2.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    75.0   140.0   169.4   200.0  1750.0     474 



patient.drugs.drug.2.regimen_indication_notes:
 adjuvant and progression                concurrent concurrent chemoradiation 
                        2                         3                         1 
                     NA's 
                      593 

patient.drugs.drug.2.regimen_number:
   1    2    3 NA's 
  11    8    3  577 


patient.drugs.drug.2.route_of_administrations.route_of_administration.2:
intravenous (iv)             NA's 
               1              598 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
 254   28  317 

patient.drugs.drug.2.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                       274                         19 
             immunotherapy targeted molecular therapy 
                         6                         30 
                      NA's 
                       270 

patient.drugs.drug.2.therapy_types.therapy_type_notes:
daily for 42 days              NA's 
                1               598 

patient.drugs.drug.2.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1     110     300    4701    2800  590000     382 


patient.drugs.drug.2.tx_on_clinical_trial:
  no  yes NA's 
  10    5  584 

patient.drugs.drug.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2009    2010    2010    2011    2014     269 

patient.drugs.drug.20.bcr_drug_barcode:
tcga-12-0670-d23547 tcga-12-3644-d23791                NA's 
                  1                   1                 597 

patient.drugs.drug.20.bcr_drug_uuid:
1e817328-0469-4ae7-b77c-939fc0027749 b047e79b-8534-41db-a020-b40c3853e120 
                                   1                                    1 
                                NA's 
                                 597 

patient.drugs.drug.20.day_of_form_completion:
  18   28 NA's 
   1    1  597 

patient.drugs.drug.20.days_to_drug_therapy_end:
 576 1722 NA's 
   1    1  597 

patient.drugs.drug.20.days_to_drug_therapy_start:
 516 1639 NA's 
   1    1  597 

patient.drugs.drug.20.drug_name:
celebrex   cpt 11     NA's 
       1        1      597 

patient.drugs.drug.20.month_of_form_completion:
   5    6 NA's 
   1    1  597 

patient.drugs.drug.20.number_cycles:
   3 NA's 
   1  598 

patient.drugs.drug.20.prescribed_dose:
 125 NA's 
   1  598 

patient.drugs.drug.20.prescribed_dose_units:
   mg mg/m2  NA's 
    1     1   597 

patient.drugs.drug.20.regimen_indication:
  adjuvant recurrence       NA's 
         1          1        597 

patient.drugs.drug.20.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               1                1              597 

patient.drugs.drug.20.therapy_ongoing:
  no NA's 
   2  597 

patient.drugs.drug.20.therapy_types.therapy_type:
 chemotherapy immunotherapy          NA's 
            1             1           597 

patient.drugs.drug.20.total_dose:
 125 1200 NA's 
   1    1  597 

patient.drugs.drug.20.total_dose_units:
   mg mg/m2  NA's 
    1     1   597 

patient.drugs.drug.20.year_of_form_completion:
2009 2010 NA's 
   1    1  597 

patient.drugs.drug.21.bcr_drug_barcode:
tcga-12-0670-d23550 tcga-12-3644-d23805                NA's 
                  1                   1                 597 

patient.drugs.drug.21.bcr_drug_uuid:
63b3fd1c-3e08-4f33-ad48-c6da51c84fe9 888dc526-60c7-44ce-9fe9-de5128a8f5a9 
                                   1                                    1 
                                NA's 
                                 597 

patient.drugs.drug.21.day_of_form_completion:
  18   28 NA's 
   1    1  597 

patient.drugs.drug.21.days_to_drug_therapy_end:
 770 1722 NA's 
   1    1  597 

patient.drugs.drug.21.days_to_drug_therapy_start:
 722 1639 NA's 
   1    1  597 

patient.drugs.drug.21.drug_name:
 avastin celebrex     NA's 
       1        1      597 

patient.drugs.drug.21.month_of_form_completion:
   5    6 NA's 
   1    1  597 

patient.drugs.drug.21.prescribed_dose:
  10 NA's 
   1  598 

patient.drugs.drug.21.prescribed_dose_units:
   mg mg/m2  NA's 
    1     1   597 

patient.drugs.drug.21.regimen_indication:
  adjuvant recurrence       NA's 
         1          1        597 

patient.drugs.drug.21.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               1                1              597 

patient.drugs.drug.21.therapy_ongoing:
  no NA's 
   2  597 

patient.drugs.drug.21.therapy_types.therapy_type:
             immunotherapy targeted molecular therapy 
                         1                          1 
                      NA's 
                       597 

patient.drugs.drug.21.total_dose:
 125 4200 NA's 
   1    1  597 

patient.drugs.drug.21.total_dose_units:
   mg mg/m2  NA's 
    1     1   597 

patient.drugs.drug.21.year_of_form_completion:
2009 2010 NA's 
   1    1  597 

patient.drugs.drug.22.bcr_drug_barcode:
tcga-12-3644-d23798                NA's 
                  1                 598 

patient.drugs.drug.22.bcr_drug_uuid:
3d4e6f17-20f9-48b3-9cd4-45e0fc5fc0c5                                 NA's 
                                   1                                  598 

patient.drugs.drug.22.day_of_form_completion:
  18 NA's 
   1  598 

patient.drugs.drug.22.days_to_drug_therapy_end:
1611 NA's 
   1  598 

patient.drugs.drug.22.days_to_drug_therapy_start:
1583 NA's 
   1  598 

patient.drugs.drug.22.drug_name:
rapamycin      NA's 
        1       598 

patient.drugs.drug.22.month_of_form_completion:
   6 NA's 
   1  598 

patient.drugs.drug.22.prescribed_dose:
   5 NA's 
   1  598 

patient.drugs.drug.22.prescribed_dose_units:
  mg NA's 
   1  598 

patient.drugs.drug.22.regimen_indication:
recurrence       NA's 
         1        598 

patient.drugs.drug.22.route_of_administrations.route_of_administration:
oral NA's 
   1  598 

patient.drugs.drug.22.therapy_ongoing:
  no NA's 
   1  598 

patient.drugs.drug.22.therapy_types.therapy_type:
targeted molecular therapy                       NA's 
                         1                        598 

patient.drugs.drug.22.total_dose:
 140 NA's 
   1  598 

patient.drugs.drug.22.total_dose_units:
  mg NA's 
   1  598 

patient.drugs.drug.22.year_of_form_completion:
2010 NA's 
   1  598 

patient.drugs.drug.23.bcr_drug_barcode:
tcga-12-3644-d23797                NA's 
                  1                 598 

patient.drugs.drug.23.bcr_drug_uuid:
cacb1786-abd9-4536-a242-45540f791df9                                 NA's 
                                   1                                  598 

patient.drugs.drug.23.day_of_form_completion:
  18 NA's 
   1  598 

patient.drugs.drug.23.days_to_drug_therapy_end:
1583 NA's 
   1  598 

patient.drugs.drug.23.days_to_drug_therapy_start:
1194 NA's 
   1  598 

patient.drugs.drug.23.drug_name:
avastin    NA's 
      1     598 

patient.drugs.drug.23.month_of_form_completion:
   6 NA's 
   1  598 

patient.drugs.drug.23.prescribed_dose:
  10 NA's 
   1  598 

patient.drugs.drug.23.prescribed_dose_units:
  mg NA's 
   1  598 

patient.drugs.drug.23.regimen_indication:
recurrence       NA's 
         1        598 

patient.drugs.drug.23.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              598 

patient.drugs.drug.23.therapy_ongoing:
  no NA's 
   1  598 

patient.drugs.drug.23.therapy_types.therapy_type:
targeted molecular therapy                       NA's 
                         1                        598 

patient.drugs.drug.23.total_dose:
18900  NA's 
    1   598 

patient.drugs.drug.23.total_dose_units:
  mg NA's 
   1  598 

patient.drugs.drug.23.year_of_form_completion:
2010 NA's 
   1  598 

patient.drugs.drug.24.bcr_drug_barcode:
tcga-12-3644-d23783                NA's 
                  1                 598 

patient.drugs.drug.24.bcr_drug_uuid:
0e157e59-e867-420c-9dd5-3a6b49c3a65a                                 NA's 
                                   1                                  598 

patient.drugs.drug.24.day_of_form_completion:
  18 NA's 
   1  598 

patient.drugs.drug.24.days_to_drug_therapy_end:
 179 NA's 
   1  598 

patient.drugs.drug.24.days_to_drug_therapy_start:
 134 NA's 
   1  598 

patient.drugs.drug.24.drug_name:
ccnu NA's 
   1  598 

patient.drugs.drug.24.month_of_form_completion:
   6 NA's 
   1  598 

patient.drugs.drug.24.number_cycles:
   1 NA's 
   1  598 

patient.drugs.drug.24.prescribed_dose:
 110 NA's 
   1  598 

patient.drugs.drug.24.prescribed_dose_units:
  mg NA's 
   1  598 

patient.drugs.drug.24.regimen_indication:
adjuvant     NA's 
       1      598 

patient.drugs.drug.24.route_of_administrations.route_of_administration:
oral NA's 
   1  598 

patient.drugs.drug.24.therapy_ongoing:
  no NA's 
   1  598 

patient.drugs.drug.24.therapy_types.therapy_type:
chemotherapy         NA's 
           1          598 

patient.drugs.drug.24.total_dose:
 190 NA's 
   1  598 

patient.drugs.drug.24.total_dose_units:
  mg NA's 
   1  598 

patient.drugs.drug.24.year_of_form_completion:
2010 NA's 
   1  598 

patient.drugs.drug.25.bcr_drug_barcode:
tcga-12-3644-d23785                NA's 
                  1                 598 

patient.drugs.drug.25.bcr_drug_uuid:
ccb25cca-36d9-4ff0-81fb-0f7fed05fce7                                 NA's 
                                   1                                  598 

patient.drugs.drug.25.day_of_form_completion:
  18 NA's 
   1  598 

patient.drugs.drug.25.days_to_drug_therapy_end:
 891 NA's 
   1  598 

patient.drugs.drug.25.days_to_drug_therapy_start:
 861 NA's 
   1  598 

patient.drugs.drug.25.drug_name:
temodar    NA's 
      1     598 

patient.drugs.drug.25.month_of_form_completion:
   6 NA's 
   1  598 

patient.drugs.drug.25.number_cycles:
   1 NA's 
   1  598 

patient.drugs.drug.25.prescribed_dose:
 200 NA's 
   1  598 

patient.drugs.drug.25.prescribed_dose_units:
  mg NA's 
   1  598 

patient.drugs.drug.25.regimen_indication:
recurrence       NA's 
         1        598 

patient.drugs.drug.25.route_of_administrations.route_of_administration:
oral NA's 
   1  598 

patient.drugs.drug.25.therapy_ongoing:
  no NA's 
   1  598 

patient.drugs.drug.25.therapy_types.therapy_type:
chemotherapy         NA's 
           1          598 

patient.drugs.drug.25.total_dose:
1700 NA's 
   1  598 

patient.drugs.drug.25.total_dose_units:
  mg NA's 
   1  598 

patient.drugs.drug.25.year_of_form_completion:
2010 NA's 
   1  598 

patient.drugs.drug.26.bcr_drug_barcode:
tcga-12-3644-d23789                NA's 
                  1                 598 

patient.drugs.drug.26.bcr_drug_uuid:
cc42c290-fa86-4ad5-b0a0-c6db17608271                                 NA's 
                                   1                                  598 

patient.drugs.drug.26.day_of_form_completion:
  18 NA's 
   1  598 

patient.drugs.drug.26.days_to_drug_therapy_end:
1611 NA's 
   1  598 

patient.drugs.drug.26.days_to_drug_therapy_start:
1583 NA's 
   1  598 

patient.drugs.drug.26.drug_name:
hydroxyuerea         NA's 
           1          598 

patient.drugs.drug.26.month_of_form_completion:
   6 NA's 
   1  598 

patient.drugs.drug.26.number_cycles:
   1 NA's 
   1  598 

patient.drugs.drug.26.prescribed_dose:
 500 NA's 
   1  598 

patient.drugs.drug.26.prescribed_dose_units:
  mg NA's 
   1  598 

patient.drugs.drug.26.regimen_indication:
recurrence       NA's 
         1        598 

patient.drugs.drug.26.route_of_administrations.route_of_administration:
oral NA's 
   1  598 

patient.drugs.drug.26.therapy_ongoing:
  no NA's 
   1  598 

patient.drugs.drug.26.therapy_types.therapy_type:
chemotherapy         NA's 
           1          598 

patient.drugs.drug.26.total_dose:
14000  NA's 
    1   598 

patient.drugs.drug.26.total_dose_units:
  mg NA's 
   1  598 

patient.drugs.drug.26.year_of_form_completion:
2010 NA's 
   1  598 

patient.drugs.drug.27.bcr_drug_barcode:
tcga-12-3644-d23786                NA's 
                  1                 598 

patient.drugs.drug.27.bcr_drug_uuid:
f608e57b-5a19-4c7b-a0ca-2374cc403f4c                                 NA's 
                                   1                                  598 

patient.drugs.drug.27.day_of_form_completion:
  18 NA's 
   1  598 

patient.drugs.drug.27.days_to_drug_therapy_end:
1142 NA's 
   1  598 

patient.drugs.drug.27.days_to_drug_therapy_start:
1030 NA's 
   1  598 

patient.drugs.drug.27.drug_name:
temodar    NA's 
      1     598 

patient.drugs.drug.27.month_of_form_completion:
   6 NA's 
   1  598 

patient.drugs.drug.27.number_cycles:
   7 NA's 
   1  598 

patient.drugs.drug.27.prescribed_dose:
  50 NA's 
   1  598 

patient.drugs.drug.27.prescribed_dose_units:
  mg NA's 
   1  598 

patient.drugs.drug.27.regimen_indication:
recurrence       NA's 
         1        598 

patient.drugs.drug.27.route_of_administrations.route_of_administration:
oral NA's 
   1  598 

patient.drugs.drug.27.therapy_ongoing:
  no NA's 
   1  598 

patient.drugs.drug.27.therapy_types.therapy_type:
chemotherapy         NA's 
           1          598 

patient.drugs.drug.27.total_dose:
10080  NA's 
    1   598 

patient.drugs.drug.27.total_dose_units:
  mg NA's 
   1  598 

patient.drugs.drug.27.year_of_form_completion:
2010 NA's 
   1  598 

patient.drugs.drug.28.bcr_drug_barcode:
tcga-12-3644-d23806                NA's 
                  1                 598 

patient.drugs.drug.28.bcr_drug_uuid:
122e2df6-bcb0-4e33-b374-23e17d9de4ae                                 NA's 
                                   1                                  598 

patient.drugs.drug.28.day_of_form_completion:
  18 NA's 
   1  598 

patient.drugs.drug.28.days_to_drug_therapy_end:
1722 NA's 
   1  598 

patient.drugs.drug.28.days_to_drug_therapy_start:
1639 NA's 
   1  598 

patient.drugs.drug.28.drug_name:
acctuane     NA's 
       1      598 

patient.drugs.drug.28.month_of_form_completion:
   6 NA's 
   1  598 

patient.drugs.drug.28.prescribed_dose:
  80 NA's 
   1  598 

patient.drugs.drug.28.prescribed_dose_units:
  mg NA's 
   1  598 

patient.drugs.drug.28.regimen_indication:
recurrence       NA's 
         1        598 

patient.drugs.drug.28.route_of_administrations.route_of_administration:
oral NA's 
   1  598 

patient.drugs.drug.28.therapy_ongoing:
  no NA's 
   1  598 

patient.drugs.drug.28.therapy_types.therapy_type:
targeted molecular therapy                       NA's 
                         1                        598 

patient.drugs.drug.28.total_dose:
1680 NA's 
   1  598 

patient.drugs.drug.28.total_dose_units:
  mg NA's 
   1  598 

patient.drugs.drug.28.year_of_form_completion:
2010 NA's 
   1  598 



patient.drugs.drug.3.clinical_trail_drug_classification:
chemotherapy         NA's 
           1          598 

patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   11.00   16.50   16.38   24.00   31.00     371 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   128.0   257.5   370.0   434.2  3942.0     415 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -15.0    46.0   126.0   230.6   277.0  2530.0     390 


patient.drugs.drug.3.measure_of_response:
clinical progressive disease               stable disease 
                           3                            4 
                        NA's 
                         592 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   6.057   9.000  12.000     371 

patient.drugs.drug.3.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.500   1.000   2.000   4.058   4.000  75.000     470 



patient.drugs.drug.3.regimen_indication:
               adjuvant other: specify in notes             progression 
                    109                       3                      67 
             recurrence                    NA's 
                     27                     393 

patient.drugs.drug.3.regimen_indication_notes:
adjuvant and progression             maintainence              maintenance 
                       1                        1                        1 
                    NA's 
                     596 

patient.drugs.drug.3.regimen_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   2.304   3.000   5.000     576 

patient.drugs.drug.3.route_of_administrations.route_of_administration:
         intratumoral      intravenous (iv)                  oral 
                    2                    56                   103 
other (specify below)                  NA's 
                   10                   428 

patient.drugs.drug.3.therapy_ongoing:
  no  yes NA's 
 173   17  409 

patient.drugs.drug.3.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                       170                         18 
             immunotherapy targeted molecular therapy 
                         4                         33 
                      NA's 
                       374 

patient.drugs.drug.3.total_dose:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
     0.4    110.0    357.5   8053.1   1850.0 603500.0      451 


patient.drugs.drug.3.tx_on_clinical_trial:
  no  yes NA's 
  11    2  586 

patient.drugs.drug.3.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2009    2010    2010    2011    2015     371 



patient.drugs.drug.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   11.00   17.00   16.68   24.00   31.00     452 

patient.drugs.drug.4.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   174.2   319.0   386.3   473.0  2614.0     481 

patient.drugs.drug.4.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -11.0    72.0   175.0   266.2   351.0  2572.0     466 


patient.drugs.drug.4.measure_of_response:
clinical progressive disease                         NA's 
                           3                          596 

patient.drugs.drug.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.946   8.000  12.000     452 

patient.drugs.drug.4.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   3.595   4.000  30.000     515 

patient.drugs.drug.4.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0    10.0    75.0   233.9   255.0  4000.0     547 


patient.drugs.drug.4.regimen_indication:
               adjuvant other: specify in notes             progression 
                     70                       1                      45 
             recurrence                    NA's 
                     21                     462 

patient.drugs.drug.4.regimen_indication_notes:
adjuvant and progression                     NA's 
                       1                      598 

patient.drugs.drug.4.regimen_number:
   1    2    3    4 NA's 
   2    5    4    2  586 

patient.drugs.drug.4.route_of_administrations.route_of_administration:
         intratumoral      intravenous (iv)                  oral 
                    1                    34                    71 
other (specify below)                  NA's 
                    6                   487 

patient.drugs.drug.4.therapy_ongoing:
  no  yes NA's 
 114    9  476 

patient.drugs.drug.4.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                       108                          9 
             immunotherapy targeted molecular therapy 
                         2                         28 
                      NA's 
                       452 

patient.drugs.drug.4.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      4     100     255    6646    2600  216600     494 


patient.drugs.drug.4.tx_on_clinical_trial:
  no  yes NA's 
   6    2  591 

patient.drugs.drug.4.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2009    2010    2010    2011    2015     452 



patient.drugs.drug.5.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.75   17.00   16.69   24.00   30.00     503 

patient.drugs.drug.5.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   202.2   379.5   462.1   580.5  2228.0     519 

patient.drugs.drug.5.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -7.0   102.5   262.0   335.4   442.0  1986.0     512 


patient.drugs.drug.5.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   5.969   9.000  12.000     503 

patient.drugs.drug.5.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   3.688   5.000  22.000     538 

patient.drugs.drug.5.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0    75.0   125.0   186.8   200.0   949.0     569 


patient.drugs.drug.5.regimen_indication:
               adjuvant other: specify in notes             progression 
                     32                       1                      42 
             recurrence                    NA's 
                     14                     510 

patient.drugs.drug.5.regimen_indication_notes:
concurrent       NA's 
         1        598 

patient.drugs.drug.5.regimen_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    2.00    2.80    3.75    5.00     589 

patient.drugs.drug.5.route_of_administrations.route_of_administration:
     intravenous (iv)                  oral other (specify below) 
                   19                    45                     4 
                 NA's 
                  531 

patient.drugs.drug.5.therapy_ongoing:
  no  yes NA's 
  78    3  518 

patient.drugs.drug.5.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                        73                          5 
             immunotherapy targeted molecular therapy 
                         2                         16 
                      NA's 
                       503 

patient.drugs.drug.5.total_dose:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
     0.4     93.8    210.0  13803.3   1545.0 828000.0      531 


patient.drugs.drug.5.tx_on_clinical_trial:
  no NA's 
   5  594 

patient.drugs.drug.5.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2009    2010    2010    2010    2015     503 



patient.drugs.drug.6.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     8.5    17.0    16.4    24.0    30.0     532 

patient.drugs.drug.6.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   303.5   390.0   533.3   600.0  2114.0     536 

patient.drugs.drug.6.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -5.0   182.5   300.0   402.9   489.5  1873.0     532 


patient.drugs.drug.6.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   6.209   9.000  12.000     532 

patient.drugs.drug.6.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   3.345   4.000  14.000     557 

patient.drugs.drug.6.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0    25.0   160.0   295.1   200.0  2000.0     575 

patient.drugs.drug.6.prescribed_dose_units:
{wafer}      mg   mg/kg   mg/m2    NA's 
      2      26       5      21     545 

patient.drugs.drug.6.regimen_indication:
   adjuvant progression  recurrence        NA's 
         25          30           8         536 

patient.drugs.drug.6.regimen_number:
   1    3    4 NA's 
   3    2    2  592 

patient.drugs.drug.6.route_of_administrations.route_of_administration:
         intratumoral      intravenous (iv)                  oral 
                    1                    16                    33 
other (specify below)                  NA's 
                    2                   547 

patient.drugs.drug.6.therapy_ongoing:
  no  yes NA's 
  61    1  537 

patient.drugs.drug.6.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                        45                          7 
             immunotherapy targeted molecular therapy 
                         2                         13 
                      NA's 
                       532 

patient.drugs.drug.6.total_dose:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
      2.6     100.0     200.0   22713.6    1550.0 1045500.0       549 

patient.drugs.drug.6.total_dose_units:
{wafer}      mg   mg/kg   mg/m2    NA's 
      2      32       3      17     545 

patient.drugs.drug.6.tx_on_clinical_trial:
  no NA's 
   3  596 

patient.drugs.drug.6.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2009    2010    2010    2011    2013     532 



patient.drugs.drug.7.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.50   18.00   17.04   24.00   30.00     548 

patient.drugs.drug.7.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   136.0   385.0   407.8   570.0  1468.0     550 

patient.drugs.drug.7.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -7.0    30.0   232.0   285.9   460.5  1307.0     548 


patient.drugs.drug.7.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   6.274   8.500  12.000     548 

patient.drugs.drug.7.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.655   4.000  12.000     570 

patient.drugs.drug.7.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.25   22.00  140.00  286.01  295.00 2000.00     580 


patient.drugs.drug.7.regimen_indication:
   adjuvant progression  recurrence        NA's 
         29          14           3         553 

patient.drugs.drug.7.regimen_number:
   1    2    4 NA's 
   4    1    1  593 


patient.drugs.drug.7.route_of_administrations.route_of_administration.2:
intravenous (iv)             NA's 
               1              598 

patient.drugs.drug.7.therapy_ongoing:
  no NA's 
  44  555 

patient.drugs.drug.7.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                        31                          5 
             immunotherapy targeted molecular therapy 
                         1                         14 
                      NA's 
                       548 

patient.drugs.drug.7.total_dose:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
     8.0     73.5    150.0  18266.9   1685.0 654000.0      559 


patient.drugs.drug.7.tx_on_clinical_trial:
  no NA's 
   3  596 

patient.drugs.drug.7.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2009    2010    2010    2011    2013     548 



patient.drugs.drug.8.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    10.0    17.0    17.3    24.0    30.0     562 

patient.drugs.drug.8.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   60.0   281.8   434.5   587.0   705.8  1817.0     563 

patient.drugs.drug.8.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   15.0   171.0   355.0   533.1   585.0  2259.0     562 


patient.drugs.drug.8.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.000   6.000   6.757   9.000  12.000     562 

patient.drugs.drug.8.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    2.00    3.76    5.00   19.00     574 

patient.drugs.drug.8.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   81.25  132.50  179.64  190.00  750.00     585 

patient.drugs.drug.8.prescribed_dose_units:
    mg mg/day  mg/kg  mg/m2   NA's 
    18      1      1     11    568 

patient.drugs.drug.8.regimen_indication:
   adjuvant progression  recurrence        NA's 
         15          15           5         564 

patient.drugs.drug.8.regimen_number:
   1    3    4    6 NA's 
   2    2    1    1  593 

patient.drugs.drug.8.route_of_administrations.route_of_administration:
     intravenous (iv)                  oral other (specify below) 
                    8                    22                     1 
                 NA's 
                  568 

patient.drugs.drug.8.therapy_ongoing:
  no  yes NA's 
  34    1  564 

patient.drugs.drug.8.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                        26                          2 
targeted molecular therapy                       NA's 
                         9                        562 

patient.drugs.drug.8.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0   140.0   200.6  6898.9  3200.0 66750.0     569 

patient.drugs.drug.8.total_dose_units:
    mg mg/day  mg/kg  mg/m2   NA's 
    22      1      1      7    568 

patient.drugs.drug.8.tx_on_clinical_trial:
  no NA's 
   2  597 

patient.drugs.drug.8.year_of_form_completion:
2009 2010 2011 2012 NA's 
  18    8    8    3  562 



patient.drugs.drug.9.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    10.5    17.5    17.2    24.0    29.0     579 

patient.drugs.drug.9.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   22.0   190.8   392.5   475.6   622.0  1639.0     579 

patient.drugs.drug.9.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   22.0   136.8   343.5   402.9   471.5  1611.0     579 


patient.drugs.drug.9.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    4.00    5.00    5.80    6.25   11.00     579 

patient.drugs.drug.9.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   3.000   2.727   3.500   7.000     588 

patient.drugs.drug.9.prescribed_dose:
  10  110  125  200 NA's 
   1    1    1    1  595 

patient.drugs.drug.9.prescribed_dose_units:
  mci    mg mg/m2  NA's 
    2    11     5   581 

patient.drugs.drug.9.regimen_indication:
   adjuvant progression  recurrence        NA's 
         11           7           1         580 

patient.drugs.drug.9.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
               6                6              587 

patient.drugs.drug.9.therapy_ongoing:
  no  yes NA's 
  17    1  581 

patient.drugs.drug.9.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                        11                          2 
targeted molecular therapy                       NA's 
                         7                        579 

patient.drugs.drug.9.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     20     110     210    1259    1038   14000     581 

patient.drugs.drug.9.total_dose_units:
  mci    mg mg/m2  NA's 
    2    11     5   581 

patient.drugs.drug.9.tx_on_clinical_trial:
  no NA's 
   1  598 

patient.drugs.drug.9.year_of_form_completion:
2009 2010 2011 2012 NA's 
  14    4    1    1  579 



patient.drugs.drug.clinical_trail_drug_classification:
chemotherapy         NA's 
           1          598 

patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.00   16.00   15.84   24.00   31.00     158 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -124.0    72.0   149.5   284.0   329.2  3942.0     239 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -260.0    24.0    52.0   168.9   142.5  3735.0     185 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                           2                            2 
              stable disease                         NA's 
                           5                          590 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   6.104   9.000  12.000     158 

patient.drugs.drug.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.500   1.000   2.000   3.613   5.000  30.000     321 




patient.drugs.drug.regimen_indication_notes:
 adjuvant and progression                concurrent concurrent chemoradiation 
                        3                         4                         2 
 peri-operative placement                      NA's 
                        1                       589 

patient.drugs.drug.regimen_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.815   2.000   5.000     572 


patient.drugs.drug.route_of_administrations.route_of_administration.2:
intravenous (iv)             NA's 
               1              598 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
 356   30  213 

patient.drugs.drug.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                       359                         37 
             immunotherapy targeted molecular therapy 
                         6                         39 
                      NA's 
                       158 

patient.drugs.drug.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1     120     280    5363    2112  444000     308 


patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
  10    2  587 

patient.drugs.drug.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2009    2010    2010    2011    2014     158 

patient.eastern_cancer_oncology_group:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   1.000   1.206   1.000   4.000     526 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    13                    489                     97 



patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   13.00   15.00   17.99   22.00   31.00     530 

patient.follow_ups.follow_up.2.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  105.0   395.0   517.0   746.7   811.2  2681.0     557 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   77.0   267.0   436.0   652.2   812.5  2768.0     568 

patient.follow_ups.follow_up.2.eastern_cancer_oncology_group:
   0    1 NA's 
   1    2  596 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             1                             39 
                          NA's 
                           559 

patient.follow_ups.follow_up.2.followup_treatment_success:
progressive disease                NA's 
                  5                 594 

patient.follow_ups.follow_up.2.karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  50.00   70.00   90.00   83.64  100.00  100.00     555 

patient.follow_ups.follow_up.2.lost_follow_up:
  no NA's 
  11  588 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    7.00    6.58    9.00   12.00     530 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
   3    2  594 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
   3    2  594 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
 324  357  396  984 NA's 
   1    1    1    1  595 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 356  382  647 NA's 
   1    1    1  596 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
locoregional disease                 NA's 
                   4                  595 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   4  595 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   6   32  561 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  17   21  561 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     91     338     670     513     711     755     594 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   41.0   177.5   260.0   400.6   433.0  2517.0     559 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
  locoregional disease progression of disease             recurrence 
                     5                     13                      6 
                  NA's 
                   575 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   5  594 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   1    3  595 

patient.follow_ups.follow_up.2.performance_status_scale_timing:
     adjuvant therapy post-adjuvant therapy  pre-adjuvant therapy 
                    1                    26                    11 
        pre-operative                  NA's 
                    6                   555 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
         5         54        540 

patient.follow_ups.follow_up.2.postoperative_rx_tx:
  no  yes NA's 
   3   54  542 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success:
progressive disease      stable disease                NA's 
                  3                   3                 593 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
   2   55  542 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
   27    42   530 

patient.follow_ups.follow_up.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2011    2011    2012    2012    2015     530 



patient.follow_ups.follow_up.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   11.25   21.00   17.60   24.00   24.00     589 

patient.follow_ups.follow_up.3.days_to_death:
 388  544  585  713 NA's 
   1    1    1    1  595 

patient.follow_ups.follow_up.3.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  415.0   612.0   640.0   656.7   678.5   951.0     593 

patient.follow_ups.follow_up.3.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             1                              3 
                          NA's 
                           595 

patient.follow_ups.follow_up.3.followup_treatment_success:
progressive disease                NA's 
                  1                 598 

patient.follow_ups.follow_up.3.karnofsky_performance_score:
  70 NA's 
   1  598 

patient.follow_ups.follow_up.3.lost_follow_up:
  no NA's 
   5  594 

patient.follow_ups.follow_up.3.month_of_form_completion:
   4    7    8   10 NA's 
   1    6    1    2  589 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   2    1  596 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
   2    1  596 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
 436 NA's 
   1  598 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
  93  436 NA's 
   1    1  597 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
  locoregional disease progression of disease                   NA's 
                     1                      1                    597 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   1  598 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment:
  no NA's 
   1  598 

patient.follow_ups.follow_up.3.performance_status_scale_timing:
post-adjuvant therapy                  NA's 
                    1                   598 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
         1          6        592 

patient.follow_ups.follow_up.3.postoperative_rx_tx:
  no  yes NA's 
   1    8  590 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success:
progressive disease                NA's 
                  1                 598 

patient.follow_ups.follow_up.3.radiation_therapy:
  no  yes NA's 
   1    8  590 

patient.follow_ups.follow_up.3.vital_status:
alive  dead  NA's 
    5     4   590 

patient.follow_ups.follow_up.3.year_of_form_completion:
2011 2012 2013 NA's 
   3    1    6  589 

patient.follow_ups.follow_up.4.bcr_followup_barcode:
tcga-32-2638-f30533 tcga-32-4209-f29186                NA's 
                  1                   1                 597 

patient.follow_ups.follow_up.4.bcr_followup_uuid:
582490c2-7f5a-4c8a-86f0-f9ccd82a418f e2426165-cae0-4437-9921-f92d85381384 
                                   1                                    1 
                                NA's 
                                 597 

patient.follow_ups.follow_up.4.day_of_form_completion:
   3    5 NA's 
   1    1  597 

patient.follow_ups.follow_up.4.days_to_death:
 618  766 NA's 
   1    1  597 

patient.follow_ups.follow_up.4.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             1                              1 
                          NA's 
                           597 

patient.follow_ups.follow_up.4.month_of_form_completion:
   3    4 NA's 
   1    1  597 

patient.follow_ups.follow_up.4.person_neoplasm_cancer_status:
with tumor       NA's 
         2        597 

patient.follow_ups.follow_up.4.postoperative_rx_tx:
  no NA's 
   1  598 

patient.follow_ups.follow_up.4.radiation_therapy:
  no NA's 
   1  598 

patient.follow_ups.follow_up.4.vital_status:
dead NA's 
   2  597 

patient.follow_ups.follow_up.4.year_of_form_completion:
2012 NA's 
   2  597 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   15.50   14.56   19.00   31.00      27 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0   154.0   360.0   479.5   575.5  3881.0     171 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   145.0   301.5   428.3   515.2  3881.0      47 

patient.follow_ups.follow_up.eastern_cancer_oncology_group:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.000   1.000   1.254   1.000   4.000     528 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason:
scheduled follow-up submission                           NA's 
                            30                            569 

patient.follow_ups.follow_up.followup_treatment_success:
complete remission/response         progressive disease 
                          1                           5 
             stable disease                        NA's 
                          3                         590 

patient.follow_ups.follow_up.karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   72.50   80.00   76.99   80.00  100.00     181 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
  14    3  582 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000  10.000   8.645  12.000  12.000      27 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
  no  yes NA's 
  27   88  484 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
  97   24  478 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -144.0   158.2   277.0   390.1   469.0  1936.0     491 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 100  188  342  407 NA's 
   1    1    1    1  595 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
locoregional disease           recurrence                 NA's 
                 121                    3                  475 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
 yes NA's 
 120  479 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    2  596 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no NA's 
   3  596 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_additional_surgery_procedure:
 142 NA's 
   1  598 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
 100  342 NA's 
   1    1  597 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
metastatic       NA's 
         1        598 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   1  598 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
 129  200  270 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
 311   55  233 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0   165.0   324.0   449.7   415.5  1643.0     592 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0   100.5   194.0   299.8   341.5  2339.0     233 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
  locoregional disease progression of disease             recurrence 
                     8                    252                    103 
                  NA's 
                   236 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   8  591 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   4    4  591 

patient.follow_ups.follow_up.performance_status_scale_timing:
                other post-adjuvant therapy  pre-adjuvant therapy 
                   28                    76                   124 
        pre-operative                  NA's 
                  122                   249 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        28        487         84 

patient.follow_ups.follow_up.postoperative_rx_tx:
  no  yes NA's 
  69  453   77 

patient.follow_ups.follow_up.primary_therapy_outcome_success:
complete remission/response         progressive disease 
                          1                           6 
             stable disease                        NA's 
                          4                         588 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
  70  482   47 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
  143   428    28 

patient.follow_ups.follow_up.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2008    2008    2009    2009    2010    2015      27 

patient.gender:
female   male   NA's 
   230    365      4 

patient.histological_type:
  glioblastoma multiforme (gbm)             treated primary gbm 
                             31                              20 
untreated primary (de novo) gbm                            NA's 
                            544                               4 

patient.history_of_neoadjuvant_treatment:
  no  yes NA's 
 573   21    5 


patient.icd_o_3_histology:
9440/3   NA's 
   595      4 


patient.informed_consent_verified:
 yes NA's 
 595    4 


patient.karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   70.00   80.00   77.15   80.00  100.00     161 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   9.000   8.563  12.000  12.000       4 

patient.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
 yes NA's 
   2  597 

patient.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no NA's 
   2  597 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 100  256 1550 NA's 
   1    1    1  596 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
recurrence       NA's 
         2        597 

patient.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
 yes NA's 
   1  598 

patient.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no NA's 
   1  598 

patient.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_additional_surgery_procedure:
 270 NA's 
   1  598 

patient.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
locoregional disease                 NA's 
                   1                  598 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   1    2  596 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   3  596 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   46.0   100.0   256.0   464.2   369.0  1550.0     594 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
progression of disease             recurrence                   NA's 
                     4                      1                    594 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   3    1  595 


patient.performance_status_scale_timing:
                other post-adjuvant therapy  pre-adjuvant therapy 
                   27                    76                   134 
        pre-operative                  NA's 
                  132                   230 

patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        30        508         61 

patient.postoperative_rx_tx:
  no  yes NA's 
   5   20  574 

patient.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                          2                           2 
        progressive disease              stable disease 
                          5                           6 
                       NA's 
                        584 

patient.prior_dx:
                              no yes, history of prior malignancy 
                              30                                1 
                            NA's 
                             568 

patient.prior_glioma:
  no  yes NA's 
 580   15    4 

patient.race:
                    asian black or african american                     white 
                       13                        51                       506 
                     NA's 
                       29 

patient.radiation_therapy:
  no  yes NA's 
   5   19  575 

patient.radiations.radiation.2.anatomic_treatment_site:
 distant recurrence    local recurrence primary tumor field       regional site 
                  2                  23                  49                   3 
               NA's 
                522 



patient.radiations.radiation.2.course_number:
   1    2 NA's 
   3   14  582 

patient.radiations.radiation.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   11.00   17.00   16.77   22.00   31.00     517 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   16.0    67.0   113.0   278.5   368.0  1906.0     526 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0    27.5    88.0   254.0   355.2  1906.0     523 

patient.radiations.radiation.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00    7.00    7.61   11.75   12.00     517 

patient.radiations.radiation.2.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     3.0     8.0    12.7    23.0    60.0     534 

patient.radiations.radiation.2.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     18    1800    3200    3401    5940    8000     528 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no  yes NA's 
  73    1  525 

patient.radiations.radiation.2.radiation_type:
               external           external beam other: specify in notes 
                      2                      48                      27 
          radioisotopes                    NA's 
                      1                     521 


patient.radiations.radiation.2.regimen_indication:
               adjuvant other: specify in notes             progression 
                     42                       1                      32 
             recurrence                    NA's 
                      4                     520 


patient.radiations.radiation.2.units:
 cgy NA's 
  72  527 

patient.radiations.radiation.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2008    2009    2011    2010    2011    2013     517 

patient.radiations.radiation.3.anatomic_treatment_site:
   local recurrence primary tumor field                NA's 
                  7                   8                 584 



patient.radiations.radiation.3.course_number:
   1    2    3 NA's 
   1    1    1  596 

patient.radiations.radiation.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   11.00   19.00   17.41   23.00   29.00     582 

patient.radiations.radiation.3.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   53.0    67.5   324.0   375.5   539.5  1367.0     584 

patient.radiations.radiation.3.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0    25.5   324.0   359.9   539.5  1367.0     584 

patient.radiations.radiation.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   8.000   7.588  10.000  12.000     582 

patient.radiations.radiation.3.numfractions:
   1    3   30 NA's 
   7    1    6  585 

patient.radiations.radiation.3.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     18    1450    1800    2629    4400    6000     584 

patient.radiations.radiation.3.radiation_treatment_ongoing:
  no NA's 
  13  586 

patient.radiations.radiation.3.radiation_type:
               external           external beam other: specify in notes 
                      3                       4                       8 
          radioisotopes                    NA's 
                      1                     583 


patient.radiations.radiation.3.regimen_indication:
   adjuvant progression  recurrence        NA's 
          4           5           5         585 

patient.radiations.radiation.3.units:
 cgy NA's 
  14  585 

patient.radiations.radiation.3.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2008    2010    2011    2011    2011    2013     582 

patient.radiations.radiation.4.anatomic_treatment_site:
   local recurrence primary tumor field       regional site                NA's 
                  2                   2                   1                 594 



patient.radiations.radiation.4.course_number:
   2    4 NA's 
   1    1  597 

patient.radiations.radiation.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     14      16      17      19      22      26     594 

patient.radiations.radiation.4.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   16.0    68.0   159.0   224.6   324.0   556.0     594 

patient.radiations.radiation.4.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   16.0    22.0   159.0   215.4   324.0   556.0     594 

patient.radiations.radiation.4.month_of_form_completion:
   3    8    9   12 NA's 
   1    1    2    1  594 

patient.radiations.radiation.4.numfractions:
   1    4   34 NA's 
   3    1    1  594 

patient.radiations.radiation.4.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     20    1600    1800    2652    3900    5940     594 

patient.radiations.radiation.4.radiation_treatment_ongoing:
  no NA's 
   5  594 

patient.radiations.radiation.4.radiation_type:
          external beam other: specify in notes                    NA's 
                      1                       3                     595 

patient.radiations.radiation.4.radiation_type_notes:
 cyberknife gamma knife         srs        NA's 
          1           1           1         596 

patient.radiations.radiation.4.regimen_indication:
  adjuvant palliative recurrence       NA's 
         2          1          2        594 

patient.radiations.radiation.4.regimen_indication_notes:
2.54 cc    NA's 
      1     598 

patient.radiations.radiation.4.units:
 cgy NA's 
   5  594 

patient.radiations.radiation.4.year_of_form_completion:
2008 2010 2011 NA's 
   1    1    3  594 

patient.radiations.radiation.5.anatomic_treatment_site:
primary tumor field                NA's 
                  1                 598 

patient.radiations.radiation.5.bcr_radiation_barcode:
tcga-28-5214-r45677                NA's 
                  1                 598 

patient.radiations.radiation.5.bcr_radiation_uuid:
9961d747-6570-49d7-b010-1966ff5cd5cf                                 NA's 
                                   1                                  598 

patient.radiations.radiation.5.day_of_form_completion:
  24 NA's 
   1  598 

patient.radiations.radiation.5.days_to_radiation_therapy_end:
  67 NA's 
   1  598 

patient.radiations.radiation.5.days_to_radiation_therapy_start:
  20 NA's 
   1  598 

patient.radiations.radiation.5.month_of_form_completion:
   7 NA's 
   1  598 

patient.radiations.radiation.5.numfractions:
  30 NA's 
   1  598 

patient.radiations.radiation.5.radiation_dosage:
6000 NA's 
   1  598 

patient.radiations.radiation.5.radiation_treatment_ongoing:
  no NA's 
   1  598 

patient.radiations.radiation.5.radiation_type:
external     NA's 
       1      598 

patient.radiations.radiation.5.units:
 cgy NA's 
   1  598 

patient.radiations.radiation.5.year_of_form_completion:
2013 NA's 
   1  598 

patient.radiations.radiation.anatomic_treatment_site:
       distant site    local recurrence primary tumor field       regional site 
                  1                  13                 417                   1 
               NA's 
                167 



patient.radiations.radiation.course_number:
   1    2 NA's 
  23    4  572 

patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    6.00   14.00   14.33   22.00   31.00     155 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -271.0    64.0    74.0   122.7    88.0  3716.0     175 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-314.00   21.00   31.00   82.75   47.00 3667.00     176 

patient.radiations.radiation.measure_of_response:
               complete response radiographic progressive disease 
                               4                                2 
                  stable disease                             NA's 
                               6                              587 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.00    6.00    6.93   11.00   12.00     155 

patient.radiations.radiation.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   30.00   30.00   28.13   30.00   60.00     222 

patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     18    5800    6000    5097    6000    9700     199 

patient.radiations.radiation.radiation_treatment_ongoing:
  no  yes NA's 
 398    2  199 





patient.radiations.radiation.units:
 cgy   gy  mci NA's 
 386    7    4  202 

patient.radiations.radiation.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2008    2009    2010    2010    2011    2015     155 

patient.tissue_prospective_collection_indicator:
  no  yes NA's 
   6   15  578 

patient.tissue_retrospective_collection_indicator:
  no  yes NA's 
  14    7  578 


patient.tumor_tissue_site:
brain  NA's 
  595     4 

patient.vital_status:
alive  dead  NA's 
  147   446     6 

patient.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2008    2008    2009    2009    2010    2015       4 

patient.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1989    2002    2006    2005    2009    2013       4 


patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2:
primary tumor and recurrent tumor(s)                                 NA's 
                                   1                                  598 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus    non-canonical             NA's 
             496               15               84                4 

patient.biospecimen_cqcf.consent_or_death_status:
consented  deceased      NA's 
       69        29       501 

patient.biospecimen_cqcf.country:
united states          NA's 
           18           581 

patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -14.0    78.5   239.5   506.9   676.8  2506.0     509 

patient.biospecimen_cqcf.days_to_sample_procurement:
   0 NA's 
  18  581 

patient.biospecimen_cqcf.digital_image_submitted:
  no  yes NA's 
  17    1  581 

patient.biospecimen_cqcf.ethnicity:
not hispanic or latino                   NA's 
                    19                    580 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site:
brain  NA's 
   99   500 

patient.biospecimen_cqcf.histological_type:
glioblastoma multiforme (gbm)                          NA's 
                           99                           500 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
  no NA's 
  98  501 

patient.biospecimen_cqcf.method_of_sample_procurement:
gross total resection    subtotal resection                  NA's 
                   10                     8                   581 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -8.000   0.000   0.000   3.882   0.000  58.000     582 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
   0.0278    0.1425    0.3290  114.6289    0.7686 1326.6000       536 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  15.00   20.00   20.00   33.27   20.77  242.88     537 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  17.80   40.62   84.08  156.73  189.25 1000.00     537 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
blood draw       NA's 
        17        582 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
   1 NA's 
  81  518 

patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
 yes NA's 
  18  581 

patient.biospecimen_cqcf.path_confirm_report_attached:
  no  yes NA's 
   8   87  504 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
  no  yes NA's 
   3   92  504 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
  no  yes NA's 
   3   92  504 

patient.biospecimen_cqcf.race:
black or african american                     white                      NA's 
                       17                         2                       580 

patient.biospecimen_cqcf.sample_prescreened:
 yes NA's 
  98  501 

patient.biospecimen_cqcf.site_of_disease:
brain  NA's 
   99   500 

patient.biospecimen_cqcf.submitted_for_lce:
  no NA's 
  18  581 

patient.biospecimen_cqcf.top_slide_submitted:
  no  yes NA's 
   1   17  581 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.bcr_sample_uuid:
242c50b9-5167-466e-aef6-aebe03e100fb                                 NA's 
                                   1                                  598 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
  15 NA's 
   1  598 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
  85 NA's 
   1  598 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
  98 NA's 
   1  598 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   1 NA's 
   1  598 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0    10.0    11.2    10.0    50.0     516 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  60.00   80.00   90.00   87.83  100.00  100.00     516 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   55.0   200.0   300.0   269.8   338.5   500.0     581 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
   1    2 NA's 
  89    1  509 

patient.biospecimen_cqcf.vessel_used:
cassette cryomold cryovial     NA's 
       3        1       14      581 

patient.days_to_index:
   0 NA's 
 595    4 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
primary tumor and recurrent tumor(s)                                 NA's 
                                  12                                  587 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   27.0   107.0   282.0   433.4   450.0  2271.0     582 

patient.samples.sample.2.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.3000  0.6500  0.8000  0.7467  0.8500  1.0000     584 

patient.samples.sample.2.is_ffpe:
  no NA's 
 532   67 

patient.samples.sample.2.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   1.000   1.100   1.093   1.250   1.500     584 

patient.samples.sample.2.oct_embedded:
false  NA's 
   17   582 



patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.700   1.800   1.800   1.819   1.820   1.970     577 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 96504895  96505239  97515722  98850023 103961240 103961312       507 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000   8.000   8.037   8.000  10.000     220 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.5000  0.5000  0.4891  0.5000  0.5000     220 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    4.00   14.00   14.04   23.00   29.00     220 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 379  220 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   6.066   9.000  12.000     220 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   8.000   6.897   9.000  12.000     220 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  189.0   294.0   424.0   539.4   644.0  1295.0     220 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0    30.0    30.0   102.8   200.0   200.0     220 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
 379  220 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    60.0    60.0   206.3   400.0   400.0     220 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2007 2008 2009 2010 NA's 
  93   90  104   92  220 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 96505470  96508744  97512936  98840883 103927544 103927616       507 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   9.000   9.000   8.755   9.000   9.000     220 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.5000  0.5000  0.4891  0.5000  0.5000     220 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    4.00   14.00   14.04   23.00   29.00     220 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 379  220 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   6.066   9.000  12.000     220 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   8.000   6.897   9.000  12.000     220 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  190.0   295.0   425.0   540.8   646.0  1296.0     220 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.12   30.00   30.00  102.95  200.00  200.00     220 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22 NA's 
 379  220 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    60.0    60.0   206.4   400.0   400.0     220 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2007 2008 2009 2010 NA's 
  93   90  104   92  220 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   5.229   8.000  10.000     468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.5000  0.5000  0.4739  0.5000  0.5000     468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   12.00   13.00   16.89   22.00   29.00     468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 131  468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   5.084   9.000  12.000     468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.00    6.00    6.26    9.00   12.00     468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    207     254     291     292     320     677     468 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.72   14.00   20.00  100.37  200.00  200.00     468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.source_center:
  22 NA's 
 131  468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0    28.0    40.0   202.6   400.0   400.0     468 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2007 2008 2009 NA's 
  67   52   12  468 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.biospecimen_barcode_bottom:
1053965773 1053965850       NA's 
         1          1        597 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.center_id:
   1    2    8 NA's 
   2    1   11  585 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.concentration:
0.16 0.18 0.19  0.5 NA's 
   1    1    1   11  585 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.day_of_shipment:
   6   27 NA's 
  11    3  585 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  14  585 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.month_of_shipment:
   1    2    6 NA's 
  11    1    2  585 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.000   8.500   7.286   9.000  10.000     585 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.plate_id:
 326  424 1849 NA's 
   1   11    2  585 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.quantity:
3.84 5.99 6.33  200 NA's 
   1    1    1   11  585 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.source_center:
  22 NA's 
  14  585 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.volume:
  24 33.3  400 NA's 
   1    2   11  585 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.year_of_shipment:
2008 2009 2011 NA's 
   1   11    2  585 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.center_id:
   9 NA's 
  11  588 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.concentration:
 0.5 NA's 
  11  588 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.day_of_shipment:
   6 NA's 
  11  588 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
  11  588 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.month_of_shipment:
   1 NA's 
  11  588 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.plate_column:
   6    8    9   10 NA's 
   3    1    5    2  588 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.plate_id:
 425 NA's 
  11  588 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.quantity:
 200 NA's 
  11  588 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.source_center:
  22 NA's 
  11  588 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.volume:
 400 NA's 
  11  588 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.year_of_shipment:
2009 NA's 
  11  588 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 9.649e+07 9.752e+07 2.046e+08 1.039e+08 1.062e+09       478 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000  10.000   8.234  10.000  10.000     189 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.5000  0.5000  0.4779  0.5000  0.5000     189 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    4.00   13.00   13.65   22.00   29.00     189 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 410  189 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.215   9.000  12.000     189 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   8.000   6.844   9.000  12.000     189 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.87   30.00   30.00   74.51  200.00  200.00     189 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
 393   17  189 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   60.00   60.00  149.82  400.00  400.00     189 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2008    2009    2009    2010    2014     189 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0     0.0   138.6    48.0  1020.0     208 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
genomeplex (rubicon) amplified dna               repli-g (qiagen) dna 
                                16                                375 
                               rna                               NA's 
                                19                                189 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
   g    h    r    w NA's 
  16    1   18  375  189 



patient.samples.sample.2.portions.portion.analytes.analyte.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               4              595 

patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0200  0.5000  0.5000  0.4763  0.5000  0.5000     206 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
 391  208 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no  yes NA's 
 374   17  208 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             410 
                                            NA's 
                                             189 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 410  189 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
   allprep rna extraction                 genomplex mirvana (allprep dna) rna 
                       18                        16                         1 
                  repli-g                      NA's 
                      375                       189 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.110   1.200   1.205   1.500   1.700     580 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   7.600   8.200   7.759   8.600   9.600     582 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
    393     206 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence:
   1 NA's 
   7  592 

patient.samples.sample.2.portions.portion.analytes.analyte.3.a260_a280_ratio:
 1.8 NA's 
   1  598 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
107025634      NA's 
        1       598 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   7    8   10 NA's 
   1   15   22  561 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
0.17  0.5 NA's 
   1   37  561 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  12.00   14.25   19.00   17.34   19.00   27.00     561 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  38  561 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    7.00   10.00    8.21   11.00   11.00     561 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.000   6.000   6.000   6.395   7.000  10.000     561 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  224.0   250.0   410.0   379.1   410.0  1847.0     561 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
2.26    5  200 NA's 
   1   19   18  561 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
  38  561 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  10 13.3  400 NA's 
  19    1   18  561 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2007 2008 2011 NA's 
  11   26    1  561 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.biospecimen_barcode_bottom:
113627806      NA's 
        1       598 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.center_id:
   7    9 NA's 
   1   37  561 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.concentration:
0.17  0.5 NA's 
   1   37  561 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  12.00   14.25   19.00   17.45   19.00   31.00     561 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  38  561 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   7.000  10.000   8.263  11.000  11.000     561 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.000   6.000   6.316   7.000  10.000     561 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  225.0   252.0   411.0   383.9   411.0  1991.0     561 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.quantity:
2.26    5  200 NA's 
   1   19   18  561 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.source_center:
  22 NA's 
  38  561 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.volume:
  10 13.3  400 NA's 
  19    1   18  561 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.year_of_shipment:
2007 2008 2011 NA's 
  11   26    1  561 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.center_id:
   2    8 NA's 
  15    3  581 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.concentration:
 0.5 NA's 
  18  581 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.day_of_shipment:
  12   13   18 NA's 
   3    7    8  581 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  18  581 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.month_of_shipment:
   2    7    9 NA's 
   7    8    3  581 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00    6.00    7.00    7.00    7.75   10.00     581 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.plate_id:
 226  254  320 NA's 
   8    3    7  581 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.quantity:
  14  200 NA's 
  15    3  581 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.source_center:
  22 NA's 
  18  581 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.volume:
  28  400 NA's 
  15    3  581 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.year_of_shipment:
2007 2008 NA's 
  11    7  581 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
107027458      NA's 
        1       598 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1       8       8       8      10      10     556 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.1 0.17 0.18  0.5 NA's 
   3    2    1   37  556 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   15.00   19.00   17.88   19.00   29.00     556 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  43  556 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   7.000   9.000   8.139  11.000  11.000     556 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   6.000   6.046   7.000  10.000     556 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  223.0   239.0   341.0   367.1   409.0  1846.0     556 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    5.00    5.00   73.18  200.00  200.00     556 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
  43  556 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  10   20   28  400 NA's 
  24    1    3   15  556 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2007 2008 2011 NA's 
  14   28    1  556 

patient.samples.sample.2.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   29.00   56.67  120.00  120.00     556 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                  rna            total rna 
                  37                    1                    5 
                NA's 
                 556 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id:
   r    t    w NA's 
   1    5   37  556 



patient.samples.sample.2.portions.portion.analytes.analyte.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               4              595 

patient.samples.sample.2.portions.portion.analytes.analyte.3.concentration:
 0.1 0.17  0.5 NA's 
   5    1   37  556 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
  37  562 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
  37  562 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              43 
                                            NA's 
                                             556 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
  43  556 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
allprep rna extraction                repli-g              total rna 
                     1                     37                      5 
                  NA's 
                   556 

patient.samples.sample.2.portions.portion.analytes.analyte.3.rna.ratio_28s_18s:
1.15 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.3.rna.rinvalue:
 7.3 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.3.spectrophotometer_method:
picogreen   uv spec      NA's 
       19        24       556 

patient.samples.sample.2.portions.portion.analytes.analyte.3.subportion_sequence:
   1 NA's 
   1  598 



patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.center_id:
   8   10 NA's 
   2    3  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
   5  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
   6   12   28 NA's 
   1    3    1  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   5  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
   1    5    9 NA's 
   1    1    3  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   1    2    6    7 NA's 
   1    1    2    1  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_id:
 250  348  424 NA's 
   3    1    1  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_row:
   b    c    d    g NA's 
   2    1    1    1  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.quantity:
 200 NA's 
   5  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.source_center:
  22 NA's 
   5  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.volume:
 400 NA's 
   5  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2007 2008 2009 NA's 
   3    1    1  594 



patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.center_id:
   9 NA's 
   5  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.concentration:
 0.5 NA's 
   5  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.day_of_shipment:
   6   12   28 NA's 
   1    3    1  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   5  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.month_of_shipment:
   1    5    9 NA's 
   1    1    3  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_column:
   1    2    6    7 NA's 
   1    1    2    1  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_id:
 252  349  425 NA's 
   3    1    1  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_row:
   b    c    d    g NA's 
   2    1    1    1  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.quantity:
 200 NA's 
   5  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.source_center:
  22 NA's 
   5  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.volume:
 400 NA's 
   5  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.year_of_shipment:
2007 2008 2009 NA's 
   3    1    1  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-06-0208-01b-01w-0254-08 tcga-06-0210-01b-01w-0254-08 
                           1                            1 
tcga-06-0211-01b-01w-0254-08                         NA's 
                           1                          596 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.bcr_aliquot_uuid:
0daf5fcc-50aa-4424-827c-a630fd00cea2 86ac3a1e-235b-441b-b3a4-b6072e746714 
                                   1                                    1 
f354a447-5e5b-457a-af45-8e22b02c2c9a                                 NA's 
                                   1                                  596 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.center_id:
   8 NA's 
   3  596 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.concentration:
 0.5 NA's 
   3  596 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.day_of_shipment:
  12 NA's 
   3  596 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   3  596 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.month_of_shipment:
   9 NA's 
   3  596 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.plate_column:
   6    7 NA's 
   2    1  596 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.plate_id:
 254 NA's 
   3  596 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.plate_row:
   b    d    g NA's 
   1    1    1  596 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.quantity:
 200 NA's 
   3  596 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.source_center:
  22 NA's 
   3  596 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.volume:
 400 NA's 
   3  596 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   3  596 



patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   2   10 NA's 
   3    2  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   5  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   6   12   28 NA's 
   1    3    1  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   5  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   1    5    9 NA's 
   1    1    3  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   1    2    6    7 NA's 
   1    1    2    1  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id:
 248  347  423 NA's 
   3    1    1  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_row:
   b    c    d    g NA's 
   2    1    1    1  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
  14  200 NA's 
   3    2  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
   5  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  28  400 NA's 
   3    2  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2007 2008 2009 NA's 
   3    1    1  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.amount:
   0 NA's 
   5  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                   5                  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.analyte_type_id:
   w NA's 
   5  594 



patient.samples.sample.2.portions.portion.analytes.analyte.4.concentration:
 0.5 NA's 
   5  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   5  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
   5  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               5 
                                            NA's 
                                             594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   5  594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      5     594 

patient.samples.sample.2.portions.portion.analytes.analyte.4.spectrophotometer_method:
uv spec    NA's 
      5     594 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0216-01b-01w-0410-10                         NA's 
                           1                          598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.bcr_aliquot_uuid:
97eb7513-198b-436f-9f6b-f265168c67f1                                 NA's 
                                   1                                  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.center_id:
  10 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.day_of_shipment:
  19 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.month_of_shipment:
  11 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_column:
   1 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_id:
 410 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_row:
   b NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.quantity:
   5 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.source_center:
  22 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.volume:
  10 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.year_of_shipment:
2008 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0216-01b-01w-0411-09                         NA's 
                           1                          598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.bcr_aliquot_uuid:
6b44e20a-1782-48ab-a68f-0030d45c657a                                 NA's 
                                   1                                  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.center_id:
   9 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.day_of_shipment:
  19 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.month_of_shipment:
  11 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.plate_column:
   1 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.plate_id:
 411 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.plate_row:
   b NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.quantity:
   5 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.source_center:
  22 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.volume:
  10 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.year_of_shipment:
2008 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0216-01b-01w-0409-08                         NA's 
                           1                          598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.bcr_aliquot_uuid:
ebce20f3-fb7d-43b7-ad02-e3c903f63e7c                                 NA's 
                                   1                                  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.center_id:
   8 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.day_of_shipment:
  19 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.month_of_shipment:
  11 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_column:
   1 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_id:
 409 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_row:
   b NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.quantity:
   5 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.source_center:
  22 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.volume:
  10 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.year_of_shipment:
2008 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.amount:
 120 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.analyte_type:
repli-g (qiagen) dna                 NA's 
                   1                  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.analyte_type_id:
   w NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.bcr_analyte_barcode:
tcga-06-0216-01b-01w                 NA's 
                   1                  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.bcr_analyte_uuid:
a35fc052-cfc4-4351-879b-9e184cbc3094                                 NA's 
                                   1                                  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.dna.normal_tumor_genotype_match:
 yes NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.dna.pcr_amplification_successful:
  no NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      1     598 

patient.samples.sample.2.portions.portion.analytes.analyte.5.spectrophotometer_method:
picogreen      NA's 
        1       598 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.700   1.890   1.900   1.888   1.900   2.240      87 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
8.957e+07 9.752e+07 1.087e+08 4.360e+08 1.050e+09 1.054e+09       361 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    4.00    3.49    4.00    8.00      77 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.070   0.150   0.150   0.165   0.160   0.500      77 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   13.00   20.00   19.06   25.00   30.00      77 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 522   77 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   5.768   8.000  12.000      77 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   6.885   9.000  12.000      77 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.600   3.000   3.470   3.603   4.010  52.500      77 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
 505   17   77 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.30   20.00   23.50   23.55   26.70  350.00      77 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2008    2009    2009    2010    2014      77 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
8.957e+07 9.751e+07 1.087e+08 4.896e+08 1.050e+09 1.054e+09       365 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.000   6.000   6.000   6.228   6.750   8.000     113 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1000  0.1500  0.1344  0.1600  0.5000     113 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   13.00   20.00   18.49   25.00   30.00     113 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 486  113 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   5.685   8.000  12.000     113 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   6.897   8.000  12.000     113 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  184.0   335.0   593.0   854.3  1486.0  2004.0     113 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   2.000   2.130   2.751   3.000   9.600     113 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
 486  113 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   13.30   13.30   26.11   40.00   60.00     113 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2008    2009    2009    2010    2011     113 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
8.958e+07 8.958e+07 9.752e+07 1.412e+08 1.039e+08 1.076e+09       486 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   2    6    8 NA's 
 288   34   38  239 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.050   0.140   0.150   0.152   0.160   0.500     239 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   13.00   18.00   18.15   24.00   30.00     239 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 360  239 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.500   5.000   5.269   8.000  10.000     239 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   6.619   9.000  12.000     239 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  185.0   313.0   520.0   619.5   787.0  2364.0     239 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.810   2.800   3.000   3.282   3.400   8.000     239 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
 360  239 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.20   20.00   20.00   23.93   20.00   60.00     239 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2008    2009    2009    2010    2012     239 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.754e+07 9.754e+07 1.050e+09 7.427e+08 1.050e+09 1.076e+09       386 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   8.000   6.169   8.000   9.000     339 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.050   0.050   0.050   0.096   0.150   0.230     339 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   3.000   8.935  19.000  29.000     339 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
 260  339 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   3.915   5.000  12.000     339 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   6.908   9.000  12.000     339 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.2000  0.6225  2.0000  1.8484  2.0000 16.5200     339 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22   23 NA's 
 259    1  339 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.33   40.00   26.89   40.00  118.00     339 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2010    2011    2010    2011    2013     339 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.651e+07 9.754e+07 1.050e+09 8.045e+08 1.050e+09 1.050e+09       502 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   1    2    8 NA's 
  26    1   72  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0500  0.0773  0.1400  0.1900     500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   3   10   19   29 NA's 
  72    1   24    2  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
  99  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   1    3    5    8 NA's 
  72    1   24    2  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    6.50    9.00    8.04   10.00   12.00     500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    237    1004    1492    1342    1493    1494     500 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.470   1.170   2.000   1.615   2.000   2.000     500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22 NA's 
  99  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
3.33  6.7   10   40 NA's 
  24    1    2   72  500 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2007 2010 2011 NA's 
   2   25   72  500 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.678e+08 1.050e+09 1.050e+09 9.744e+08 1.050e+09 1.076e+09       576 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   4    8    9 NA's 
   2   21    2  574 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0500  0.0604  0.0500  0.1500     574 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   3   21   29 NA's 
  20    2    3  574 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
  25  574 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   1    8 NA's 
  22    3  574 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.00    8.00    7.96   10.00   12.00     574 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   2.000   2.000   2.215   2.000   6.000     574 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  22   23 NA's 
  23    2  574 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
13.4   30   40 NA's 
   2    1   22  574 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2007 2011 2012 2015 NA's 
   2   20    1    2  574 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_barcode:
tcga-06-0147-10a-01d-0241-06 tcga-06-0148-10a-01d-0241-06 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_uuid:
778a08dc-f276-424c-9f80-b7e09800ce39 96ac7482-6308-4fbd-bdac-f984baee8f0c 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
   6 NA's 
   2  597 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
0.14 0.15 NA's 
   1    1  597 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
  29 NA's 
   2  597 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   8 NA's 
   2  597 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   1 NA's 
   2  597 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id:
 241 NA's 
   2  597 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_row:
   c    d NA's 
   1    1  597 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
 2.8    3 NA's 
   1    1  597 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  22 NA's 
   2  597 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
  20 NA's 
   2  597 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2007 NA's 
   2  597 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.129e+07 9.752e+07 1.040e+08 3.000e+08 1.604e+08 1.054e+09       360 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    4    6    8 NA's 
 491    6   34    1   67 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1500  0.1500  0.1671  0.1600  0.5000      67 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   13.00   20.00   19.11   25.00   30.00      67 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 532   67 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   5.688   8.000  12.000      67 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   7.000   6.851   9.000  12.000      67 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.050   1.000   1.010   1.426   1.130  15.000      67 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
 515   17   67 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.500   6.670   6.700   8.986   6.700 100.000      67 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2008    2009    2009    2010    2014      67 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   13.60   68.81  148.36  163.25 1800.00      67 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
                               dna genomeplex (rubicon) amplified dna 
                               502                                 28 
                               rna                               NA's 
                                 2                                 67 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
   d    g    r NA's 
 502   28    2   67 



patient.samples.sample.2.portions.portion.analytes.analyte.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               8              591 

patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0100  0.1400  0.1500  0.1671  0.1600  0.5000      67 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
 530   69 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
 168  362   69 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             532 
                                            NA's 
                                              67 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
 532   67 


patient.samples.sample.2.portions.portion.analytes.analyte.rna.ratio_28s_18s:
 1.2    2 NA's 
   1    1  597 

patient.samples.sample.2.portions.portion.analytes.analyte.rna.rinvalue:
 7.4 NA's 
   1  598 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
       20       512        67 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1 NA's 
   7  592 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   13.00   14.00   14.16   14.00   31.00      67 

patient.samples.sample.2.portions.portion.is_ffpe:
  no NA's 
 532   67 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   7.000   6.564   7.000  12.000      67 

patient.samples.sample.2.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.173   1.000  21.000      67 

patient.samples.sample.2.portions.portion.portion_sequence:
   1 NA's 
  14  585 




patient.samples.sample.2.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
  23  576 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   0   90  100 NA's 
   1    1    1  596 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   0   10 NA's 
   1    1  597 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    5.00   17.89   20.00   90.00     580 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   0 NA's 
   2  597 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_normal_cells:
   0   10   35 NA's 
  16    1    1  581 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_stromal_cells:
   0    5   10 NA's 
  12    1    5  581 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   66.25   90.00   76.11   93.75  100.00     581 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   77.50   90.00   82.89   97.50  100.00     580 

patient.samples.sample.2.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
     5     18    576 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  25  574 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
  90 NA's 
   1  598 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
  10 NA's 
   1  598 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   10.00   20.25   21.25   85.00     579 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   1  598 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   0   80 NA's 
  19    1  579 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
   0    5   10 NA's 
  15    2    3  579 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  15.00   45.00   85.00   71.25   96.25  100.00     579 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0    77.5    92.5    83.7   100.0   100.0     579 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
bottom    top   NA's 
    18      7    574 

patient.samples.sample.2.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00   35.50   50.35   75.75  147.00     579 

patient.samples.sample.2.portions.portion.year_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2010    2011    2011    2011    2014      67 


patient.samples.sample.2.portions.shipment_portion.center_id:
  20 NA's 
  10  589 

patient.samples.sample.2.portions.shipment_portion.is_ffpe:
  no NA's 
  10  589 

patient.samples.sample.2.portions.shipment_portion.plate_id:
2189 NA's 
  10  589 

patient.samples.sample.2.portions.shipment_portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   12.0    15.0    22.0    22.7    31.0    31.0     589 


patient.samples.sample.2.portions.shipment_portion.shipment_portion_day_of_shipment:
   5 NA's 
  10  589 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_month_of_shipment:
   3 NA's 
  10  589 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_year_of_shipment:
2012 NA's 
  10  589 

patient.samples.sample.2.sample_type:
blood derived normal        primary tumor      recurrent tumor 
                 482                   10                   11 
 solid tissue normal                 NA's 
                  29                   67 

patient.samples.sample.2.sample_type_id:
   1    2   10   11 NA's 
  10   11  482   29   67 

patient.samples.sample.2.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.2000  0.3000  0.5000  0.4933  0.6000  0.9000     584 

patient.samples.sample.2.vial_number:
   a    b    c NA's 
 488   29   15   67 

patient.samples.sample.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
primary tumor and recurrent tumor(s)                                 NA's 
                                   2                                  597 



patient.samples.sample.3.intermediate_dimension:
 0.6  1.2 NA's 
   1    1  597 

patient.samples.sample.3.is_ffpe:
  no NA's 
  21  578 

patient.samples.sample.3.longest_dimension:
 0.8  1.4 NA's 
   1    1  597 

patient.samples.sample.3.pathology_report_file_name:
tcga-06-0210.5617d8a4-bb27-4643-a3fd-8ddcf659cb3b.pdf 
                                                    1 
tcga-06-0211.528ded4e-c801-4f67-bb1e-62470b39b41e.pdf 
                                                    1 
                                                 NA's 
                                                  597 

patient.samples.sample.3.pathology_report_uuid:
528ded4e-c801-4f67-bb1e-62470b39b41e 5617d8a4-bb27-4643-a3fd-8ddcf659cb3b 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio:
 1.8 NA's 
   2  597 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
96504901     NA's 
       1      598 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   1    8   10 NA's 
   1   11    5  582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
 0.5 NA's 
  17  582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    6.00   12.00   13.59   19.00   28.00     582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  17  582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   9.000   7.471   9.000  12.000     582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.000   7.176  11.000  12.000     582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  224.0   251.0   323.0   414.2   611.0   837.0     582 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
6.65   30  200 NA's 
   1    6   10  582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
  17  582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
13.3   60  400 NA's 
   1    6   10  582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2007 2008 2009 2010 NA's 
   8    2    6    1  582 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
96508768     NA's 
       1      598 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   6    9 NA's 
   1   16  582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
 0.5 NA's 
  17  582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    6.00   12.00   13.59   19.00   28.00     582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  17  582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   9.000   7.471   9.000  12.000     582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.000   7.176  11.000  12.000     582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  225.0   253.0   324.0   415.8   613.0   839.0     582 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
13.35    30   200  NA's 
    1     6    10   582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22 NA's 
  17  582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
26.7   60  400 NA's 
   1    6   10  582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2007 2008 2009 2010 NA's 
   8    2    6    1  582 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.center_id:
   2    4    8 NA's 
   3    1    5  590 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.concentration:
 0.5 NA's 
   9  590 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
  12   18   19   27 NA's 
   5    2    1    1  590 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   9  590 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
   2    7    9   12 NA's 
   1    2    5    1  590 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   5.222   7.000  12.000     590 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  226.0   254.0   255.0   260.1   255.0   325.0     590 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.quantity:
  14   20  200 NA's 
   3    1    5  590 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.source_center:
  22 NA's 
   9  590 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.volume:
  28   40  400 NA's 
   3    1    5  590 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2007 2008 NA's 
   8    1  590 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
  96486592 1061604002 1061604025       NA's 
         1          1          1        596 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   1    2   10 NA's 
   2    6   11  580 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
0.16  0.5 NA's 
   2   17  580 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    3.00   12.00   12.47   18.50   28.00     580 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  19  580 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   9.000   7.737   9.000  12.000     580 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   7.000   6.526  10.500  12.000     580 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  223.0   249.0   347.0   580.2   625.0  2005.0     580 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.33   14.00   30.00   67.75  115.00  200.00     580 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
  19  580 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  28.00   30.65   60.00  137.89  230.00  400.00     580 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2007    2008    2008    2009    2011     580 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0       0       0     149      50    1020     580 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
genomeplex (rubicon) amplified dna               repli-g (qiagen) dna 
                                 1                                 16 
                               rna                               NA's 
                                 2                                580 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   g    r    w NA's 
   1    2   16  580 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
0.16  0.5 NA's 
   2   17  580 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
  17  582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no NA's 
  17  582 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              19 
                                            NA's 
                                             580 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
  19  580 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
allprep rna extraction              genomplex                repli-g 
                     2                      1                     16 
                  NA's 
                   580 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
 1.3  1.4 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue:
 8.4  8.5 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
     19     580 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0125-10a-01w-0224-08 tcga-06-0216-10a-01w-0348-08 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_uuid:
ccbd44f7-741f-4237-bfce-e6abc2fc1671 d41b9c4b-0f5e-4fa9-a504-46c13964b836 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   8 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
  18   28 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   5    7 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   6 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
 224  348 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_row:
   a    b NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
 200 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
 400 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2007 2008 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0125-10a-01w-0225-09 tcga-06-0216-10a-01w-0349-09 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_uuid:
7f0af751-dc78-42c2-8974-d3f073f179a7 b20b920f-8b18-4e4f-8c1d-8f9dfbc5864f 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.center_id:
   9 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.concentration:
 0.5 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.day_of_shipment:
  18   28 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.month_of_shipment:
   5    7 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_column:
   6 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_id:
 225  349 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_row:
   a    b NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.quantity:
 200 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.source_center:
  22 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.volume:
 400 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.year_of_shipment:
2007 2008 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-06-0125-10a-01w-0226-02                         NA's 
                           1                          598 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.bcr_aliquot_uuid:
50448941-2a2a-4a0c-b7ce-a88f65c1ff38                                 NA's 
                                   1                                  598 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.center_id:
   2 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.day_of_shipment:
  18 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.month_of_shipment:
   7 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.plate_column:
   6 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.plate_id:
 226 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.plate_row:
   b NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.quantity:
  14 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.source_center:
  22 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.volume:
  28 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0125-10a-01w-0223-10 tcga-06-0216-10a-01w-0347-10 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
0a6a2033-7651-4054-a9fe-b674ac6167bf 70c21e45-6606-4815-8df0-ac276b6980d5 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
  10 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
  18   28 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   5    7 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   6 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
 223  347 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_row:
   a    b NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
 200 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
 400 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2007 2008 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.amount:
   0 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                   2                  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.bcr_analyte_barcode:
tcga-06-0125-10a-01w tcga-06-0216-10a-01w                 NA's 
                   1                    1                  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.bcr_analyte_uuid:
86c184c5-fbf0-4fb1-a30b-1b71dba5a7bf 9699b730-00e6-4752-9d49-6e6435dc0f5b 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.concentration:
 0.5 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      2     597 

patient.samples.sample.3.portions.portion.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
      2     597 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0216-10a-01w-0410-10                         NA's 
                           1                          598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.bcr_aliquot_uuid:
93014d3e-9d50-41bb-9119-1735961217b6                                 NA's 
                                   1                                  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.center_id:
  10 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
  19 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
  11 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_id:
 410 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_row:
   f NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.quantity:
   5 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.source_center:
  22 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.volume:
  10 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2008 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0216-10a-01w-0411-09                         NA's 
                           1                          598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_uuid:
d27e7a9b-e407-44f4-8a2c-91d3eb5db3cb                                 NA's 
                                   1                                  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.center_id:
   9 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.day_of_shipment:
  19 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.month_of_shipment:
  11 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_id:
 411 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_row:
   f NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.quantity:
   5 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.source_center:
  22 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.volume:
  10 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.year_of_shipment:
2008 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0216-10a-01w-0409-08                         NA's 
                           1                          598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_uuid:
b0633227-4493-4722-a4cb-dace5c4931df                                 NA's 
                                   1                                  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   8 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
  19 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
  11 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id:
 409 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_row:
   f NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
   5 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  10 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2008 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.amount:
 120 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                   1                  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.analyte_type_id:
   w NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.bcr_analyte_barcode:
tcga-06-0216-10a-01w                 NA's 
                   1                  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.bcr_analyte_uuid:
9699b730-00e6-4752-9d49-6e6435dc0f5b                                 NA's 
                                   1                                  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      1     598 

patient.samples.sample.3.portions.portion.analytes.analyte.4.spectrophotometer_method:
picogreen      NA's 
        1       598 

patient.samples.sample.3.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.800   1.890   1.900   1.891   1.900   1.910     578 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.129e+07 1.054e+09 1.054e+09 8.614e+08 1.054e+09 1.054e+09       594 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    2    4 NA's 
   5    4   12  578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1500  0.1548  0.1700  0.1800     578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0    13.0    24.0    19.1    28.0    29.0     578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  21  578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   8.000   6.762   8.000  10.000     578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   5.762   8.000  11.000     578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  215.0   237.0   385.0   677.9   592.0  2003.0     578 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.300   2.800   3.740   3.371   4.010   5.000     578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
  21  578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   20.00   23.50   21.92   26.70   33.30     578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2007    2008    2009    2009    2011     578 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
8.957e+07 1.283e+08 1.054e+09 6.760e+08 1.054e+09 1.054e+09       594 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   4    5    6    8 NA's 
   5    2   12    2  578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1500  0.1438  0.1600  0.1800     578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   13.00   24.00   19.71   28.00   29.00     578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  21  578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   6.000   6.524   8.000  10.000     578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   5.762   8.000  11.000     578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  216.0   239.0   386.0   692.3   593.0  2280.0     578 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.130   3.164   3.000   6.800     578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
  21  578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.80   13.30   13.30   23.09   40.00   60.00     578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2007    2008    2009    2009    2012     578 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
  89576919  128286587  128286588 1049950527       NA's 
         1          1          1          1        595 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   2    6    8 NA's 
  11    5    3  580 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1500  0.1379  0.1600  0.1800     580 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   13.00   20.00   19.58   28.50   29.00     580 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  19  580 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   6.000   5.842   8.000   9.000     580 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.500   5.000   5.684   8.000  11.000     580 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  217.0   241.0   387.0   645.3   594.0  2280.0     580 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.995   3.000   3.210   3.400   5.000     580 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
  19  580 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  17.60   20.00   20.00   26.54   26.65   60.00     580 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2007    2008    2009    2009    2012     580 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.754e+07 1.050e+09 1.050e+09 9.004e+08 1.057e+09 1.076e+09       587 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   1    8    9   10 NA's 
   2    9    3    2  583 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0500  0.0850  0.1425  0.1800     583 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    3.00    6.00   11.12   16.75   29.00     583 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  16  583 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   4.500   3.875   5.500   8.000     583 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.500   5.375   8.250  12.000     583 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  333.0   880.5  1491.0  1409.4  1690.5  2366.0     583 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.570   1.500   2.000   4.224   2.000  20.000     583 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
  16  583 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.33   30.00   40.00   44.13   40.00  133.30     583 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2008    2010    2011    2011    2011    2012     583 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.754e+07 1.063e+09 1.076e+09 9.328e+08 1.076e+09 1.076e+09       592 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   1    4    8    9 NA's 
   1    1    1    6  590 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.05 0.14 0.17 0.18 NA's 
   6    1    1    1  590 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   3    6   19   29 NA's 
   1    2    1    5  590 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   9  590 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   1    5    8 NA's 
   1    3    5  590 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   3.778   4.000   9.000     590 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    334    1003    2366    1685    2366    2366     590 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.600   1.500   1.500   2.454   2.000   7.990     590 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22 NA's 
   9  590 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.33   30.00   30.00   30.44   30.00   57.10     590 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2008    2010    2012    2011    2012    2012     590 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.754e+07 1.508e+08 1.539e+08 3.225e+08 1.603e+08 1.050e+09       594 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   2    6    8 NA's 
   1    1    4  593 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0725  0.0850  0.1000  0.1275  0.1700     593 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.750   6.000   9.167  14.250  21.000     593 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   6  593 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   1    5   10 NA's 
   2    2    2  593 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   1    4    6 NA's 
   4    1    1  593 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_row:
   a    c    h NA's 
   2    1    3  593 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.9400  0.9725  1.4350  1.4600  1.9500  2.0100     593 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  22   23 NA's 
   3    3  593 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   12.20   13.40   17.55   18.35   40.00     593 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2008    2010    2012    2012    2013    2014     593 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_barcode:
tcga-06-0152-10a-01d-2366-09 tcga-06-0216-10a-01d-0336-02 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_uuid:
99f2308c-9c22-461f-af58-6a09f62ede38 cf57b176-b76b-4210-8afd-1ab3c8c92ff0 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
1076437049       NA's 
         1        598 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
   2    9 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
0.05 0.17 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
   6   29 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   5    8 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   3    4 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id:
 336 2366 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_row:
   f    h NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
 1.5 2.99 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  22 NA's 
   2  597 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
17.6   30 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2008 2012 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_barcode:
tcga-06-0216-10a-01d-1492-08                         NA's 
                           1                          598 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_uuid:
1e3c9b2d-1a94-4b4b-a3e9-967c77c6498d                                 NA's 
                                   1                                  598 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom:
1049951819       NA's 
         1        598 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.center_id:
   8 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.concentration:
0.05 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.day_of_shipment:
   3 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.month_of_shipment:
   1 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_column:
   8 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_id:
1492 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_row:
   d NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.quantity:
   2 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.source_center:
  22 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.volume:
  40 NA's 
   1  598 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.9.year_of_shipment:
2011 NA's 
   1  598 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.129e+07 1.054e+09 1.054e+09 8.614e+08 1.054e+09 1.054e+09       594 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    6 NA's 
  16    5  578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1500  0.1548  0.1700  0.1800     578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0    13.0    24.0    19.1    28.0    29.0     578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  21  578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   8.000   6.762   8.000  10.000     578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   5.762   8.000  11.000     578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  214.0   235.0   384.0   676.6   591.0  2002.0     578 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.010   1.070   2.161   2.000   5.660     578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22 NA's 
  21  578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.90    6.70    6.70   13.93   13.30   33.30     578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2007    2008    2009    2009    2011     578 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   32.87  133.30  166.13  232.40  600.00     578 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
  21  578 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
  21  578 



patient.samples.sample.3.portions.portion.analytes.analyte.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               1              598 

patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1500  0.1557  0.1700  0.1800     578 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
  21  578 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
   3   18  578 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              21 
                                            NA's 
                                             578 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
  21  578 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type     chemical lysis dna extraction 
                                2                                15 
pre-extracted dna received by tss                              NA's 
                                4                               578 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
     21     578 



patient.samples.sample.3.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   14.00   14.00   13.43   14.00   27.00     578 

patient.samples.sample.3.portions.portion.is_ffpe:
  no NA's 
  21  578 

patient.samples.sample.3.portions.portion.month_of_creation:
   2    4    7   10 NA's 
   2    2   15    2  578 

patient.samples.sample.3.portions.portion.portion_number:
   1    2 NA's 
  20    1  578 

patient.samples.sample.3.portions.portion.slides.slide.2.bcr_slide_barcode:
tcga-06-0210-02a-01-ts1 tcga-06-0211-02a-02-bs2                    NA's 
                      1                       1                     597 

patient.samples.sample.3.portions.portion.slides.slide.2.bcr_slide_uuid:
5e5f48ad-8612-49ea-980e-b583c4e724ec d4bd5992-f2b5-400c-9b84-24bd269b482c 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.3.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.3.portions.portion.slides.slide.2.percent_necrosis:
   5 NA's 
   1  598 

patient.samples.sample.3.portions.portion.slides.slide.2.percent_normal_cells:
   0 NA's 
   1  598 

patient.samples.sample.3.portions.portion.slides.slide.2.percent_stromal_cells:
  10 NA's 
   1  598 

patient.samples.sample.3.portions.portion.slides.slide.2.percent_tumor_cells:
  85 NA's 
   1  598 

patient.samples.sample.3.portions.portion.slides.slide.2.percent_tumor_nuclei:
  90 NA's 
   1  598 

patient.samples.sample.3.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
     1      1    597 

patient.samples.sample.3.portions.portion.slides.slide.bcr_slide_barcode:
tcga-06-0210-02a-01-bs1 tcga-06-0211-02a-01-bs1                    NA's 
                      1                       1                     597 

patient.samples.sample.3.portions.portion.slides.slide.bcr_slide_uuid:
2fb83c94-f310-4136-919f-8f483d697f9a 446598f7-75be-4c3f-805d-a20fa0643008 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis:
   0   10 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells:
   0 NA's 
   1  598 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells:
  10   20 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells:
  80 NA's 
   2  597 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei:
  80   90 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
bottom    top   NA's 
     1      1    597 

patient.samples.sample.3.portions.portion.weight:
 118  119 NA's 
   1    1  597 

patient.samples.sample.3.portions.portion.year_of_creation:
2009 2010 2011 NA's 
   2    1   18  578 

patient.samples.sample.3.portions.shipment_portion.bcr_shipment_portion_uuid:
f8f3fffc-4e71-4000-98f4-bd91286aabc3                                 NA's 
                                   1                                  598 

patient.samples.sample.3.portions.shipment_portion.center_id:
  20 NA's 
   1  598 

patient.samples.sample.3.portions.shipment_portion.is_ffpe:
  no NA's 
   1  598 

patient.samples.sample.3.portions.shipment_portion.plate_id:
2189 NA's 
   1  598 

patient.samples.sample.3.portions.shipment_portion.portion_number:
  21 NA's 
   1  598 

patient.samples.sample.3.portions.shipment_portion.shipment_portion_bcr_aliquot_barcode:
tcga-06-0210-02a-21-2189-20                        NA's 
                          1                         598 

patient.samples.sample.3.portions.shipment_portion.shipment_portion_day_of_shipment:
   5 NA's 
   1  598 

patient.samples.sample.3.portions.shipment_portion.shipment_portion_month_of_shipment:
   3 NA's 
   1  598 

patient.samples.sample.3.portions.shipment_portion.shipment_portion_year_of_shipment:
2012 NA's 
   1  598 

patient.samples.sample.3.sample_type:
blood derived normal      recurrent tumor                 NA's 
                  19                    2                  578 

patient.samples.sample.3.sample_type_id:
   2   10 NA's 
   2   19  578 

patient.samples.sample.3.shortest_dimension:
 0.3  0.4 NA's 
   1    1  597 

patient.samples.sample.3.vial_number:
   a    b    c NA's 
  17    2    2  578 

patient.samples.sample.4.bcr_sample_barcode:
tcga-06-0210-10a tcga-06-0211-10a tcga-19-0957-10b tcga-19-1389-10d 
               1                1                1                1 
            NA's 
             595 

patient.samples.sample.4.bcr_sample_uuid:
074a1516-3adc-41d0-84cb-2a5b13c44469 4b511d1f-f753-4d9b-b47b-fca98b8d3e03 
                                   1                                    1 
5c4ba77d-d7f4-4d66-b9a9-875167a063ae b48285a2-9d65-4c6e-ac58-555f87d5a9d2 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.4.is_ffpe:
  no NA's 
   4  595 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0210-10a-01w-0251-10 tcga-06-0211-10a-01w-0251-10 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
aabfa801-56a7-4de4-9bc8-f5f6bbf63ec9 f664a5bd-35f1-42e3-b1c6-f8de5ddb511e 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
  10 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
 0.5 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  12 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   9 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   1 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
 251 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   e    f NA's 
   1    1  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
 200 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
 400 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2007 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0210-10a-01w-0253-09 tcga-06-0211-10a-01w-0253-09 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid:
4e0a244d-fcba-4f9a-9eb8-ca0a1228014f dff013d9-2df1-412a-a84c-10984ca30b01 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   9 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
 0.5 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
  12 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   9 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   1 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
 253 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   e    f NA's 
   1    1  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
 200 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
 400 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2007 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-06-0210-10a-01w-0255-08 tcga-06-0211-10a-01w-0255-08 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_uuid:
c0bb8ae1-94be-47c8-89d6-8db33468ae5e e038c4b5-d174-41e7-9cc4-e4a65bc0623f 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.4.center_id:
   8 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.4.concentration:
 0.5 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
  12 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
   9 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   1 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_id:
 255 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_row:
   e    f NA's 
   1    1  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.4.quantity:
 200 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.4.source_center:
  22 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.4.volume:
 400 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0210-10a-01w-0249-02 tcga-06-0211-10a-01w-0249-02 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
5cebdc7d-af29-434c-ab62-8cac1c4ee77d 8b08e56a-34ad-4738-8618-065c69f04074 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   2 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
 0.5 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
  12 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   9 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   1 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
 249 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_row:
   e    f NA's 
   1    1  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
  14 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  28 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2007 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.amount:
   0 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                 NA's 
                   2                  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type_id:
   w NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.bcr_analyte_barcode:
tcga-06-0210-10a-01w tcga-06-0211-10a-01w                 NA's 
                   1                    1                  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.bcr_analyte_uuid:
74382ecd-7d5f-4107-9cc8-a33ff90568f9 89950671-f0b9-491e-8fcf-6ce9d812ae5b 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.concentration:
 0.5 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      2     597 

patient.samples.sample.4.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      2     597 

patient.samples.sample.4.portions.portion.analytes.analyte.a260_a280_ratio:
1.84 1.85 NA's 
   1    1  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0210-10a-01d-0237-01 tcga-06-0211-10a-01d-0237-01 
                           1                            1 
tcga-19-0957-10b-01d-2366-09 tcga-19-1389-10d-01d-2364-08 
                           1                            1 
                        NA's 
                         595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
0c575fce-e5b9-4386-9d3d-ea120a0104ba 1af2e986-70d4-4f60-a52b-134678fe5cee 
                                   1                                    1 
32c52390-7282-4b95-a3d7-37b3618fa579 fc515c66-0198-47fd-8892-4d0d13bf3f20 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
1076437048 1076438077       NA's 
         1          1        597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    8    9 NA's 
   2    1    1  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.05 0.18 0.19 NA's 
   2    1    1  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  29 NA's 
   4  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   8 NA's 
   4  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   4    5    6 NA's 
   1    2    1  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
 237 2364 2366 NA's 
   2    1    1  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_row:
   c    d    f NA's 
   1    2    1  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
 1.5  1.8  1.9 NA's 
   2    1    1  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
   4  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
  10   30 NA's 
   2    2  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2007 2012 NA's 
   2    2  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0210-10a-01d-0239-04 tcga-06-0211-10a-01d-0239-04 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
0d1a973a-ad05-43c5-aff3-bd12c94d3767 61c18d5d-5614-49f9-b151-f21b701e8b2b 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   4 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.18 0.19 NA's 
   1    1  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  29 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   8 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   5 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
 239 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_row:
   c    d NA's 
   1    1  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
 7.2  7.6 NA's 
   1    1  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  40 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2007 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-06-0210-10a-01d-0241-06 tcga-06-0211-10a-01d-0241-06 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
2908c4e3-c13d-4985-a8a9-d114169817e3 37068265-58a8-41ba-a74e-dee3f07df111 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   6 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.18 0.19 NA's 
   1    1  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  29 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   8 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   5 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
 241 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_row:
   c    d NA's 
   1    1  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
 3.6  3.8 NA's 
   1    1  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  20 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-06-0210-10a-01d-1491-08 tcga-06-0211-10a-01d-1491-08 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
4e2a4226-999f-4cca-bf37-17d54b7f8a96 ebf97220-6d4d-4016-bae3-ec279144c19d 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
1049950207 1049950214       NA's 
         1          1        597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   8 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.05 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   3 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   1 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   9 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
1491 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row:
   e    f NA's 
   1    1  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   2 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
  40 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2011 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-06-0210-10a-01d-2366-09 tcga-06-0211-10a-01d-2366-09 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
316f3270-3f5f-4afe-863f-adae4e2a2ff0 7f87df9f-3bdb-4e1f-a03a-34950bea5486 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
1076436983 1076437000       NA's 
         1          1        597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   9 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.05 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  29 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   8 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   4 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
2366 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   a    b NA's 
   1    1  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
 1.5 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
  30 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2012 NA's 
   2  597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0210-10a-01d-0235-06 tcga-06-0211-10a-01d-0235-06 
                           1                            1 
tcga-19-0957-10b-01d-2319-08 tcga-19-1389-10d-01d-2319-08 
                           1                            1 
                        NA's 
                         595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
049b7f10-33b9-4669-b314-a4ea3905dd6b 2681881b-e46f-4b42-9ef7-c5765943c0ac 
                                   1                                    1 
416dc626-fb75-43cc-91e1-226ac90f70ff 6fb55d66-7c23-4f4f-9937-b88bc9a36c61 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
1061615908 1061615918       NA's 
         1          1        597 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   6    8 NA's 
   2    2  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.05 0.18 0.19 NA's 
   2    1    1  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
  13   29 NA's 
   2    2  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   6    8 NA's 
   2    2  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   5    8 NA's 
   2    2  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
 235 2319 NA's 
   2    2  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_row:
   c    d NA's 
   2    2  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   1 5.99 6.33 NA's 
   2    1    1  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22 NA's 
   4  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.volume:
  20 33.3 NA's 
   2    2  595 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2007 2012 NA's 
   2    2  595 

patient.samples.sample.4.portions.portion.analytes.analyte.amount:
   0    5 12.7 41.7 NA's 
   1    1    1    1  595 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
   4  595 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
   4  595 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_barcode:
tcga-06-0210-10a-01d tcga-06-0211-10a-01d tcga-19-0957-10b-01d 
                   1                    1                    1 
tcga-19-1389-10d-01d                 NA's 
                   1                  595 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_uuid:
06c14955-1a5d-4e15-bfbf-be1509ee7369 27e2bbf3-1aaa-4df6-b234-ecc4ba137509 
                                   1                                    1 
88bdaf86-b464-4ff7-a549-8c90c9369c46 f37b3486-b71d-4a2f-9e53-96ecd11eff98 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               1              598 

patient.samples.sample.4.portions.portion.analytes.analyte.concentration:
0.13 0.15 0.18 0.19 NA's 
   1    1    1    1  595 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   4  595 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
   2    2  595 

patient.samples.sample.4.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                             595 

patient.samples.sample.4.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.4.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
    chemical lysis dna extraction pre-extracted dna received by tss 
                                2                                 2 
                             NA's 
                              595 

patient.samples.sample.4.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
        2         2       595 

patient.samples.sample.4.portions.portion.bcr_portion_barcode:
tcga-06-0210-10a-01 tcga-06-0211-10a-01 tcga-19-0957-10b-01 tcga-19-1389-10d-01 
                  1                   1                   1                   1 
               NA's 
                595 

patient.samples.sample.4.portions.portion.bcr_portion_uuid:
16e648f6-3d2a-45b6-a90d-3457fb93ecb9 6832bba5-dede-42e0-a4af-76156ddab09e 
                                   1                                    1 
a1cfc043-d951-4e28-bffc-d9e6fed18dea d34197ab-8b0e-4c6d-a136-fd5bbf51241b 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.4.portions.portion.day_of_creation:
  14   29 NA's 
   2    2  595 

patient.samples.sample.4.portions.portion.is_ffpe:
  no NA's 
   4  595 

patient.samples.sample.4.portions.portion.month_of_creation:
   5    7 NA's 
   2    2  595 

patient.samples.sample.4.portions.portion.portion_number:
   1 NA's 
   4  595 

patient.samples.sample.4.portions.portion.year_of_creation:
2011 2012 NA's 
   2    2  595 

patient.samples.sample.4.sample_type:
blood derived normal                 NA's 
                   4                  595 

patient.samples.sample.4.sample_type_id:
  10 NA's 
   4  595 

patient.samples.sample.4.vial_number:
   a    b    d NA's 
   2    1    1  595 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   27.0   107.0   282.0   433.4   450.0  2271.0     582 









patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.9:
8620b410-9d16-418e-88e1-b75c020da50a                                 NA's 
                                   1                                  598 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   90.0   160.0   260.0   319.4   410.0   800.0     582 

patient.samples.sample.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.2000  0.6000  0.8000  0.7496  0.9000  2.0000      93 

patient.samples.sample.is_ffpe:
  no NA's 
 595    4 

patient.samples.sample.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   0.900   1.000   1.123   1.300   3.000      93 

patient.samples.sample.oct_embedded:
false  true  NA's 
    9     8   582 



patient.samples.sample.portions.portion.2.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.800   1.802   1.810   1.816   1.817   1.870     589 



patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   6.000   6.000   6.455   7.000  10.000     588 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1500  0.1500  0.1600  0.2218  0.1800  0.5000     588 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  12   28   29 NA's 
   1    3    7  588 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  11  588 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   5    6    8    9 NA's 
   1    2    7    1  588 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.818   3.000   8.000     588 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  213.0   231.0   231.0   240.2   231.5   348.0     588 


patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.755   3.200  38.755   3.600 200.000     588 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
  11  588 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.volume:
13.3 16.7   20  400 NA's 
   2    1    6    2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2007 2008 NA's 
  10    1  588 



patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.center_id:
   1    3    9 NA's 
   3    6    2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1500  0.1500  0.1600  0.2218  0.1800  0.5000     588 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
  12   28   29 NA's 
   1    3    7  588 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  11  588 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   5    6    8    9 NA's 
   1    2    7    1  588 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.818   3.000   8.000     588 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  219.0   232.0   232.0   242.2   232.5   349.0     588 


patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.510   2.755   3.010  39.145   5.000 200.000     588 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22 NA's 
  11  588 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.volume:
16.7   27 33.3  400 NA's 
   6    1    2    2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2007 2008 NA's 
  10    1  588 



patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.center_id:
   1    2    8 NA's 
   6    2    1  590 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1400  0.1500  0.1600  0.1978  0.1800  0.5000     590 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
  12   28   29 NA's 
   1    2    6  590 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   9  590 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
   6    8    9 NA's 
   2    6    1  590 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   1    2    3    8 NA's 
   3    2    3    1  590 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.plate_id:
 220  233  254 NA's 
   2    6    1  590 


patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.80    5.00    5.33   26.53    5.99  200.00     590 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.source_center:
  22 NA's 
   9  590 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.volume:
  20 33.3  400 NA's 
   2    6    1  590 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   9  590 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-06-0138-01a-02r-0676-04 tcga-06-0156-01a-02r-1849-01 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.bcr_aliquot_uuid:
88958da0-8140-466c-8183-bf4995529b16 b72bfbb6-a5a9-47bb-900e-446f1db880ae 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.biospecimen_barcode_bottom:
1053965784       NA's 
         1        598 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.center_id:
   1    4 NA's 
   1    1  597 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.concentration:
0.15 NA's 
   2  597 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.day_of_shipment:
  27   28 NA's 
   1    1  597 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.month_of_shipment:
   6   10 NA's 
   1    1  597 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.plate_column:
   3    7 NA's 
   1    1  597 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.plate_id:
 676 1849 NA's 
   1    1  597 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.plate_row:
   b    e NA's 
   1    1  597 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.quantity:
 0.6    5 NA's 
   1    1  597 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.source_center:
  22 NA's 
   2  597 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.volume:
   4 33.3 NA's 
   1    1  597 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.5.year_of_shipment:
2009 2011 NA's 
   1    1  597 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-06-0138-01a-02r-1849-01                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.bcr_aliquot_uuid:
6f2fedb4-66e2-48fd-8744-cbd30075bb03                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.biospecimen_barcode_bottom:
1053965828       NA's 
         1        598 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.center_id:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.day_of_shipment:
  27 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.plate_column:
   2 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.plate_id:
1849 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.plate_row:
   e NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.quantity:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.volume:
33.3 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.6.year_of_shipment:
2011 NA's 
   1  598 



patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.center_id:
   2    3    7   10 NA's 
   1    2    8    1  587 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0300  0.1500  0.1550  0.2058  0.1800  0.5000     587 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
  12   28   29 NA's 
   1    3    8  587 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  12  587 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   5    6    8    9 NA's 
   1    2    8    1  587 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.500   3.167   3.750   8.000     587 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_id:
 212  229  248  347 NA's 
   2    8    1    1  587 


patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.400   2.000   2.325  19.885   4.287 200.000     587 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
  12  587 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.30   13.30   13.30   49.56   27.25  400.00     587 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2007 2008 NA's 
  11    1  587 

patient.samples.sample.portions.portion.2.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    3.75   15.00   30.93   34.60  150.00     587 

patient.samples.sample.portions.portion.2.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                   2                   10                  587 

patient.samples.sample.portions.portion.2.analytes.analyte.2.analyte_type_id:
   r    w NA's 
  10    2  587 



patient.samples.sample.portions.portion.2.analytes.analyte.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               8              591 

patient.samples.sample.portions.portion.2.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.030   0.150   0.155   0.205   0.180   0.500     587 

patient.samples.sample.portions.portion.2.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   2  597 

patient.samples.sample.portions.portion.2.analytes.analyte.2.dna.pcr_amplification_successful:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.2.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              12 
                                            NA's 
                                             587 

patient.samples.sample.portions.portion.2.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
  12  587 

patient.samples.sample.portions.portion.2.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
allprep rna extraction                repli-g                   NA's 
                    10                      2                    587 

patient.samples.sample.portions.portion.2.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.80    1.00    1.15    1.26    1.55    1.80     589 

patient.samples.sample.portions.portion.2.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.800   8.050   8.250   8.338   8.525   9.300     591 

patient.samples.sample.portions.portion.2.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
     12     587 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0147-01a-02w-0250-10 tcga-06-0148-01a-03w-0250-10 
                           1                            1 
tcga-06-0171-01a-02w-0250-10                         NA's 
                           1                          596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_uuid:
3aac16fe-74c3-4944-8f5d-819f523c21db 5745dc85-3499-4ac1-8738-0a080b34e7d0 
                                   1                                    1 
df68f744-2bff-47a4-9335-b67230e088ed                                 NA's 
                                   1                                  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.center_id:
  10 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
  12 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   9 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   1    4 NA's 
   2    1  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.plate_id:
 250 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.plate_row:
   c    d    e NA's 
   1    1    1  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.quantity:
 200 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.volume:
 400 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2007 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0147-01a-02w-0252-09 tcga-06-0148-01a-03w-0252-09 
                           1                            1 
tcga-06-0171-01a-02w-0252-09                         NA's 
                           1                          596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_uuid:
0fb0697b-abe5-4358-a9d9-3952bcd59cf2 1f904917-e3be-4e0c-8552-ae996e4e327a 
                                   1                                    1 
66ba76bb-e38c-43d4-b8e2-4ed7e0a48509                                 NA's 
                                   1                                  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.center_id:
   9 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.concentration:
 0.5 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.day_of_shipment:
  12 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.month_of_shipment:
   9 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.plate_column:
   1    4 NA's 
   2    1  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.plate_id:
 252 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.plate_row:
   c    d    e NA's 
   1    1    1  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.quantity:
 200 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.source_center:
  22 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.volume:
 400 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.year_of_shipment:
2007 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-06-0147-01a-02w-0254-08 tcga-06-0148-01a-03w-0254-08 
                           1                            1 
tcga-06-0171-01a-02w-0254-08                         NA's 
                           1                          596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.bcr_aliquot_uuid:
71a8b744-72f8-40a3-a8c1-15259c19ee30 7a26926d-382a-4c65-96be-b46917f81a14 
                                   1                                    1 
7a5b216f-6e83-4647-b145-e05daa99e395                                 NA's 
                                   1                                  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.center_id:
   8 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.concentration:
 0.5 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.day_of_shipment:
  12 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.month_of_shipment:
   9 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.plate_column:
   1    4 NA's 
   2    1  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.plate_id:
 254 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.plate_row:
   c    d    e NA's 
   1    1    1  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.quantity:
 200 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.source_center:
  22 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.volume:
 400 NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   3  596 



patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.center_id:
   2    7 NA's 
   3    7  589 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.concentration:
 0.1  0.5 NA's 
   7    3  589 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
  12   28   29 NA's 
   3    2    5  589 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  10  589 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   6    8    9 NA's 
   2    5    3  589 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    2.50    3.10    3.75    8.00     589 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_id:
 213  230  248 NA's 
   2    5    3  589 


patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.quantity:
 0.8    1    2   14 NA's 
   1    5    1    3  589 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
  10  589 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.volume:
   8   10   20   28 NA's 
   1    5    1    3  589 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2007 NA's 
  10  589 

patient.samples.sample.portions.portion.2.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    2.00   17.78   10.00  100.00     590 

patient.samples.sample.portions.portion.2.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna            total rna                 NA's 
                   3                    7                  589 

patient.samples.sample.portions.portion.2.analytes.analyte.3.analyte_type_id:
   t    w NA's 
   7    3  589 



patient.samples.sample.portions.portion.2.analytes.analyte.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               7              592 

patient.samples.sample.portions.portion.2.analytes.analyte.3.concentration:
0.04  0.1 0.14  0.5 NA's 
   1    5    1    3  589 

patient.samples.sample.portions.portion.2.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
   3  596 

patient.samples.sample.portions.portion.2.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              10 
                                            NA's 
                                             589 

patient.samples.sample.portions.portion.2.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
  10  589 

patient.samples.sample.portions.portion.2.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g total rna      NA's 
        3         7       589 

patient.samples.sample.portions.portion.2.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
     10     589 



patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.center_id:
   8   10 NA's 
   2    5  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
  12   18 NA's 
   5    2  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
   7    9 NA's 
   2    5  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   1    2    4 NA's 
   1    3    3  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.plate_id:
 224  250 NA's 
   2    5  592 


patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.quantity:
 200 NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.source_center:
  22 NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.volume:
 400 NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2007 NA's 
   7  592 



patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.center_id:
   9 NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.concentration:
 0.5 NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.day_of_shipment:
  12   18 NA's 
   5    2  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.month_of_shipment:
   7    9 NA's 
   2    5  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.plate_column:
   1    2    4 NA's 
   1    3    3  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.plate_id:
 225  252 NA's 
   2    5  592 


patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.quantity:
 200 NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.source_center:
  22 NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.volume:
 400 NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.year_of_shipment:
2007 NA's 
   7  592 



patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.center_id:
   2    8 NA's 
   2    5  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.concentration:
 0.5 NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.day_of_shipment:
  12   18 NA's 
   5    2  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.month_of_shipment:
   7    9 NA's 
   2    5  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.plate_column:
   1    2    4 NA's 
   1    3    3  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.plate_id:
 226  254 NA's 
   2    5  592 


patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.quantity:
  14  200 NA's 
   2    5  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.source_center:
  22 NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.volume:
  28  400 NA's 
   2    5  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   7  592 



patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.center_id:
   2   10 NA's 
   5    2  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
  12   18 NA's 
   5    2  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   7    9 NA's 
   2    5  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_column:
   1    2    4 NA's 
   1    3    3  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_id:
 223  248 NA's 
   2    5  592 


patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.quantity:
  14  200 NA's 
   5    2  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.volume:
  28  400 NA's 
   5    2  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2007 NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.amount:
   0 NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                   7                  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.analyte_type_id:
   w NA's 
   7  592 



patient.samples.sample.portions.portion.2.analytes.analyte.4.concentration:
 0.5 NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               7 
                                            NA's 
                                             592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   7  592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      7     592 

patient.samples.sample.portions.portion.2.analytes.analyte.4.spectrophotometer_method:
uv spec    NA's 
      7     592 

patient.samples.sample.portions.portion.2.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.700   1.800   1.890   1.842   1.900   1.920     580 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.bcr_aliquot_barcode:
tcga-06-0171-01a-02d-a45w-05                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.bcr_aliquot_uuid:
136d09ca-cef1-48a1-a67a-fc833c565947                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.biospecimen_barcode_bottom:
167788594      NA's 
        1       598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.center_id:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.day_of_shipment:
  24 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.month_of_shipment:
   9 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.plate_column:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.plate_id:
a45w NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.plate_row:
   c NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.quantity:
1.95 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.source_center:
  23 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.volume:
  13 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.10.year_of_shipment:
2014 NA's 
   1  598 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
108691281 108692110 108692113 108692155      NA's 
        1         1         1         1       595 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   3.000   3.211   5.000   7.000     580 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1521  0.1650  0.2000     580 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   6   25   28   29 NA's 
   3    4    2   10  580 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  19  580 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   8.000   6.158   8.000   8.000     580 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.526   3.500   7.000     580 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  215.0   236.0   236.0   554.4   320.5  1700.0     580 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.300   1.500   1.700   2.284   2.340   5.000     580 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
  19  580 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   10.00   10.10   14.85   16.70   33.30     580 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2007 2008 2011 NA's 
  12    3    4  580 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.center_id:
   3    4    6    7 NA's 
   1    9    3    2  584 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1567  0.1700  0.2000     584 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   6   28   29 NA's 
   3    2   10  584 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  15  584 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   2    5    6    8 NA's 
   2    1    2   10  584 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.467   3.500   7.000     584 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  216.0   238.0   238.0   250.5   238.0   335.0     584 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.860   3.000   5.600   4.719   6.200   7.200     584 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
  15  584 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.80   18.35   40.00   30.34   40.00   40.00     584 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2007 2008 NA's 
  12    3  584 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.center_id:
   1    2    6 NA's 
   1    3    9  586 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1608  0.1700  0.2000     586 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   6   28   29 NA's 
   1    2   10  586 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  13  586 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   5    6    8 NA's 
   1    2   10  586 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   3.000   2.923   4.000   7.000     586 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_id:
 217  233  240  336 NA's 
   2    1    9    1  586 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   2.800   3.200   3.543   3.600   6.660     586 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
  13  586 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.volume:
 5.2   20 33.3 NA's 
   1    9    3  586 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2007 2008 NA's 
  12    1  586 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.center_id:
   5 NA's 
  12  587 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1475  0.1600  0.1575  0.1700  0.1800     587 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   6   28   29 NA's 
   1    2    9  587 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  12  587 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   5    6    8 NA's 
   1    2    9  587 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.583   3.250   7.000     587 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_id:
 218  242  337 NA's 
   2    9    1  587 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.120   2.340   2.590   3.001   2.882   5.330     587 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
  12  587 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.volume:
11.8 16.7 33.3 NA's 
   1    9    2  587 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2007 2008 NA's 
  11    1  587 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 8.118e+08 1.050e+09 8.117e+08 1.050e+09 1.050e+09       591 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.center_id:
   1    8 NA's 
   2    6  591 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.concentration:
0.05 0.15 0.16 NA's 
   6    1    1  591 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   3    7   19 NA's 
   6    1    1  591 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   8  591 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   1    4    5 NA's 
   6    1    1  591 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.500   3.375   4.500   9.000     591 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.plate_id:
 888 1003 1490 1491 NA's 
   1    1    1    5  591 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.quantity:
 0.3 0.53    2 NA's 
   1    1    6  591 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.source_center:
  22 NA's 
   8  591 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.volume:
   2 3.33   40 NA's 
   1    1    6  591 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2010 2011 NA's 
   2    6  591 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-06-0145-01a-02d-1490-08 tcga-06-0171-01a-02d-1491-08 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid:
a1fcbf3f-00e9-41d2-8f75-82452564c3db cec8cd80-c5e9-4005-9be9-8e2a46d8530b 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
1049950172 1049950491       NA's 
         1          1        597 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.center_id:
   8 NA's 
   2  597 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.concentration:
0.05 NA's 
   2  597 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   3 NA's 
   2  597 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   1 NA's 
   2  597 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.plate_column:
   3    5 NA's 
   1    1  597 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.plate_id:
1490 1491 NA's 
   1    1  597 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.plate_row:
   b    e NA's 
   1    1  597 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.quantity:
   2 NA's 
   2  597 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.source_center:
  22 NA's 
   2  597 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.volume:
  40 NA's 
   2  597 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2011 NA's 
   2  597 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_barcode:
tcga-06-0171-01a-02d-2366-09                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_uuid:
b023de78-d4ca-4660-b566-f9e24a4e9853                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
1076437004       NA's 
         1        598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.center_id:
   9 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.concentration:
0.05 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
  29 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   8 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.plate_column:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.plate_id:
2366 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.plate_row:
   c NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.quantity:
 1.5 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.volume:
  30 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2012 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_barcode:
tcga-06-0171-01a-02d-a33c-08                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_uuid:
18901f76-660b-4cc6-af1b-1b6f5d62204d                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom:
153889063      NA's 
        1       598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.center_id:
   8 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.concentration:
0.07 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.day_of_shipment:
  21 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.month_of_shipment:
  10 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.plate_column:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.plate_id:
a33c NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.plate_row:
   g NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.quantity:
0.94 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.source_center:
  23 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.volume:
13.4 NA's 
   1  598 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.9.year_of_shipment:
2013 NA's 
   1  598 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
108685294 108685297 108685318 108685339      NA's 
        1         1         1         1       595 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.center_id:
   1    6    7 NA's 
   9    9    2  579 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.1400  0.1450  0.1495  0.1625  0.2000     579 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.day_of_shipment:
   6   25   28   29 NA's 
   3    4    3   10  579 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  20  579 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     4.0     8.0     6.4     8.0    11.0     579 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     1.0     2.0     2.7     4.0     7.0     579 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  214.0   234.0   234.0   539.1   312.8  1698.0     579 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.600   2.800   3.571   5.000   5.990     579 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.source_center:
  22 NA's 
  20  579 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.90   18.32   20.00   23.77   33.30   33.30     579 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.year_of_shipment:
2007 2008 2011 NA's 
  13    3    4  579 

patient.samples.sample.portions.portion.2.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   9.625  53.750 153.363 225.000 600.000     579 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type:
      dna       rna total rna      NA's 
       12         7         1       579 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type_id:
   d    r    t NA's 
  12    7    1  579 



patient.samples.sample.portions.portion.2.analytes.analyte.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               8              591 

patient.samples.sample.portions.portion.2.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.1400  0.1450  0.1505  0.1625  0.2000     579 

patient.samples.sample.portions.portion.2.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
  12  587 

patient.samples.sample.portions.portion.2.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
  12  587 

patient.samples.sample.portions.portion.2.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              20 
                                            NA's 
                                             579 

patient.samples.sample.portions.portion.2.analytes.analyte.is_derived_from_ffpe:
  no NA's 
  20  579 

patient.samples.sample.portions.portion.2.analytes.analyte.protocols.protocol.experimental_protocol_type:
 adna preparation type allprep rna extraction              total rna 
                    12                      7                      1 
                  NA's 
                   579 

patient.samples.sample.portions.portion.2.analytes.analyte.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.250   1.492   1.565   1.708   1.667   2.700     593 

patient.samples.sample.portions.portion.2.analytes.analyte.rna.rinvalue:
   8  8.1  8.4  8.8 NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.2.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
     20     579 

patient.samples.sample.portions.portion.2.analytes.analyte.subportion_sequence:
   3 NA's 
   1  598 



patient.samples.sample.portions.portion.2.day_of_creation:
   4   11 NA's 
  17    3  579 

patient.samples.sample.portions.portion.2.is_ffpe:
  no NA's 
  20  579 

patient.samples.sample.portions.portion.2.month_of_creation:
   2    3    4 NA's 
  16    1    3  579 

patient.samples.sample.portions.portion.2.portion_number:
   2    3    5   13 NA's 
   7    8    1    4  579 




patient.samples.sample.portions.portion.2.slides.slide.2.is_derived_from_ffpe:
  no NA's 
  15  584 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_lymphocyte_infiltration:
   0 NA's 
   6  593 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_monocyte_infiltration:
   0 NA's 
   5  594 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   10.00   17.50   31.25   50.00     585 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_neutrophil_infiltration:
   0 NA's 
   5  594 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_normal_cells:
   0   25 NA's 
  13    1  585 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_stromal_cells:
   0   10   15   21 NA's 
   9    1    3    1  585 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  50.00   61.25   79.50   75.29   87.50  100.00     585 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_tumor_nuclei:
  80   85   95  100 NA's 
   1    1    5    7  585 

patient.samples.sample.portions.portion.2.slides.slide.2.section_location:
bottom    top   NA's 
    13      2    584 




patient.samples.sample.portions.portion.2.slides.slide.is_derived_from_ffpe:
  no NA's 
  20  579 

patient.samples.sample.portions.portion.2.slides.slide.percent_lymphocyte_infiltration:
   0 NA's 
   5  594 

patient.samples.sample.portions.portion.2.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   5  594 

patient.samples.sample.portions.portion.2.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   10.00   12.65   25.00   50.00     579 

patient.samples.sample.portions.portion.2.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   5  594 

patient.samples.sample.portions.portion.2.slides.slide.percent_normal_cells:
   0   25 NA's 
  18    1  580 

patient.samples.sample.portions.portion.2.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0     0.0     4.0     2.5    30.0     580 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  40.00   75.00   85.00   81.63   98.50  100.00     580 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    85.0    97.5    89.0   100.0   100.0     579 

patient.samples.sample.portions.portion.2.slides.slide.section_location:
bottom    top   NA's 
     2     18    579 

patient.samples.sample.portions.portion.2.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  45.50   46.25   89.50  109.44  150.75  216.00     591 

patient.samples.sample.portions.portion.2.year_of_creation:
2009 2011 NA's 
  16    4  579 

patient.samples.sample.portions.portion.3.analytes.analyte.2.a260_a280_ratio:
1.79 1.81 1.83 NA's 
   1    1    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0137-01a-03r-0213-07 tcga-06-0145-01a-03r-0213-07 
                           1                            1 
tcga-06-0148-01a-04r-0231-06 tcga-06-0156-01a-03r-0231-06 
                           1                            1 
                        NA's 
                         595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
03c04fe4-1b36-479b-a7f1-e17ea7c17889 33016e10-1d75-4378-befe-c40830bb8613 
                                   1                                    1 
5056c36e-c761-4cff-846a-e8cdcfb5e00e 9503f28f-ac40-4a32-a151-fc5b5dc42dc1 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.2.center_id:
   6    7 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.15 0.16 0.17 NA's 
   2    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  28   29 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   6    8 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   1    2    4 NA's 
   1    2    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.2.plate_id:
 213  231 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   a    b    d    g NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.2.quantity:
   2  3.2  3.4 NA's 
   2    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.2.volume:
13.3   20 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2007 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0137-01a-03r-0219-01 tcga-06-0145-01a-03r-0219-01 
                           1                            1 
tcga-06-0148-01a-04r-0232-03 tcga-06-0156-01a-03r-0232-03 
                           1                            1 
                        NA's 
                         595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid:
0b70c674-6976-4dbe-a36f-b36d7f08fdaf 3ea32cc2-2441-4c99-a254-1fc51bc7c04f 
                                   1                                    1 
90453579-a8cb-4883-82eb-9909d701fd87 cb54c6ec-023b-4818-ac7a-06aa1aa80a0e 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.3.center_id:
   1    3 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.15 0.16 0.17 NA's 
   2    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
  28   29 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   6    8 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   1    2    4 NA's 
   1    2    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.3.plate_id:
 219  232 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   a    b    d    g NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.3.quantity:
2.67 2.84    5 NA's 
   1    1    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.3.volume:
16.7 33.3 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2007 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-06-0137-01a-03r-0220-02 tcga-06-0145-01a-03r-0220-02 
                           1                            1 
tcga-06-0148-01a-04r-0233-01 tcga-06-0156-01a-03r-0233-01 
                           1                            1 
                        NA's 
                         595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_uuid:
359c83a5-c66a-471d-964c-e5cf46feec3a 7c8ada65-dfee-4874-b707-1aad7e70d83e 
                                   1                                    1 
88ef5904-dc13-43e7-9786-209098cecf4c ea475020-4b6a-473d-9d21-85c31c57b48d 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.4.center_id:
   1    2 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.4.concentration:
0.15 0.16 0.17 NA's 
   2    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
  28   29 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
   6    8 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   1    2    3 NA's 
   1    2    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.4.plate_id:
 220  233 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.4.plate_row:
   b    d NA's 
   1    3  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.4.quantity:
   3 5.33 5.66 NA's 
   2    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.4.source_center:
  22 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.4.volume:
  20 33.3 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-06-0137-01a-03r-0513-10 tcga-06-0156-01a-03r-1849-01 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.5.bcr_aliquot_uuid:
67b4bd07-77fd-4595-b2a0-5f3a3a43a221 e20ded68-f3c3-42d5-92e8-0bd277e671a2 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.5.biospecimen_barcode_bottom:
1053965803       NA's 
         1        598 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.5.center_id:
   1   10 NA's 
   1    1  597 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.5.concentration:
0.15 0.17 NA's 
   1    1  597 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.5.day_of_shipment:
   6   27 NA's 
   1    1  597 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.5.month_of_shipment:
   5    6 NA's 
   1    1  597 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.5.plate_column:
   3   12 NA's 
   1    1  597 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.5.plate_id:
 513 1849 NA's 
   1    1  597 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.5.plate_row:
   a    c NA's 
   1    1  597 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.5.quantity:
 5.66 10.71  NA's 
    1     1   597 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.5.source_center:
  22 NA's 
   2  597 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.5.volume:
33.3 71.4 NA's 
   1    1  597 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.5.year_of_shipment:
2009 2011 NA's 
   1    1  597 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0137-01a-03r-0212-03 tcga-06-0145-01a-03r-0212-03 
                           1                            1 
tcga-06-0148-01a-04r-0229-07 tcga-06-0156-01a-03r-0229-07 
                           1                            1 
                        NA's 
                         595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
0692dd73-ca75-442c-b98b-82042f0664e9 4ce41c79-7ce3-4697-8bc0-d459186570fe 
                                   1                                    1 
dc38c7ca-f2f5-46ab-ba93-919f7988be9f ef471c48-74bd-4d9d-a901-5e47833f3bf2 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.center_id:
   3    7 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.concentration:
0.15 0.16 0.17 NA's 
   2    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
  28   29 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   6    8 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.plate_column:
   1    2    4 NA's 
   1    2    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.plate_id:
 212  229 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.plate_row:
   a    b    d    g NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.quantity:
2.13 2.26    5 NA's 
   1    1    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.volume:
13.3 33.3 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2007 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.amount:
  10   32   70  150 NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.analyte_type:
 rna NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.analyte_type_id:
   r NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.bcr_analyte_barcode:
tcga-06-0137-01a-03r tcga-06-0145-01a-03r tcga-06-0148-01a-04r 
                   1                    1                    1 
tcga-06-0156-01a-03r                 NA's 
                   1                  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.bcr_analyte_uuid:
454d11ab-1c39-4d2e-8235-b986bef7938d 6440f439-f3c7-4ff5-b250-05a50744b388 
                                   1                                    1 
bde3e429-4afa-4855-8a6c-9862232ec313 d6bff964-700e-4176-a192-52eb717b2a52 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               3              596 

patient.samples.sample.portions.portion.3.analytes.analyte.2.concentration:
0.15 0.16 0.17 NA's 
   2    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                             595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
allprep rna extraction                   NA's 
                     4                    595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.rna.ratio_28s_18s:
   1  1.3  1.4 NA's 
   1    1    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.2.rna.rinvalue:
 8.4  9.3 NA's 
   2    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      4     595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0148-01a-04w-0250-10                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_uuid:
fa49f5c4-7ad6-4e84-919a-ab99c406f6fe                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.center_id:
  10 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
  12 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   9 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.plate_id:
 250 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.plate_row:
   f NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.quantity:
 200 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.volume:
 400 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0148-01a-04w-0252-09                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_uuid:
cef90cc5-0ca5-4f0c-8b07-736b8920cf1c                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.center_id:
   9 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.day_of_shipment:
  12 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.month_of_shipment:
   9 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.plate_column:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.plate_id:
 252 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.plate_row:
   f NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.quantity:
 200 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.volume:
 400 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-06-0148-01a-04w-0254-08                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.bcr_aliquot_uuid:
2dd38295-bcf5-4d63-be93-672f5423faaf                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.center_id:
   8 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.day_of_shipment:
  12 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.month_of_shipment:
   9 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.plate_column:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.plate_id:
 254 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.plate_row:
   f NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.quantity:
 200 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.volume:
 400 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0137-01a-03t-0213-07 tcga-06-0145-01a-03t-0213-07 
                           1                            1 
tcga-06-0148-01a-04w-0248-02 tcga-06-0156-01a-03t-0230-07 
                           1                            1 
                        NA's 
                         595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
30f413fb-a6e4-49fb-98f8-44e6e6a2587d 58293e1f-3e0e-44b8-ae3b-3e24b7b78ac1 
                                   1                                    1 
e38897f8-0f85-4275-80ac-1fe7d65a2034 e6bfc30b-fff2-4749-bc8b-94f9e28b641f 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.center_id:
   2    7 NA's 
   1    3  595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.concentration:
 0.1  0.5 NA's 
   3    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
  12   28   29 NA's 
   1    2    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   6    8    9 NA's 
   2    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.plate_column:
   1    2    7    9 NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.plate_id:
 213  230  248 NA's 
   2    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.plate_row:
   a    c    f    g NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.quantity:
   1    2   14 NA's 
   1    2    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.volume:
  10   20   28 NA's 
   1    2    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2007 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.amount:
   0   10   20   30 NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna            total rna                 NA's 
                   1                    3                  595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.analyte_type_id:
   t    w NA's 
   3    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.bcr_analyte_barcode:
tcga-06-0137-01a-03t tcga-06-0145-01a-03t tcga-06-0148-01a-04w 
                   1                    1                    1 
tcga-06-0156-01a-03t                 NA's 
                   1                  595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.bcr_analyte_uuid:
61bd6f5b-5bd7-4471-90d2-0cb85697f4f0 8d5024b2-e70a-4b9f-9273-74f5f60271cf 
                                   1                                    1 
a410fd08-a2ae-4455-a904-c2a8ab20dff5 ddc34ca8-1a3f-459c-ad90-04ffd3b6eb00 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               3              596 

patient.samples.sample.portions.portion.3.analytes.analyte.3.concentration:
 0.1 0.17 0.27  0.5 NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.3.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                             595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g total rna      NA's 
        1         3       595 

patient.samples.sample.portions.portion.3.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
      4     595 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0137-01a-03w-0224-08 tcga-06-0145-01a-03w-0224-08 
                           1                            1 
tcga-06-0156-01a-03w-0250-10                         NA's 
                           1                          596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.2.bcr_aliquot_uuid:
0359e35f-5b20-48a6-b0c0-3689ec0024dc 2efb1373-a4c5-44a0-8034-fc8149dc4531 
                                   1                                    1 
58fd24c3-e17a-455a-80f6-56c371357ca0                                 NA's 
                                   1                                  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.2.center_id:
   8   10 NA's 
   2    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
  12   18 NA's 
   1    2  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
   7    9 NA's 
   2    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   2    3    4 NA's 
   1    1    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.2.plate_id:
 224  250 NA's 
   2    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.2.plate_row:
   a    d    g NA's 
   1    1    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.2.quantity:
 200 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.2.source_center:
  22 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.2.volume:
 400 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2007 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0137-01a-03w-0225-09 tcga-06-0145-01a-03w-0225-09 
                           1                            1 
tcga-06-0156-01a-03w-0252-09                         NA's 
                           1                          596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_uuid:
25a2714e-8dbf-4c50-98bf-f81e44365ba3 39e575c0-f71e-4145-9ca7-d026f027c078 
                                   1                                    1 
f24752b7-cc8f-4d41-8bc9-49e06357ef4d                                 NA's 
                                   1                                  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.3.center_id:
   9 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.3.concentration:
 0.5 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.3.day_of_shipment:
  12   18 NA's 
   1    2  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.3.month_of_shipment:
   7    9 NA's 
   2    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.3.plate_column:
   2    3    4 NA's 
   1    1    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.3.plate_id:
 225  252 NA's 
   2    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.3.plate_row:
   a    d    g NA's 
   1    1    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.3.quantity:
 200 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.3.source_center:
  22 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.3.volume:
 400 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.3.year_of_shipment:
2007 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-06-0137-01a-03w-0226-02 tcga-06-0145-01a-03w-0226-02 
                           1                            1 
tcga-06-0156-01a-03w-0254-08                         NA's 
                           1                          596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.4.bcr_aliquot_uuid:
24d23937-a4a9-4e1f-a53b-784940033105 baa9b05e-4352-4e83-baa4-7b636d3c9cf0 
                                   1                                    1 
e9796208-7056-4e6a-b93a-5698ce58ce7d                                 NA's 
                                   1                                  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.4.center_id:
   2    8 NA's 
   2    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.4.concentration:
 0.5 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.4.day_of_shipment:
  12   18 NA's 
   1    2  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.4.month_of_shipment:
   7    9 NA's 
   2    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.4.plate_column:
   2    3    4 NA's 
   1    1    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.4.plate_id:
 226  254 NA's 
   2    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.4.plate_row:
   a    d    g NA's 
   1    1    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.4.quantity:
  14  200 NA's 
   2    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.4.source_center:
  22 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.4.volume:
  28  400 NA's 
   2    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0137-01a-03w-0223-10 tcga-06-0145-01a-03w-0223-10 
                           1                            1 
tcga-06-0156-01a-03w-0248-02                         NA's 
                           1                          596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_uuid:
64d2e7f4-ea97-4a57-8762-b015d6db8a13 84f05364-2887-401d-850b-5f12927d0b2a 
                                   1                                    1 
de8a4f1b-b0a3-4446-8825-1e51f9ac1db3                                 NA's 
                                   1                                  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.center_id:
   2   10 NA's 
   1    2  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
  12   18 NA's 
   1    2  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   7    9 NA's 
   2    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.plate_column:
   2    3    4 NA's 
   1    1    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.plate_id:
 223  248 NA's 
   2    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.plate_row:
   a    d    g NA's 
   1    1    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.quantity:
  14  200 NA's 
   1    2  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.volume:
  28  400 NA's 
   1    2  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2007 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.amount:
   0 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                   3                  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.analyte_type_id:
   w NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.bcr_analyte_barcode:
tcga-06-0137-01a-03w tcga-06-0145-01a-03w tcga-06-0156-01a-03w 
                   1                    1                    1 
                NA's 
                 596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.bcr_analyte_uuid:
342c7f13-7fe3-420d-8ef6-41e16b9542b1 cc17c3fe-c2d4-47b1-be3f-ca389c4b023d 
                                   1                                    1 
fc99c9c5-f80c-4e73-a559-5898ed388711                                 NA's 
                                   1                                  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.concentration:
 0.5 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               3 
                                            NA's 
                                             596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      3     596 

patient.samples.sample.portions.portion.3.analytes.analyte.4.spectrophotometer_method:
uv spec    NA's 
      3     596 

patient.samples.sample.portions.portion.3.analytes.analyte.a260_a280_ratio:
1.89 1.91 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0137-01a-03d-0215-04 tcga-06-0145-01a-03d-0215-04 
                           1                            1 
tcga-06-0148-01a-04d-0236-01 tcga-06-0156-01a-03d-0236-01 
                           1                            1 
                        NA's 
                         595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
11416c19-9820-4bf4-99db-405da7af5134 2c32f015-859a-4aa8-a468-23f076cd4579 
                                   1                                    1 
3a132d83-ccdf-408e-82d0-ce040ed70bff 3f4f290d-3137-430b-b181-ff7ffb1d03c2 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.center_id:
   1    4 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.concentration:
0.12 0.15 0.18 NA's 
   1    2    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  28   29 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   6    8 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.plate_column:
   1    2    4 NA's 
   1    2    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.plate_id:
 215  236 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.plate_row:
   a    g NA's 
   1    3  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.quantity:
 1.2  1.8    5 NA's 
   1    1    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.volume:
  10 33.3 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2007 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0137-01a-03d-0216-06 tcga-06-0145-01a-03d-0216-06 
                           1                            1 
tcga-06-0148-01a-04d-0238-04 tcga-06-0156-01a-03d-0238-04 
                           1                            1 
                        NA's 
                         595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
0f36cad2-9ab2-4fa7-bad6-fc96e343b7d1 3108f9df-c7c7-446c-b0fd-4391edc9b538 
                                   1                                    1 
91d90976-69c0-414e-be4a-16724a8933ab aa84a7bb-e75a-4d04-b300-693083a899d8 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.center_id:
   4    6 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.concentration:
0.12 0.15 0.18 NA's 
   1    2    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  28   29 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   6    8 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.plate_column:
   1    2    4 NA's 
   1    2    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.plate_id:
 216  238 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.plate_row:
   a    g NA's 
   1    3  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.quantity:
   3  4.8  7.2 NA's 
   2    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.volume:
  20   40 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2007 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-06-0137-01a-03d-0217-02 tcga-06-0145-01a-03d-0217-02 
                           1                            1 
tcga-06-0148-01a-04d-0240-06 tcga-06-0156-01a-03d-0240-06 
                           1                            1 
                        NA's 
                         595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
0a3e3180-e5ce-4226-81a2-b6e01fe34d08 b1bd01ac-aaf6-4d70-bf8a-b74f9832ca4c 
                                   1                                    1 
d430681b-a84c-43cb-b53f-8cd377ea55f0 da5c8596-44f2-4e11-93aa-71489c15b1b3 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.center_id:
   2    6 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.concentration:
0.12 0.15 0.16 0.18 NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  28   29 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   6    8 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.plate_column:
   1    2    3    5 NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.plate_id:
 217  240 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.plate_row:
   e    f    g NA's 
   1    1    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.quantity:
 2.4  3.6    5 5.33 NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.volume:
  20 33.3 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-06-0137-01a-03d-0218-05 tcga-06-0145-01a-03d-0218-05 
                           1                            1 
tcga-06-0148-01a-04d-0242-05 tcga-06-0156-01a-03d-0242-05 
                           1                            1 
                        NA's 
                         595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
016d1b50-ba06-4733-bac1-2ec7984605c0 1cd13e9e-23b7-4e49-9b5f-cc7cecabf2ea 
                                   1                                    1 
72572982-e355-4bcd-a045-72913b1b4ae4 b0e3b163-0940-4909-928e-c5a15f389122 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.center_id:
   5 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.concentration:
0.12 0.15 0.16 0.18 NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  28   29 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   6    8 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.plate_column:
   1    2    4 NA's 
   1    2    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.plate_id:
 218  242 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.plate_row:
   a    g NA's 
   1    3  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.quantity:
   2 3.01    5 5.33 NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.volume:
16.7 33.3 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2007 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-06-0137-01a-03d-0513-10 tcga-06-0145-01a-03d-1003-01 
                           1                            1 
tcga-06-0156-01a-03d-1491-08                         NA's 
                           1                          596 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
2889ce5a-8615-490b-9f8b-e4c095e55660 3b94893f-1c26-4df7-8bf7-11e176172532 
                                   1                                    1 
6c4b534e-68fa-4171-8b02-7e433cbcf24f                                 NA's 
                                   1                                  596 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
  97544078 1049950175       NA's 
         1          1        597 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.center_id:
   1    8   10 NA's 
   1    1    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.concentration:
0.05 0.15 0.16 NA's 
   1    1    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   3    6   19 NA's 
   1    1    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   1    5 NA's 
   1    2  596 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.plate_column:
   1    2 NA's 
   2    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.plate_id:
 513 1003 1491 NA's 
   1    1    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.plate_row:
   a    c NA's 
   1    2  596 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.quantity:
0.53    2   20 NA's 
   1    1    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.source_center:
  22 NA's 
   3  596 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.volume:
 3.33    40 133.3  NA's 
    1     1     1   596 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2009 2010 2011 NA's 
   1    1    1  596 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-06-0137-01a-03d-1490-08 tcga-06-0145-01a-03d-1490-08 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid:
4bc350b4-4558-4869-a5a8-1b4a88305e2e a50a6f11-1679-4649-b2c1-7b5197306773 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
1049950490 1049950506       NA's 
         1          1        597 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.center_id:
   8 NA's 
   2  597 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.concentration:
0.05 NA's 
   2  597 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   3 NA's 
   2  597 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   1 NA's 
   2  597 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.plate_column:
   4    5 NA's 
   1    1  597 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.plate_id:
1490 NA's 
   2  597 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.plate_row:
   e    f NA's 
   1    1  597 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.quantity:
   2 NA's 
   2  597 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.source_center:
  22 NA's 
   2  597 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.volume:
  40 NA's 
   2  597 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2011 NA's 
   2  597 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0137-01a-03d-0214-01 tcga-06-0145-01a-03d-0214-01 
                           1                            1 
tcga-06-0148-01a-04d-0234-06 tcga-06-0156-01a-03d-0234-06 
                           1                            1 
                        NA's 
                         595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
2637dded-37c0-47dd-9209-2945b7b4a6c8 6cfbab5c-6de7-4dea-824f-f3d88522c95c 
                                   1                                    1 
a9e9862a-ff51-4cf2-aad1-e3420dcee203 c867f9ab-6ba0-43b6-a32a-f8fe0aa199f9 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.center_id:
   1    6 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.concentration:
0.12 0.15 0.18 NA's 
   1    2    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.day_of_shipment:
  28   29 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.month_of_shipment:
   6    8 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.plate_column:
   1    2    4 NA's 
   1    2    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.plate_id:
 214  234 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.plate_row:
   a    g NA's 
   1    3  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.quantity:
   2    4 5.99 NA's 
   2    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.source_center:
  22 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.volume:
13.3 33.3 NA's 
   2    2  595 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.year_of_shipment:
2007 NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.amount:
83.37 226.4   500   700  NA's 
    1     1     1     1   595 

patient.samples.sample.portions.portion.3.analytes.analyte.analyte_type:
 dna NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.analyte_type_id:
   d NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.bcr_analyte_barcode:
tcga-06-0137-01a-03d tcga-06-0145-01a-03d tcga-06-0148-01a-04d 
                   1                    1                    1 
tcga-06-0156-01a-03d                 NA's 
                   1                  595 

patient.samples.sample.portions.portion.3.analytes.analyte.bcr_analyte_uuid:
7c068503-625a-4d12-a464-fcb1fa65b7d1 905a15fc-69ef-4216-87f2-ec1b17755d9e 
                                   1                                    1 
b14b041c-214d-4312-82a1-3dbacbf3099b bfa7fba3-f31c-437d-b880-e3143cdfd308 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.portions.portion.3.analytes.analyte.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               3              596 

patient.samples.sample.portions.portion.3.analytes.analyte.concentration:
0.12 0.15 0.16 0.18 NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.3.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                             595 

patient.samples.sample.portions.portion.3.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.portions.portion.3.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    4                   595 

patient.samples.sample.portions.portion.3.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      4     595 

patient.samples.sample.portions.portion.3.bcr_portion_barcode:
tcga-06-0137-01a-03 tcga-06-0145-01a-03 tcga-06-0148-01a-04 tcga-06-0156-01a-03 
                  1                   1                   1                   1 
               NA's 
                595 

patient.samples.sample.portions.portion.3.bcr_portion_uuid:
7507eb83-723b-4d9a-94db-4f4eb369faa2 7acb607f-ced9-43cf-9b1a-e1893e5fc5e7 
                                   1                                    1 
a839fc4c-0e71-4296-8696-be73ca3321b2 f570d97f-3e78-4b0d-a5af-b8f9896704f1 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.portions.portion.3.day_of_creation:
   4 NA's 
   4  595 

patient.samples.sample.portions.portion.3.is_ffpe:
  no NA's 
   4  595 

patient.samples.sample.portions.portion.3.month_of_creation:
   2 NA's 
   4  595 

patient.samples.sample.portions.portion.3.portion_number:
   3    4 NA's 
   3    1  595 

patient.samples.sample.portions.portion.3.slides.slide.2.bcr_slide_barcode:
tcga-06-0137-01a-03-bs3 tcga-06-0145-01a-03-bs3 tcga-06-0148-01a-04-bs4 
                      1                       1                       1 
tcga-06-0156-01a-03-ts3                    NA's 
                      1                     595 

patient.samples.sample.portions.portion.3.slides.slide.2.bcr_slide_uuid:
003f242a-aac0-4d4b-af05-4ba6facd000b 5da5e32d-aafb-48b8-ae1d-1492617a5daa 
                                   1                                    1 
bf9d7aa2-5206-4971-b3db-364d4df6dd83 c2c8f4dd-06d2-46cb-bc41-58b001adcb01 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.portions.portion.3.slides.slide.2.image_file_name:
tcga-06-0137-01a-03-bs3.003f242a-aac0-4d4b-af05-4ba6facd000b.svs 
                                                               1 
tcga-06-0145-01a-03-bs3.5da5e32d-aafb-48b8-ae1d-1492617a5daa.svs 
                                                               1 
tcga-06-0148-01a-04-bs4.c2c8f4dd-06d2-46cb-bc41-58b001adcb01.svs 
                                                               1 
tcga-06-0156-01a-03-ts3.bf9d7aa2-5206-4971-b3db-364d4df6dd83.svs 
                                                               1 
                                                            NA's 
                                                             595 

patient.samples.sample.portions.portion.3.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_lymphocyte_infiltration:
   0 NA's 
   3  596 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_monocyte_infiltration:
   0 NA's 
   3  596 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_necrosis:
   0    5   20   40 NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_neutrophil_infiltration:
   0 NA's 
   3  596 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_normal_cells:
   0 NA's 
   4  595 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_stromal_cells:
   0   50 NA's 
   3    1  595 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_tumor_cells:
  50   60   80   95 NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.3.slides.slide.2.percent_tumor_nuclei:
  90  100 NA's 
   1    3  595 

patient.samples.sample.portions.portion.3.slides.slide.2.section_location:
bottom    top   NA's 
     3      1    595 

patient.samples.sample.portions.portion.3.slides.slide.bcr_slide_barcode:
tcga-06-0137-01a-02-bs2 tcga-06-0145-01a-02-bs2 tcga-06-0148-01a-03-bs3 
                      1                       1                       1 
tcga-06-0156-01a-03-bs3                    NA's 
                      1                     595 

patient.samples.sample.portions.portion.3.slides.slide.bcr_slide_uuid:
1ecbdd52-f82b-4621-9062-d4e61bcd0469 35ddb8d4-b43e-49f4-a89c-b958748f7e53 
                                   1                                    1 
6bac7c82-b883-42a5-82e1-2da0a9c1ebf7 b3e605db-2a13-4244-8c2a-35a653adb295 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.portions.portion.3.slides.slide.image_file_name:
tcga-06-0137-01a-02-bs2.1ecbdd52-f82b-4621-9062-d4e61bcd0469.svs 
                                                               1 
tcga-06-0145-01a-02-bs2.6bac7c82-b883-42a5-82e1-2da0a9c1ebf7.svs 
                                                               1 
tcga-06-0148-01a-03-bs3.35ddb8d4-b43e-49f4-a89c-b958748f7e53.svs 
                                                               1 
                                                            NA's 
                                                             596 

patient.samples.sample.portions.portion.3.slides.slide.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.portions.portion.3.slides.slide.percent_lymphocyte_infiltration:
   0 NA's 
   3  596 

patient.samples.sample.portions.portion.3.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   3  596 

patient.samples.sample.portions.portion.3.slides.slide.percent_necrosis:
   5   10   20   95 NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.3.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   3  596 

patient.samples.sample.portions.portion.3.slides.slide.percent_normal_cells:
   0 NA's 
   4  595 

patient.samples.sample.portions.portion.3.slides.slide.percent_stromal_cells:
   0   15 NA's 
   3    1  595 

patient.samples.sample.portions.portion.3.slides.slide.percent_tumor_cells:
   5   80   90 NA's 
   1    2    1  595 

patient.samples.sample.portions.portion.3.slides.slide.percent_tumor_nuclei:
   5   95  100 NA's 
   1    1    2  595 

patient.samples.sample.portions.portion.3.slides.slide.section_location:
bottom    top   NA's 
     1      3    595 

patient.samples.sample.portions.portion.3.year_of_creation:
2009 NA's 
   4  595 

patient.samples.sample.portions.portion.4.analytes.analyte.2.a260_a280_ratio:
 1.8 1.82 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0145-01a-04r-0213-07 tcga-06-0148-01a-05r-0231-06 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
10a3977a-2c59-4fde-ab7a-dd94aed79fef 9b22f346-0778-4848-a753-93d7dfcb8123 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.2.center_id:
   6    7 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.14 0.15 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  28   29 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   6    8 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   1    4 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.2.plate_id:
 213  231 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   b    e NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.2.quantity:
   2  2.8 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.2.volume:
13.3   20 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2007 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0145-01a-04r-0219-01 tcga-06-0148-01a-05r-0232-03 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid:
83012603-b02a-48a6-85e7-bb2deb4845ef 86c7bed7-a6e7-409e-b896-026db6f36b8b 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.3.center_id:
   1    3 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.14 0.15 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
  28   29 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   6    8 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   1    4 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.3.plate_id:
 219  232 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   b    e NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.3.quantity:
2.34    5 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.3.volume:
16.7 33.3 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2007 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-06-0145-01a-04r-0220-02 tcga-06-0148-01a-05r-0233-01 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_uuid:
6bedd42d-24fc-4f43-93a8-3fb32c771ea1 a376b70c-1368-4ed4-b887-5d243a17ee13 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.4.center_id:
   1    2 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.4.concentration:
0.14 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
  28   29 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
   6    8 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   1    3 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.4.plate_id:
 220  233 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.4.plate_row:
   e NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.4.quantity:
 2.8 4.66 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.4.source_center:
  22 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.4.volume:
  20 33.3 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0145-01a-04r-0212-03 tcga-06-0148-01a-05r-0229-07 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
025a06b8-85da-4820-a40f-7e120e74834c 822b6a12-f9d7-4bf4-929e-57a403da82ce 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.center_id:
   3    7 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.concentration:
0.14 0.15 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
  28   29 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   6    8 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.plate_column:
   1    4 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.plate_id:
 212  229 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.plate_row:
   b    e NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.quantity:
1.86    5 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.volume:
13.3 33.3 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2007 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.amount:
 100 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.2.analyte_type:
 rna NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.analyte_type_id:
   r NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.bcr_analyte_barcode:
tcga-06-0145-01a-04r tcga-06-0148-01a-05r                 NA's 
                   1                    1                  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.bcr_analyte_uuid:
bb9849e3-bc99-48d6-8b52-1ca9df5fec95 fb42860d-6540-408a-9011-545096a1c58d 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               2              597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.concentration:
0.14 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
allprep rna extraction                   NA's 
                     2                    597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.rna.ratio_28s_18s:
   1  1.4 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.rna.rinvalue:
   7  8.7 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      2     597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0148-01a-05w-0250-10                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_uuid:
c396eafd-bdd0-4fac-94a7-08f7672e9c82                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.2.center_id:
  10 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
  12 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   9 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.2.plate_id:
 250 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.2.plate_row:
   g NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.2.quantity:
 200 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.2.volume:
 400 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0148-01a-05w-0252-09                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_uuid:
339849fb-3cf4-4636-94d3-a1d75d4967de                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.3.center_id:
   9 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.3.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.3.day_of_shipment:
  12 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.3.month_of_shipment:
   9 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.3.plate_column:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.3.plate_id:
 252 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.3.plate_row:
   g NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.3.quantity:
 200 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.3.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.3.volume:
 400 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.3.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-06-0148-01a-05w-0254-08                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.4.bcr_aliquot_uuid:
420248cb-7273-4c71-9bda-2f9decedb587                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.4.center_id:
   8 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.4.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.4.day_of_shipment:
  12 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.4.month_of_shipment:
   9 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.4.plate_column:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.4.plate_id:
 254 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.4.plate_row:
   g NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.4.quantity:
 200 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.4.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.4.volume:
 400 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0145-01a-04t-0213-07 tcga-06-0148-01a-05w-0248-02 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
2add0ce1-23db-4b4a-b4a6-e77e2db362a4 3b41d604-6249-46f7-9b08-ba681bb36606 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.center_id:
   2    7 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.concentration:
 0.1  0.5 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
  12   28 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   6    9 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.plate_column:
   1    9 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.plate_id:
 213  248 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.plate_row:
   b    g NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.quantity:
   2   14 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.volume:
  20   28 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2007 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.amount:
   0   80 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna            total rna                 NA's 
                   1                    1                  597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.analyte_type_id:
   t    w NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.bcr_analyte_barcode:
tcga-06-0145-01a-04t tcga-06-0148-01a-05w                 NA's 
                   1                    1                  597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.bcr_analyte_uuid:
0e814c9d-4e4b-43b0-b037-85111d1b247f 310c3b55-e5e7-4839-9a42-3d020e51d57a 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               2              597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.concentration:
0.48  0.5 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g total rna      NA's 
        1         1       597 

patient.samples.sample.portions.portion.4.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
      2     597 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0145-01a-04w-0224-08                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.2.bcr_aliquot_uuid:
c713a79c-1a8f-455f-96be-b037ede3b7f8                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.2.center_id:
   8 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
  18 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
   7 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.2.plate_id:
 224 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.2.plate_row:
   e NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.2.quantity:
 200 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.2.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.2.volume:
 400 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0145-01a-04w-0225-09                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_uuid:
8a9480de-e40c-429c-a982-053c01764216                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.3.center_id:
   9 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.3.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.3.day_of_shipment:
  18 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.3.month_of_shipment:
   7 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.3.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.3.plate_id:
 225 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.3.plate_row:
   e NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.3.quantity:
 200 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.3.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.3.volume:
 400 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.3.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-06-0145-01a-04w-0226-02                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.4.bcr_aliquot_uuid:
c5430bc6-b823-4cc8-8888-c1e34d6ae498                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.4.center_id:
   2 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.4.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.4.day_of_shipment:
  18 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.4.month_of_shipment:
   7 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.4.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.4.plate_id:
 226 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.4.plate_row:
   e NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.4.quantity:
  14 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.4.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.4.volume:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0145-01a-04w-0223-10                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_uuid:
a7f9277c-2cfe-4fe6-ba88-5436a0c8c48d                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.center_id:
  10 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
  18 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   7 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.plate_id:
 223 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.plate_row:
   e NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.quantity:
 200 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.volume:
 400 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.amount:
   0 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                   1                  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.analyte_type_id:
   w NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.bcr_analyte_barcode:
tcga-06-0145-01a-04w                 NA's 
                   1                  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.bcr_analyte_uuid:
93744eb3-5242-4d78-a261-3e11f0e8b2b2                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      1     598 

patient.samples.sample.portions.portion.4.analytes.analyte.4.spectrophotometer_method:
uv spec    NA's 
      1     598 

patient.samples.sample.portions.portion.4.analytes.analyte.a260_a280_ratio:
 1.9 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0145-01a-04d-0215-04 tcga-06-0148-01a-05d-0236-01 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
768c29e0-2050-44c0-bfb1-77da0b5555f7 a6828da7-70a9-43fa-aa16-173151114d0c 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.2.center_id:
   1    4 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.2.concentration:
0.14 0.15 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  28   29 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   6    8 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.2.plate_column:
   1    4 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.2.plate_id:
 215  236 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.2.plate_row:
   b    h NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.2.quantity:
 1.4    5 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.2.volume:
  10 33.3 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2007 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0145-01a-04d-0216-06 tcga-06-0148-01a-05d-0238-04 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
229c77ca-7fab-46f0-a329-595891f55397 ff2a1571-dbef-4ead-be2c-62f659f7ec7d 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.3.center_id:
   4    6 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.3.concentration:
0.14 0.15 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  28   29 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   6    8 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.3.plate_column:
   1    4 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.3.plate_id:
 216  238 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.3.plate_row:
   b    h NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.3.quantity:
   3  5.6 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.3.volume:
  20   40 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2007 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-06-0145-01a-04d-0217-02 tcga-06-0148-01a-05d-0240-06 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
ad01fe5f-bc15-4247-be40-9def76941afa fa248a09-fbad-4c6b-ba93-c354f5af2c40 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.4.center_id:
   2    6 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.4.concentration:
0.14 0.16 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  28   29 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   6    8 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.4.plate_column:
   1    5 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.4.plate_id:
 217  240 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.4.plate_row:
   f    h NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.4.quantity:
 2.8 5.33 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.4.volume:
  20 33.3 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-06-0145-01a-04d-0218-05 tcga-06-0148-01a-05d-0242-05 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
355912a1-0c34-4fab-9b1a-d22a54a642cb 9dc5679e-9d8d-4f05-9e90-04faeecb2415 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.5.center_id:
   5 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.5.concentration:
0.14 0.16 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  28   29 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   6    8 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.5.plate_column:
   1    4 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.5.plate_id:
 218  242 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.5.plate_row:
   b    h NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.5.quantity:
2.34 5.33 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.5.volume:
16.7 33.3 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2007 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-06-0145-01a-04d-1003-01                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
22949b13-2d16-4d62-8a94-dedfb94ce30c                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
97544055     NA's 
       1      598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.6.center_id:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.6.concentration:
0.16 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  19 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.6.plate_column:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.6.plate_id:
1003 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.6.plate_row:
   d NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.6.quantity:
0.53 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.6.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.6.volume:
3.33 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2010 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-06-0145-01a-04d-1490-08                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid:
e422ce22-5fd0-4927-b80e-ef96ccb87b0e                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
1049950515       NA's 
         1        598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.7.center_id:
   8 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.7.concentration:
0.05 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   3 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.7.plate_column:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.7.plate_id:
1490 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.7.plate_row:
   g NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.7.quantity:
   2 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.7.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.7.volume:
  40 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2011 NA's 
   1  598 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0145-01a-04d-0214-01 tcga-06-0148-01a-05d-0234-06 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
249be6ba-2c27-4506-bff4-040b3f417113 58a31418-c9a7-4ea0-841b-e501a41cfc5b 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.center_id:
   1    6 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.day_of_shipment:
  28   29 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.month_of_shipment:
   6    8 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.plate_column:
   1    4 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.plate_id:
 214  234 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.plate_row:
   b    h NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.quantity:
   2 4.66 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.source_center:
  22 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.volume:
13.3 33.3 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.aliquots.aliquot.year_of_shipment:
2007 NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.amount:
83.37   700  NA's 
    1     1   597 

patient.samples.sample.portions.portion.4.analytes.analyte.analyte_type:
 dna NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.analyte_type_id:
   d NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.bcr_analyte_barcode:
tcga-06-0145-01a-04d tcga-06-0148-01a-05d                 NA's 
                   1                    1                  597 

patient.samples.sample.portions.portion.4.analytes.analyte.bcr_analyte_uuid:
2c81dac0-c673-49c5-ac17-37cefc869321 8c128789-ff26-4d64-8c4a-05c5762b1d12 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.4.analytes.analyte.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               2              597 

patient.samples.sample.portions.portion.4.analytes.analyte.concentration:
0.14 0.16 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             597 

patient.samples.sample.portions.portion.4.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.4.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    2                   597 

patient.samples.sample.portions.portion.4.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      2     597 

patient.samples.sample.portions.portion.4.bcr_portion_barcode:
tcga-06-0145-01a-04 tcga-06-0148-01a-05                NA's 
                  1                   1                 597 

patient.samples.sample.portions.portion.4.bcr_portion_uuid:
41587777-1901-4fb6-8f6d-d6190c3479f5 5e3d406c-1526-4dac-b4a2-95d88adc3888 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.4.day_of_creation:
   4 NA's 
   2  597 

patient.samples.sample.portions.portion.4.is_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.4.month_of_creation:
   2 NA's 
   2  597 

patient.samples.sample.portions.portion.4.portion_number:
   4    5 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.slides.slide.2.bcr_slide_barcode:
tcga-06-0145-01a-04-bs4 tcga-06-0148-01a-05-bs5                    NA's 
                      1                       1                     597 

patient.samples.sample.portions.portion.4.slides.slide.2.bcr_slide_uuid:
074dae2c-3292-4459-8a0c-c05185407389 35af777a-905c-495a-a4d5-f7bc302a5cb7 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.4.slides.slide.2.image_file_name:
tcga-06-0145-01a-04-bs4.35af777a-905c-495a-a4d5-f7bc302a5cb7.svs 
                                                               1 
tcga-06-0148-01a-05-bs5.074dae2c-3292-4459-8a0c-c05185407389.svs 
                                                               1 
                                                            NA's 
                                                             597 

patient.samples.sample.portions.portion.4.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.4.slides.slide.2.percent_lymphocyte_infiltration:
   0 NA's 
   2  597 

patient.samples.sample.portions.portion.4.slides.slide.2.percent_monocyte_infiltration:
   0 NA's 
   2  597 

patient.samples.sample.portions.portion.4.slides.slide.2.percent_necrosis:
   5   25 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.slides.slide.2.percent_neutrophil_infiltration:
   0 NA's 
   2  597 

patient.samples.sample.portions.portion.4.slides.slide.2.percent_normal_cells:
   0 NA's 
   2  597 

patient.samples.sample.portions.portion.4.slides.slide.2.percent_stromal_cells:
   0 NA's 
   2  597 

patient.samples.sample.portions.portion.4.slides.slide.2.percent_tumor_cells:
  75   95 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.slides.slide.2.percent_tumor_nuclei:
 100 NA's 
   2  597 

patient.samples.sample.portions.portion.4.slides.slide.2.section_location:
bottom   NA's 
     2    597 

patient.samples.sample.portions.portion.4.slides.slide.bcr_slide_barcode:
tcga-06-0145-01a-03-bs3 tcga-06-0148-01a-04-bs4                    NA's 
                      1                       1                     597 

patient.samples.sample.portions.portion.4.slides.slide.bcr_slide_uuid:
5da5e32d-aafb-48b8-ae1d-1492617a5daa c2c8f4dd-06d2-46cb-bc41-58b001adcb01 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.4.slides.slide.image_file_name:
tcga-06-0145-01a-03-bs3.5da5e32d-aafb-48b8-ae1d-1492617a5daa.svs 
                                                               1 
tcga-06-0148-01a-04-bs4.c2c8f4dd-06d2-46cb-bc41-58b001adcb01.svs 
                                                               1 
                                                            NA's 
                                                             597 

patient.samples.sample.portions.portion.4.slides.slide.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.4.slides.slide.percent_lymphocyte_infiltration:
   0 NA's 
   2  597 

patient.samples.sample.portions.portion.4.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   2  597 

patient.samples.sample.portions.portion.4.slides.slide.percent_necrosis:
   5   20 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   2  597 

patient.samples.sample.portions.portion.4.slides.slide.percent_normal_cells:
   0 NA's 
   2  597 

patient.samples.sample.portions.portion.4.slides.slide.percent_stromal_cells:
   0 NA's 
   2  597 

patient.samples.sample.portions.portion.4.slides.slide.percent_tumor_cells:
  80   95 NA's 
   1    1  597 

patient.samples.sample.portions.portion.4.slides.slide.percent_tumor_nuclei:
 100 NA's 
   2  597 

patient.samples.sample.portions.portion.4.slides.slide.section_location:
 top NA's 
   2  597 

patient.samples.sample.portions.portion.4.year_of_creation:
2009 NA's 
   2  597 

patient.samples.sample.portions.portion.5.analytes.analyte.2.a260_a280_ratio:
1.84 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0145-01a-05r-0213-07                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
b6988e2b-5610-4f5f-a026-e7a605747c9f                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.2.plate_id:
 213 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   c NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.2.quantity:
   2 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.2.volume:
13.3 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0145-01a-05r-0219-01                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid:
dcde4313-f777-4726-8119-b118d6293040                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.3.center_id:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.3.plate_id:
 219 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   c NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.3.quantity:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.3.volume:
33.3 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-06-0145-01a-05r-0220-02                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_uuid:
c5487e1c-4a78-4376-85fc-342183e8f436                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.4.center_id:
   2 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.4.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   3 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.4.plate_id:
 220 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.4.plate_row:
   f NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.4.quantity:
   3 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.4.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.4.volume:
  20 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0145-01a-05r-0212-03                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
0ab41602-a7ce-4529-95ea-d98f034e4d42                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.center_id:
   3 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.plate_id:
 212 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.plate_row:
   c NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.quantity:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.volume:
33.3 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.amount:
 100 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.analyte_type:
 rna NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.analyte_type_id:
   r NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.bcr_analyte_barcode:
tcga-06-0145-01a-05r                 NA's 
                   1                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.bcr_analyte_uuid:
90236dea-6e6c-4d2a-9b09-d200f0bec94a                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               1              598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
allprep rna extraction                   NA's 
                     1                    598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.rna.ratio_28s_18s:
 1.3 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.rna.rinvalue:
 8.3 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      1     598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0145-01a-05t-0213-07                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
6b122870-223e-4978-b19b-6b791d25d61b                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.aliquots.aliquot.center_id:
   7 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.aliquots.aliquot.concentration:
 0.1 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.aliquots.aliquot.plate_column:
   9 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.aliquots.aliquot.plate_id:
 213 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.aliquots.aliquot.plate_row:
   c NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.aliquots.aliquot.quantity:
   2 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.aliquots.aliquot.volume:
  20 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.amount:
   2 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.analyte_type:
total rna      NA's 
        1       598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.analyte_type_id:
   t NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.bcr_analyte_barcode:
tcga-06-0145-01a-05t                 NA's 
                   1                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.bcr_analyte_uuid:
8607a2dd-73bb-44c8-a034-6027b5f29201                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               1              598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.concentration:
0.17 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
total rna      NA's 
        1       598 

patient.samples.sample.portions.portion.5.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
      1     598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0145-01a-05w-0224-08                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.2.bcr_aliquot_uuid:
697f25be-24f8-4efe-9e45-a2befc4b84cb                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.2.center_id:
   8 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
  18 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
   7 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.2.plate_id:
 224 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.2.plate_row:
   f NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.2.quantity:
 200 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.2.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.2.volume:
 400 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0145-01a-05w-0225-09                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_uuid:
957d8075-3b5a-4672-a277-5393dd252a87                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.3.center_id:
   9 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.3.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.3.day_of_shipment:
  18 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.3.month_of_shipment:
   7 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.3.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.3.plate_id:
 225 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.3.plate_row:
   f NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.3.quantity:
 200 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.3.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.3.volume:
 400 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.3.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-06-0145-01a-05w-0226-02                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.4.bcr_aliquot_uuid:
42436754-1fa0-47ed-8547-e990f4736c12                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.4.center_id:
   2 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.4.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.4.day_of_shipment:
  18 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.4.month_of_shipment:
   7 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.4.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.4.plate_id:
 226 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.4.plate_row:
   f NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.4.quantity:
  14 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.4.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.4.volume:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0145-01a-05w-0223-10                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_uuid:
e6f21cd4-120f-4885-8ce7-3b47dd7b154b                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.center_id:
  10 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
  18 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   7 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.plate_id:
 223 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.plate_row:
   f NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.quantity:
 200 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.volume:
 400 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.amount:
   0 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                   1                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.analyte_type_id:
   w NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.bcr_analyte_barcode:
tcga-06-0145-01a-05w                 NA's 
                   1                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.bcr_analyte_uuid:
98bed26d-a4f0-42ca-83a4-b73c90a4a121                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      1     598 

patient.samples.sample.portions.portion.5.analytes.analyte.4.spectrophotometer_method:
uv spec    NA's 
      1     598 

patient.samples.sample.portions.portion.5.analytes.analyte.a260_a280_ratio:
1.93 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0145-01a-05d-0215-04                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
1c61cd77-7fa0-41e8-8985-7e0904da868d                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.2.center_id:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.2.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.2.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.2.plate_id:
 215 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.2.plate_row:
   c NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.2.quantity:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.2.volume:
33.3 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0145-01a-05d-0216-06                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
ca6c67e6-3246-4039-b47f-d16934089a18                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.3.center_id:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.3.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.3.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.3.plate_id:
 216 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.3.plate_row:
   c NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.3.quantity:
   3 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.3.volume:
  20 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-06-0145-01a-05d-0217-02                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
ecea1581-dc7e-4121-90ee-353a099db4b3                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.4.center_id:
   2 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.4.concentration:
0.13 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.4.plate_column:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.4.plate_id:
 217 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.4.plate_row:
   g NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.4.quantity:
4.33 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.4.volume:
33.3 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-06-0145-01a-05d-0218-05                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
c14eb1ff-a98e-4e09-8216-fb1c37bc5c3b                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.5.center_id:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.5.concentration:
0.13 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.5.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.5.plate_id:
 218 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.5.plate_row:
   c NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.5.quantity:
4.33 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.5.volume:
33.3 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-06-0145-01a-05d-1003-01                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
7f812173-0cef-42ff-bffb-6eb62a4b3340                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
97544054     NA's 
       1      598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.6.center_id:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.6.concentration:
0.13 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  19 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.6.plate_column:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.6.plate_id:
1003 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.6.plate_row:
   e NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.6.quantity:
0.43 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.6.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.6.volume:
3.33 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2010 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-06-0145-01a-05d-1490-08                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid:
c75b7e96-b1e5-4f45-a7a6-7079d4872f64                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
1049950530       NA's 
         1        598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.7.center_id:
   8 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.7.concentration:
0.05 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   3 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.7.plate_column:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.7.plate_id:
1490 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.7.plate_row:
   h NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.7.quantity:
   2 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.7.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.7.volume:
  40 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2011 NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0145-01a-05d-0214-01 tcga-06-0148-01a-07t-0230-07 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
bf1bd21e-1d51-4abd-9257-6194fe2d60c0 ea39eed2-1f1f-4258-bd34-2b2eff7cccaa 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.center_id:
   1    7 NA's 
   1    1  597 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.concentration:
 0.1 0.15 NA's 
   1    1  597 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.day_of_shipment:
  28   29 NA's 
   1    1  597 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.month_of_shipment:
   6    8 NA's 
   1    1  597 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.plate_column:
   1    4 NA's 
   1    1  597 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.plate_id:
 214  230 NA's 
   1    1  597 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.plate_row:
   c    d NA's 
   1    1  597 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.quantity:
   1    2 NA's 
   1    1  597 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.source_center:
  22 NA's 
   2  597 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.volume:
  10 13.3 NA's 
   1    1  597 

patient.samples.sample.portions.portion.5.analytes.analyte.aliquots.aliquot.year_of_shipment:
2007 NA's 
   2  597 

patient.samples.sample.portions.portion.5.analytes.analyte.amount:
83.37   100  NA's 
    1     1   597 

patient.samples.sample.portions.portion.5.analytes.analyte.analyte_type:
      dna total rna      NA's 
        1         1       597 

patient.samples.sample.portions.portion.5.analytes.analyte.analyte_type_id:
   d    t NA's 
   1    1  597 

patient.samples.sample.portions.portion.5.analytes.analyte.bcr_analyte_barcode:
tcga-06-0145-01a-05d tcga-06-0148-01a-07t                 NA's 
                   1                    1                  597 

patient.samples.sample.portions.portion.5.analytes.analyte.bcr_analyte_uuid:
620a33c7-5201-485d-8240-5e2b59f3e5cf 73d93f83-c4c3-48d6-aa25-a48599017ed5 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.5.analytes.analyte.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               1              598 

patient.samples.sample.portions.portion.5.analytes.analyte.concentration:
 0.1 0.13 NA's 
   1    1  597 

patient.samples.sample.portions.portion.5.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   1  598 

patient.samples.sample.portions.portion.5.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             597 

patient.samples.sample.portions.portion.5.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.5.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type             total rna                  NA's 
                    1                     1                   597 

patient.samples.sample.portions.portion.5.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      2     597 

patient.samples.sample.portions.portion.5.bcr_portion_barcode:
tcga-06-0145-01a-05 tcga-06-0148-01a-07                NA's 
                  1                   1                 597 

patient.samples.sample.portions.portion.5.bcr_portion_uuid:
cba20613-0c1f-4784-8d02-5504f7353a8a d6803061-927e-4c95-b11e-6b7392d9673f 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.5.day_of_creation:
   4 NA's 
   2  597 

patient.samples.sample.portions.portion.5.is_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.5.month_of_creation:
   2 NA's 
   2  597 

patient.samples.sample.portions.portion.5.portion_number:
   5    7 NA's 
   1    1  597 

patient.samples.sample.portions.portion.5.slides.slide.2.bcr_slide_barcode:
tcga-06-0145-01a-05-bs5                    NA's 
                      1                     598 

patient.samples.sample.portions.portion.5.slides.slide.2.bcr_slide_uuid:
f2922c1c-cdaf-4cbd-b032-8a7ff682d8ef                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.5.slides.slide.2.image_file_name:
tcga-06-0145-01a-05-bs5.f2922c1c-cdaf-4cbd-b032-8a7ff682d8ef.svs 
                                                               1 
                                                            NA's 
                                                             598 

patient.samples.sample.portions.portion.5.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.5.slides.slide.2.percent_lymphocyte_infiltration:
   0 NA's 
   1  598 

patient.samples.sample.portions.portion.5.slides.slide.2.percent_monocyte_infiltration:
   0 NA's 
   1  598 

patient.samples.sample.portions.portion.5.slides.slide.2.percent_necrosis:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.5.slides.slide.2.percent_neutrophil_infiltration:
   0 NA's 
   1  598 

patient.samples.sample.portions.portion.5.slides.slide.2.percent_normal_cells:
   0 NA's 
   1  598 

patient.samples.sample.portions.portion.5.slides.slide.2.percent_stromal_cells:
   0 NA's 
   1  598 

patient.samples.sample.portions.portion.5.slides.slide.2.percent_tumor_cells:
  95 NA's 
   1  598 

patient.samples.sample.portions.portion.5.slides.slide.2.percent_tumor_nuclei:
 100 NA's 
   1  598 

patient.samples.sample.portions.portion.5.slides.slide.2.section_location:
bottom   NA's 
     1    598 

patient.samples.sample.portions.portion.5.slides.slide.bcr_slide_barcode:
tcga-06-0145-01a-04-bs4 tcga-06-0148-01a-06-bs6                    NA's 
                      1                       1                     597 

patient.samples.sample.portions.portion.5.slides.slide.bcr_slide_uuid:
208b287a-bcf6-45c6-aabf-e90a79e201fc 35af777a-905c-495a-a4d5-f7bc302a5cb7 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.5.slides.slide.image_file_name:
tcga-06-0145-01a-04-bs4.35af777a-905c-495a-a4d5-f7bc302a5cb7.svs 
                                                               1 
tcga-06-0148-01a-06-bs6.208b287a-bcf6-45c6-aabf-e90a79e201fc.svs 
                                                               1 
                                                            NA's 
                                                             597 

patient.samples.sample.portions.portion.5.slides.slide.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.5.slides.slide.percent_lymphocyte_infiltration:
   0 NA's 
   2  597 

patient.samples.sample.portions.portion.5.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   2  597 

patient.samples.sample.portions.portion.5.slides.slide.percent_necrosis:
   5   50 NA's 
   1    1  597 

patient.samples.sample.portions.portion.5.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   2  597 

patient.samples.sample.portions.portion.5.slides.slide.percent_normal_cells:
   0 NA's 
   2  597 

patient.samples.sample.portions.portion.5.slides.slide.percent_stromal_cells:
   0 NA's 
   2  597 

patient.samples.sample.portions.portion.5.slides.slide.percent_tumor_cells:
  50   95 NA's 
   1    1  597 

patient.samples.sample.portions.portion.5.slides.slide.percent_tumor_nuclei:
 100 NA's 
   2  597 

patient.samples.sample.portions.portion.5.slides.slide.section_location:
 top NA's 
   2  597 

patient.samples.sample.portions.portion.5.year_of_creation:
2009 NA's 
   2  597 

patient.samples.sample.portions.portion.6.analytes.analyte.2.a260_a280_ratio:
1.83 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0145-01a-06r-0213-07                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.2.bcr_aliquot_uuid:
9bbd5891-a989-4ffe-883f-71b84e6abc15                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.2.plate_id:
 213 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   d NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.2.quantity:
   2 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.2.volume:
13.3 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0145-01a-06r-0219-01                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.3.bcr_aliquot_uuid:
f4d84edd-d6e3-4dc5-95d4-637141e0e72d                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.3.center_id:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.3.plate_id:
 219 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   d NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.3.quantity:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.3.volume:
33.3 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-06-0145-01a-06r-0220-02                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_uuid:
0c4292c4-0d98-4e94-b5f2-f16f02ff4a6f                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.4.center_id:
   2 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.4.concentration:
0.14 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   3 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.4.plate_id:
 220 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.4.plate_row:
   g NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.4.quantity:
 2.8 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.4.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.4.volume:
  20 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0145-01a-06r-0212-03                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
31db9a5d-577c-455b-aaea-769e137197b4                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.center_id:
   3 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.plate_id:
 212 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.plate_row:
   d NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.quantity:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.volume:
33.3 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.amount:
  50 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.analyte_type:
 rna NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.analyte_type_id:
   r NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.bcr_analyte_barcode:
tcga-06-0145-01a-06r                 NA's 
                   1                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.bcr_analyte_uuid:
2fac2b3f-0f2e-4abf-aaa9-58b129cc3feb                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               1              598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.concentration:
0.14 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
allprep rna extraction                   NA's 
                     1                    598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.rna.ratio_28s_18s:
 1.5 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.rna.rinvalue:
 8.3 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      1     598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0145-01a-06t-0213-07                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
a88e4636-7378-46ea-9bfc-e94f76d9dd11                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.aliquots.aliquot.center_id:
   7 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.aliquots.aliquot.concentration:
 0.1 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.aliquots.aliquot.plate_column:
   9 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.aliquots.aliquot.plate_id:
 213 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.aliquots.aliquot.plate_row:
   d NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.aliquots.aliquot.quantity:
   2 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.aliquots.aliquot.volume:
  20 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.amount:
   2 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.analyte_type:
total rna      NA's 
        1       598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.analyte_type_id:
   t NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.bcr_analyte_barcode:
tcga-06-0145-01a-06t                 NA's 
                   1                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.bcr_analyte_uuid:
041dee76-fccd-4000-a0cd-6e134b045608                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               1              598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.concentration:
0.11 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
total rna      NA's 
        1       598 

patient.samples.sample.portions.portion.6.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
      1     598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0145-01a-06w-0224-08                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.2.bcr_aliquot_uuid:
9b889b89-d76e-461f-b0bc-4fb60ec21a7a                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.2.center_id:
   8 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
  18 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
   7 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.2.plate_id:
 224 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.2.plate_row:
   g NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.2.quantity:
 200 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.2.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.2.volume:
 400 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0145-01a-06w-0225-09                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_uuid:
62ac6962-16ac-4861-a555-4a0859ddb824                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.3.center_id:
   9 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.3.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.3.day_of_shipment:
  18 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.3.month_of_shipment:
   7 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.3.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.3.plate_id:
 225 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.3.plate_row:
   g NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.3.quantity:
 200 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.3.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.3.volume:
 400 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.3.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-06-0145-01a-06w-0226-02                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.4.bcr_aliquot_uuid:
8cfaa554-c94b-45d2-bf05-1c195753c450                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.4.center_id:
   2 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.4.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.4.day_of_shipment:
  18 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.4.month_of_shipment:
   7 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.4.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.4.plate_id:
 226 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.4.plate_row:
   g NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.4.quantity:
  14 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.4.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.4.volume:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0145-01a-06w-0223-10                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_uuid:
b3b79a5e-855c-4582-997e-7f427082b869                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.center_id:
  10 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
  18 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   7 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.plate_id:
 223 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.plate_row:
   g NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.quantity:
 200 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.volume:
 400 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.amount:
   0 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                   1                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.analyte_type_id:
   w NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.bcr_analyte_barcode:
tcga-06-0145-01a-06w                 NA's 
                   1                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.bcr_analyte_uuid:
4e8e81ee-89ed-4986-8869-b92ba46e2650                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.concentration:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      1     598 

patient.samples.sample.portions.portion.6.analytes.analyte.4.spectrophotometer_method:
uv spec    NA's 
      1     598 

patient.samples.sample.portions.portion.6.analytes.analyte.a260_a280_ratio:
1.89 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-06-0145-01a-06d-0215-04                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
30e6da49-35fa-437d-b4e6-406b3646c1df                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.2.center_id:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.2.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.2.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.2.plate_id:
 215 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.2.plate_row:
   d NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.2.quantity:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.2.volume:
33.3 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-06-0145-01a-06d-0216-06                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
6bcd5208-d574-41f4-bb0e-a22742265046                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.3.center_id:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.3.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.3.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.3.plate_id:
 216 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.3.plate_row:
   d NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.3.quantity:
   3 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.3.volume:
  20 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-06-0145-01a-06d-0217-02                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
759c8a2e-0934-47bd-820e-244d59a0e7fd                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.4.center_id:
   2 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.4.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.4.plate_column:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.4.plate_id:
 217 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.4.plate_row:
   h NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.4.quantity:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.4.volume:
33.3 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-06-0145-01a-06d-0218-05                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
3a6a048e-6bef-41a3-a979-b611d0696143                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.5.center_id:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.5.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.5.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.5.plate_id:
 218 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.5.plate_row:
   d NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.5.quantity:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.5.volume:
33.3 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-06-0145-01a-06d-1003-01                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
c19f57ec-2b20-4a40-97f5-1ac45679be9a                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
97544031     NA's 
       1      598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.6.center_id:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.6.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  19 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.6.plate_column:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.6.plate_id:
1003 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.6.plate_row:
   f NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.6.quantity:
 0.5 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.6.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.6.volume:
3.33 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2010 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-06-0145-01a-06d-1490-08                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid:
8cc5694d-b4a2-43bb-838d-58cc470349fd                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
1049950444       NA's 
         1        598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.7.center_id:
   8 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.7.concentration:
0.05 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   3 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.7.plate_column:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.7.plate_id:
1490 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.7.plate_row:
   a NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.7.quantity:
   2 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.7.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.7.volume:
  40 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2011 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-06-0145-01a-06d-0214-01                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
1906881d-87a2-4543-ab33-c3e07af181b9                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.center_id:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.day_of_shipment:
  28 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.plate_id:
 214 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.plate_row:
   d NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.quantity:
   2 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.volume:
13.3 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.aliquots.aliquot.year_of_shipment:
2007 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.amount:
83.37  NA's 
    1   598 

patient.samples.sample.portions.portion.6.analytes.analyte.analyte_type:
 dna NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.analyte_type_id:
   d NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.bcr_analyte_barcode:
tcga-06-0145-01a-06d                 NA's 
                   1                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.bcr_analyte_uuid:
ab9cc7c1-150f-4389-9f09-ca5b17727082                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.analytes.analyte.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               1              598 

patient.samples.sample.portions.portion.6.analytes.analyte.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             598 

patient.samples.sample.portions.portion.6.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    1                   598 

patient.samples.sample.portions.portion.6.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      1     598 

patient.samples.sample.portions.portion.6.bcr_portion_barcode:
tcga-06-0145-01a-06                NA's 
                  1                 598 

patient.samples.sample.portions.portion.6.bcr_portion_uuid:
4ceae2e9-099d-4cb3-b198-84577426197d                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.day_of_creation:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.6.is_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.month_of_creation:
   2 NA's 
   1  598 

patient.samples.sample.portions.portion.6.portion_number:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.6.slides.slide.2.bcr_slide_barcode:
tcga-06-0145-01a-06-bs6                    NA's 
                      1                     598 

patient.samples.sample.portions.portion.6.slides.slide.2.bcr_slide_uuid:
7ffdd6df-3e88-4b41-8a70-d1dbf9e45514                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.slides.slide.2.image_file_name:
tcga-06-0145-01a-06-bs6.7ffdd6df-3e88-4b41-8a70-d1dbf9e45514.svs 
                                                               1 
                                                            NA's 
                                                             598 

patient.samples.sample.portions.portion.6.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.slides.slide.2.percent_lymphocyte_infiltration:
   0 NA's 
   1  598 

patient.samples.sample.portions.portion.6.slides.slide.2.percent_monocyte_infiltration:
   0 NA's 
   1  598 

patient.samples.sample.portions.portion.6.slides.slide.2.percent_necrosis:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.6.slides.slide.2.percent_neutrophil_infiltration:
   0 NA's 
   1  598 

patient.samples.sample.portions.portion.6.slides.slide.2.percent_normal_cells:
   0 NA's 
   1  598 

patient.samples.sample.portions.portion.6.slides.slide.2.percent_stromal_cells:
   0 NA's 
   1  598 

patient.samples.sample.portions.portion.6.slides.slide.2.percent_tumor_cells:
  95 NA's 
   1  598 

patient.samples.sample.portions.portion.6.slides.slide.2.percent_tumor_nuclei:
 100 NA's 
   1  598 

patient.samples.sample.portions.portion.6.slides.slide.2.section_location:
bottom   NA's 
     1    598 

patient.samples.sample.portions.portion.6.slides.slide.bcr_slide_barcode:
tcga-06-0145-01a-05-bs5                    NA's 
                      1                     598 

patient.samples.sample.portions.portion.6.slides.slide.bcr_slide_uuid:
f2922c1c-cdaf-4cbd-b032-8a7ff682d8ef                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.6.slides.slide.image_file_name:
tcga-06-0145-01a-05-bs5.f2922c1c-cdaf-4cbd-b032-8a7ff682d8ef.svs 
                                                               1 
                                                            NA's 
                                                             598 

patient.samples.sample.portions.portion.6.slides.slide.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.6.slides.slide.percent_lymphocyte_infiltration:
   0 NA's 
   1  598 

patient.samples.sample.portions.portion.6.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   1  598 

patient.samples.sample.portions.portion.6.slides.slide.percent_necrosis:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.6.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   1  598 

patient.samples.sample.portions.portion.6.slides.slide.percent_normal_cells:
   0 NA's 
   1  598 

patient.samples.sample.portions.portion.6.slides.slide.percent_stromal_cells:
   0 NA's 
   1  598 

patient.samples.sample.portions.portion.6.slides.slide.percent_tumor_cells:
  95 NA's 
   1  598 

patient.samples.sample.portions.portion.6.slides.slide.percent_tumor_nuclei:
 100 NA's 
   1  598 

patient.samples.sample.portions.portion.6.slides.slide.section_location:
 top NA's 
   1  598 

patient.samples.sample.portions.portion.6.year_of_creation:
2009 NA's 
   1  598 

patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.300   1.800   1.800   1.812   1.820   2.230      23 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
8.957e+07 8.957e+07 1.012e+08 3.122e+08 1.087e+08 1.050e+09       444 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     3.0     3.0     3.9     6.0     7.0      91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.1500  0.1600  0.1921  0.1700  0.6300      91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   13.00   20.00   19.28   25.00   30.00      91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 508   91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   6.352   9.000  12.000      91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.073   4.000   9.000      91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  178.0   281.0   389.0   599.3   790.0  1847.0      91 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   2.200   2.650   2.974   3.000   7.600      91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
 508   91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   13.30   16.70   16.97   16.70   40.00      91 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2007    2008    2009    2010    2011      91 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
8.957e+07 8.957e+07 9.756e+07 4.951e+08 1.054e+09 1.062e+09       470 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.000   5.486   7.000   7.000     120 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.1500  0.1600  0.1948  0.1700  0.6300     120 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   14.00   23.00   20.11   27.00   30.00     120 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 479  120 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   5.856   8.000  12.000     120 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.727   5.000  11.000     120 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  179.0   282.0   390.0   599.3   791.0  2027.0     120 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.00    2.26    2.93    3.01   13.35     120 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22 NA's 
 479  120 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   13.30   13.30   16.63   16.70   33.30     120 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2007    2008    2009    2010    2011     120 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.651e+07 5.838e+08 1.054e+09 8.111e+08 1.054e+09 1.054e+09       540 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   3.199   4.000   9.000     223 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.1500  0.1600  0.2087  0.1800  0.6300     223 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   27.00   28.00   24.52   28.00   31.00     223 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 376  223 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.750  10.000   7.457  10.000  12.000     223 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   5.348   9.000  12.000     223 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  181.0   296.0   677.0   680.5   678.0  1991.0     223 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.640   1.130   3.096   5.000  20.000     223 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.source_center:
  22 NA's 
 376  223 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    4.00    6.00   15.34   33.30   88.20     223 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2008    2009    2009    2009    2011     223 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.054e+09 1.054e+09 1.054e+09 1.054e+09 1.054e+09 1.062e+09       553 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   4.000   3.176   4.000   9.000     480 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1200  0.1600  0.1700  0.2703  0.4900  0.6300     480 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   27.00   27.00   25.83   28.00   28.00     480 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
 119  480 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   6.000   7.294  10.000  11.000     480 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.941   7.000  12.000     480 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    187     676     676    1067    1849    2027     480 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   2.000   2.960   3.608   5.330  15.000     480 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.source_center:
  22 NA's 
 119  480 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     4.0    15.7    19.5    33.3   100.0     480 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.year_of_shipment:
2007 2008 2009 2011 NA's 
   4   14   55   46  480 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.054e+09 1.054e+09 1.054e+09 1.054e+09 1.054e+09 1.054e+09       584 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.center_id:
   1    4    8    9 NA's 
  15    9    2    1  572 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1500  0.1700  0.1800  0.2741  0.5000  0.5500     572 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.day_of_shipment:
   6   15   27   28 NA's 
   1    2   15    9  572 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
  27  572 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.month_of_shipment:
   4    5    6   10 NA's 
   2    1   15    9  572 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.519   6.000  12.000     572 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    507     676    1849    1309    1849    1850     572 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.640   2.060   5.300   4.915   5.825  20.000     572 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.source_center:
  22 NA's 
  27  572 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    4.00   33.30   25.53   33.30  125.00     572 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.year_of_shipment:
2009 2011 NA's 
  12   15  572 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.7.biospecimen_barcode_bottom:
1053965826 1053965831 1053965833 1053965857       NA's 
         1          1          1          1        595 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.7.center_id:
   1    4 NA's 
   4    1  594 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.7.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.150   0.160   0.180   0.176   0.190   0.200     594 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.7.day_of_shipment:
  27   28 NA's 
   4    1  594 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   5  594 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.7.month_of_shipment:
   6   10 NA's 
   4    1  594 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.7.plate_column:
   2    4    7 NA's 
   1    3    1  594 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.7.plate_id:
 676 1849 NA's 
   1    4  594 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.7.plate_row:
   e    f    h NA's 
   1    2    2  594 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.7.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.600   5.330   5.990   4.982   6.330   6.660     594 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.7.source_center:
  22 NA's 
   5  594 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.7.volume:
   4 33.3 NA's 
   1    4  594 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.7.year_of_shipment:
2009 2011 NA's 
   1    4  594 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.8.bcr_aliquot_barcode:
tcga-06-0184-01a-01r-1849-01                         NA's 
                           1                          598 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.8.bcr_aliquot_uuid:
7522ddf3-0d35-4085-9f94-1ca2e38aa804                                 NA's 
                                   1                                  598 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.8.biospecimen_barcode_bottom:
1053965785       NA's 
         1        598 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.8.center_id:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.8.concentration:
0.15 NA's 
   1  598 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.8.day_of_shipment:
  27 NA's 
   1  598 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   1  598 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.8.month_of_shipment:
   6 NA's 
   1  598 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.8.plate_column:
   4 NA's 
   1  598 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.8.plate_id:
1849 NA's 
   1  598 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.8.plate_row:
   b NA's 
   1  598 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.8.quantity:
   5 NA's 
   1  598 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.8.source_center:
  22 NA's 
   1  598 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.8.volume:
33.3 NA's 
   1  598 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.8.year_of_shipment:
2011 NA's 
   1  598 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.129e+07 9.752e+07 1.087e+08 3.894e+08 1.050e+09 1.062e+09       372 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   2.555   3.000   8.000       8 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.1500  0.1600  0.1837  0.1700  0.6300       8 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   14.00   23.00   20.27   27.00   30.00       8 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 591    8 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.286   9.000  12.000       8 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.051   4.000   9.000       8 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   2.260   3.000   3.449   3.400  26.650       8 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
 574   17    8 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.30   20.00   18.73   20.00   53.30       8 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2007    2009    2009    2010    2014       8 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    37.6    78.0   111.5   154.3   500.0      10 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
genomeplex (rubicon) amplified dna                                rna 
                                42                                544 
                         total rna                               NA's 
                                 5                                  8 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
   g    h    r    t NA's 
  42   53  491    5    8 



patient.samples.sample.portions.portion.analytes.analyte.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               1              598 

patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.1500  0.1600  0.1852  0.1700  0.6300       8 

patient.samples.sample.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
  42  557 

patient.samples.sample.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no NA's 
  42  557 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             591 
                                            NA's 
                                               8 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 591    8 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
   allprep rna extraction                 genomplex mirvana (allprep dna) rna 
                      491                        42                        53 
                total rna                      NA's 
                        5                         8 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.200   1.400   1.453   1.700   4.000      55 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   7.800   8.400   8.257   8.800   9.800     265 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
    591       8 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
  60    9  530 

patient.samples.sample.portions.portion.analytes.analyte.3.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.500   1.700   1.800   1.759   1.800   2.000     508 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.651e+07 1.070e+08 1.070e+08 1.259e+08 1.087e+08 1.054e+09       548 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   3.000   7.000   6.063   7.000  10.000     488 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.1400  0.1500  0.1839  0.1700  0.5000     488 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   23.00   25.00   22.95   27.00   29.00     488 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 111  488 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   6.000   4.667   6.000  10.000     488 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.414   4.500  12.000     488 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  213.0   299.0   656.0   993.8  1847.0  1873.0     488 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.860   2.130  20.210   2.755 200.000     488 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
 111  488 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.90   13.30   13.30   49.27   16.70  400.00     488 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2008    2009    2009    2011    2011     488 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.651e+07 1.136e+08 1.136e+08 3.207e+08 1.136e+08 1.054e+09       590 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.center_id:
   1    3    7    9 NA's 
  10    1   39   11  538 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1100  0.1500  0.1600  0.2198  0.1800  0.5000     538 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   23.00   23.00   22.87   28.00   31.00     538 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  61  538 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   3.803   7.000   9.000     538 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   3.000   2.787   4.000   8.000     538 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  219.0   252.0   300.0   507.3   300.0  1991.0     538 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.46    2.13    2.26   35.50    5.00  200.00     538 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.source_center:
  22 NA's 
  61  538 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.30   13.30   13.30   80.79   33.30  400.00     538 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.year_of_shipment:
2007 2008 2010 2011 NA's 
  18   34    1    8  538 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.biospecimen_barcode_bottom:
1053937450       NA's 
         1        598 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.244   4.000   9.000     554 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1600  0.1700  0.2373  0.1900  0.5000     554 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   27.00   28.00   24.33   28.00   29.00     554 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  45  554 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   7.000  10.000   8.533  10.000  10.000     554 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.444   5.000   8.000     554 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  220.0   254.0   677.0   514.3   677.0  1873.0     554 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.60    0.68    0.76   33.16    4.41  200.00     554 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.source_center:
  22 NA's 
  45  554 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00    4.00    4.00   71.47   28.00  400.00     554 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.year_of_shipment:
2007 2008 2009 2011 NA's 
  17    3   24    1  554 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.biospecimen_barcode_bottom:
1061604836       NA's 
         1        598 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.center_id:
   4    7    9 NA's 
   3    1    6  589 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1325  0.1450  0.1450  0.1500  0.1700     589 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.day_of_shipment:
   8   26   28 NA's 
   6    1    3  589 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  10  589 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.month_of_shipment:
   7   10 NA's 
   6    4  589 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.plate_column:
   1    5    7 NA's 
   6    3    1  589 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.plate_id:
 376  676 2027 NA's 
   6    3    1  589 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.560   0.980   3.820   2.879   4.045   5.000     589 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.source_center:
  22 NA's 
  10  589 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.volume:
   4 13.3 29.4 NA's 
   3    1    6  589 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.year_of_shipment:
2008 2009 2011 NA's 
   6    3    1  589 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.biospecimen_barcode_bottom:
1053965804 1053965854 1053965855       NA's 
         1          1          1        596 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.center_id:
   1    4    8    9 NA's 
   3    3    1    1  591 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1300  0.1400  0.1438  0.1525  0.1700     591 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.day_of_shipment:
  15   25   27   28 NA's 
   1    1    3    3  591 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   8  591 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.month_of_shipment:
   4    6   10   11 NA's 
   1    3    3    1  591 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.75    3.50    4.25    5.25   12.00     591 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.plate_id:
 418  507  676 1849 NA's 
   1    1    3    3  591 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.plate_row:
   a    c    d    h NA's 
   3    1    1    3  591 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.520   0.650   4.240   5.070   5.082  19.990     591 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.source_center:
  22 NA's 
   8  591 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.volume:
    4  29.4  33.3 153.8  NA's 
    3     1     3     1   591 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.year_of_shipment:
2008 2009 2011 NA's 
   1    4    3  591 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-06-0124-01a-01r-0676-04 tcga-06-0126-01a-01r-1849-01 
                           1                            1 
tcga-06-0128-01a-01r-0572-09 tcga-06-0129-01a-01r-1849-01 
                           1                            1 
                        NA's 
                         595 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.bcr_aliquot_uuid:
17c98d8c-8bd4-4b16-a384-0497d730f497 2e55dba8-6510-49bc-995c-de7a623dc6d0 
                                   1                                    1 
77184fe2-f049-4eb1-b024-dbf90c242c61 a61a5c87-233d-4560-bd68-a8ef7dbb982b 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.biospecimen_barcode_bottom:
1053965770 1053965783       NA's 
         1          1        597 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.center_id:
   1    4    9 NA's 
   2    1    1  595 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.concentration:
0.13 0.14 NA's 
   3    1  595 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.day_of_shipment:
  23   27   28 NA's 
   1    2    1  595 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.month_of_shipment:
   6    7   10 NA's 
   2    1    1  595 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.plate_column:
   2    6   12 NA's 
   2    1    1  595 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.plate_id:
 572  676 1849 NA's 
   1    1    2  595 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.plate_row:
   a    b    d NA's 
   2    1    1  595 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.quantity:
0.52    2 4.33 4.66 NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.source_center:
  22 NA's 
   4  595 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.volume:
   4 15.4 33.3 NA's 
   1    1    2  595 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.year_of_shipment:
2009 2011 NA's 
   2    2  595 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.bcr_aliquot_barcode:
tcga-06-0124-01a-01r-1849-01 tcga-06-0128-01a-01r-0676-04 
                           1                            1 
                        NA's 
                         597 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.bcr_aliquot_uuid:
58d61460-88d6-46b3-b47a-e2ba55fad23c eca799a4-cde6-453c-a112-6c0fd9ea2855 
                                   1                                    1 
                                NA's 
                                 597 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.biospecimen_barcode_bottom:
1053965853       NA's 
         1        598 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.center_id:
   1    4 NA's 
   1    1  597 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.concentration:
0.13 NA's 
   2  597 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.day_of_shipment:
  27   28 NA's 
   1    1  597 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   2  597 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.month_of_shipment:
   6   10 NA's 
   1    1  597 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.plate_column:
   1    6 NA's 
   1    1  597 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.plate_id:
 676 1849 NA's 
   1    1  597 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.plate_row:
   f    g NA's 
   1    1  597 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.quantity:
0.52 4.33 NA's 
   1    1  597 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.source_center:
  22 NA's 
   2  597 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.volume:
   4 33.3 NA's 
   1    1  597 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.year_of_shipment:
2009 2011 NA's 
   1    1  597 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.129e+07 9.756e+07 1.070e+08 2.727e+08 1.530e+08 1.054e+09       377 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   7.000   6.094   7.000  10.000      16 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1000  0.1000  0.1275  0.1000  0.5000      16 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   13.00   20.00   19.56   27.00   30.00      16 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 583   16 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.177   9.000  12.000      16 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.578   5.000  10.000      16 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.270   1.000   1.000   3.751   1.950 200.000      16 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22   23 NA's 
 566   17   16 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   10.00   10.00   16.42   17.50  400.00      16 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2007    2009    2009    2010    2014      16 

patient.samples.sample.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   17.00   39.00   57.71   75.00  422.00      34 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                  rna            total rna 
                  28                   91                  464 
                NA's 
                  16 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
   r    t    w NA's 
  91  464   28   16 



patient.samples.sample.portions.portion.analytes.analyte.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               1              598 

patient.samples.sample.portions.portion.analytes.analyte.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1000  0.1000  0.1162  0.1000  0.5000      33 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
  28  571 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no  yes NA's 
  11   17  571 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             583 
                                            NA's 
                                              16 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 583   16 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
allprep rna extraction                repli-g              total rna 
                    91                     28                    464 
                  NA's 
                    16 

patient.samples.sample.portions.portion.analytes.analyte.3.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.008   1.300   1.194   1.470   2.100     455 

patient.samples.sample.portions.portion.analytes.analyte.3.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   7.125   7.800   7.464   8.800   9.700     497 

patient.samples.sample.portions.portion.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
    566      33 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2 NA's 
  55    8  536 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 96504904  96505221  97515722  98849835 103960960 103961317       507 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.center_id:
   8   10 NA's 
 279   59  261 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
 338  261 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    3.25   14.00   13.61   23.00   28.00     261 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 338  261 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   6.314   9.000  11.000     261 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.678   5.000  12.000     261 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  189.0   323.0   611.0   572.6   837.0  1295.0     261 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.quantity:
   5   30  200 NA's 
  11  185  142  261 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.source_center:
  22 NA's 
 338  261 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.volume:
  10   60  400 NA's 
  11  185  142  261 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2007 2008 2009 2010 NA's 
  80   57  109   92  261 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 96508729  96508779  97512924  98841251 103927548 103927621       507 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.center_id:
   8    9 NA's 
  17  321  261 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.concentration:
 0.5 NA's 
 338  261 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    3.25   14.00   13.61   23.00   28.00     261 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 338  261 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   6.314   9.000  11.000     261 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.678   5.000  12.000     261 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  190.0   324.0   613.0   574.1   839.0  1296.0     261 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.quantity:
   5   30  200 NA's 
  11  185  142  261 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.source_center:
  22 NA's 
 338  261 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.volume:
  10   60  400 NA's 
  11  185  142  261 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.year_of_shipment:
2007 2008 2009 2010 NA's 
  80   57  109   92  261 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.center_id:
   2    8    9   10 NA's 
  28   31   17   11  512 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.concentration:
 0.5 NA's 
  87  512 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   12.00   13.00   16.82   22.00   27.00     512 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  87  512 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   5.356   9.000   9.000     512 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.299   5.000   8.000     512 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  207.0   226.0   254.0   280.9   325.0   423.0     512 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.quantity:
  14  200 NA's 
  28   59  512 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.source_center:
  22 NA's 
  87  512 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.volume:
  28  400 NA's 
  28   59  512 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.year_of_shipment:
2007 2008 2009 NA's 
  55   21   11  512 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.5.center_id:
   8 NA's 
  11  588 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.5.concentration:
 0.5 NA's 
  11  588 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.5.day_of_shipment:
   6 NA's 
  11  588 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  11  588 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.5.month_of_shipment:
   1 NA's 
  11  588 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.5.plate_column:
   1    3    4    5 NA's 
   3    1    2    5  588 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.5.plate_id:
 424 NA's 
  11  588 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.5.quantity:
 200 NA's 
  11  588 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.5.source_center:
  22 NA's 
  11  588 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.5.volume:
 400 NA's 
  11  588 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.5.year_of_shipment:
2009 NA's 
  11  588 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.6.center_id:
   9 NA's 
  11  588 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.6.concentration:
 0.5 NA's 
  11  588 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.6.day_of_shipment:
   6 NA's 
  11  588 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
  11  588 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.6.month_of_shipment:
   1 NA's 
  11  588 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.6.plate_column:
   1    3    4    5 NA's 
   3    1    2    5  588 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.6.plate_id:
 425 NA's 
  11  588 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.6.quantity:
 200 NA's 
  11  588 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.6.source_center:
  22 NA's 
  11  588 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.6.volume:
 400 NA's 
  11  588 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.6.year_of_shipment:
2009 NA's 
  11  588 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 96485768  96486602  97515906  98832985 103927452 103927525       507 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   2    7    8   10 NA's 
  48   42   11  279  219 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.1  0.5 NA's 
  42  338  219 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    4.00   18.00   14.84   23.00   29.00     219 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 380  219 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.863   9.000  11.000     219 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.666   5.000  12.000     219 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    188     301     423     539     641    1294     219 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.50   14.00   30.00   66.12   30.00  200.00     219 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
 380  219 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0    28.0    60.0   133.3    60.0   400.0     219 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2007 2008 2009 2010 NA's 
  90   89  109   92  219 

patient.samples.sample.portions.portion.analytes.analyte.4.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00  138.21   39.25 1020.00     219 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna            total rna                 NA's 
                 338                   42                  219 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type_id:
   t    w NA's 
  42  338  219 



patient.samples.sample.portions.portion.analytes.analyte.4.concentration:
0.09  0.1  0.2  0.5 NA's 
   2   39    1  338  219 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
 338  261 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
 338  261 

patient.samples.sample.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             380 
                                            NA's 
                                             219 

patient.samples.sample.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
 380  219 

patient.samples.sample.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
  repli-g total rna      NA's 
      338        42       219 

patient.samples.sample.portions.portion.analytes.analyte.4.spectrophotometer_method:
uv spec    NA's 
    380     219 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.center_id:
   8   10 NA's 
  39   20  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.concentration:
 0.5 NA's 
  59  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.day_of_shipment:
  12   13   18   19 NA's 
   1   30    9   19  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  59  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.month_of_shipment:
   2    7    9   11 NA's 
  30    9    1   19  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.356   3.000   5.000     540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_id:
 224  250  318  410 NA's 
   9    1   30   19  540 


patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.quantity:
   5  200 NA's 
  19   40  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.source_center:
  22 NA's 
  59  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.volume:
  10  400 NA's 
  19   40  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.year_of_shipment:
2007 2008 NA's 
  10   49  540 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.center_id:
   9 NA's 
  59  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.concentration:
 0.5 NA's 
  59  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.day_of_shipment:
  12   13   18   19 NA's 
   1   30    9   19  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  59  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.month_of_shipment:
   2    7    9   11 NA's 
  30    9    1   19  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.356   3.000   5.000     540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.plate_id:
 225  252  319  411 NA's 
   9    1   30   19  540 


patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.quantity:
   5  200 NA's 
  19   40  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.source_center:
  22 NA's 
  59  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.volume:
  10  400 NA's 
  19   40  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.year_of_shipment:
2007 2008 NA's 
  10   49  540 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.center_id:
   2    8 NA's 
  39    1  559 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.concentration:
 0.5 NA's 
  40  559 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.day_of_shipment:
  12   13   18 NA's 
   1   30    9  559 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  40  559 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.month_of_shipment:
   2    7    9 NA's 
  30    9    1  559 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    2.00    2.35    3.00    5.00     559 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.plate_id:
 226  254  320 NA's 
   9    1   30  559 


patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.quantity:
  14  200 NA's 
  39    1  559 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.source_center:
  22 NA's 
  40  559 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.volume:
  28  400 NA's 
  39    1  559 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.year_of_shipment:
2007 2008 NA's 
  10   30  559 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.center_id:
   2    8   10 NA's 
   1   19   39  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.concentration:
 0.5 NA's 
  59  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.day_of_shipment:
  12   13   18   19 NA's 
   1   30    9   19  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  59  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.month_of_shipment:
   2    7    9   11 NA's 
  30    9    1   19  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.356   3.000   5.000     540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_id:
 223  248  317  409 NA's 
   9    1   30   19  540 


patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.quantity:
   5   14  200 NA's 
  19    1   39  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.source_center:
  22 NA's 
  59  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.volume:
  10   28  400 NA's 
  19    1   39  540 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.year_of_shipment:
2007 2008 NA's 
  10   49  540 

patient.samples.sample.portions.portion.analytes.analyte.5.amount:
   0  120 NA's 
  40   19  540 

patient.samples.sample.portions.portion.analytes.analyte.5.analyte_type:
repli-g (qiagen) dna                 NA's 
                  59                  540 

patient.samples.sample.portions.portion.analytes.analyte.5.analyte_type_id:
   w NA's 
  59  540 



patient.samples.sample.portions.portion.analytes.analyte.5.concentration:
 0.5 NA's 
  59  540 

patient.samples.sample.portions.portion.analytes.analyte.5.dna.normal_tumor_genotype_match:
 yes NA's 
  59  540 

patient.samples.sample.portions.portion.analytes.analyte.5.dna.pcr_amplification_successful:
  no NA's 
  59  540 

patient.samples.sample.portions.portion.analytes.analyte.5.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              59 
                                            NA's 
                                             540 

patient.samples.sample.portions.portion.analytes.analyte.5.is_derived_from_ffpe:
  no NA's 
  59  540 

patient.samples.sample.portions.portion.analytes.analyte.5.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
     59     540 

patient.samples.sample.portions.portion.analytes.analyte.5.spectrophotometer_method:
picogreen   uv spec      NA's 
       19        40       540 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.790   1.900   1.900   1.898   1.900   2.000      25 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.bcr_aliquot_barcode:
tcga-06-0152-01a-02d-a45w-05 tcga-06-0210-01a-01d-a45w-05 
                           1                            1 
tcga-06-0221-01a-01d-a45w-05 tcga-16-1460-01a-01d-2294-05 
                           1                            1 
                        NA's 
                         595 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.bcr_aliquot_uuid:
08506a52-971a-4d31-bc29-85e7b8dfbab3 1c67bb3c-3aca-46a4-9b45-696e2c842e49 
                                   1                                    1 
44a646bb-538e-4425-a397-990500d6aef3 adbc6369-5071-4291-a6f3-d8c0cd9d9876 
                                   1                                    1 
                                NA's 
                                 595 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.biospecimen_barcode_bottom:
120990307 167788716 167788812 167814100      NA's 
        1         1         1         1       595 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.center_id:
   5 NA's 
   4  595 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.concentration:
0.15 0.16 0.17 0.18 NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.day_of_shipment:
   4   24 NA's 
   1    3  595 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.is_derived_from_ffpe:
  no NA's 
   4  595 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.month_of_shipment:
   6    9 NA's 
   1    3  595 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_column:
   1    5 NA's 
   3    1  595 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_id:
2294 a45w NA's 
   1    3  595 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_row:
   b    e    g NA's 
   1    2    1  595 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.quantity:
2.08 2.21 2.34    6 NA's 
   1    1    1    1  595 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.source_center:
  22   23 NA's 
   1    3  595 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.volume:
  13   40 NA's 
   3    1  595 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.year_of_shipment:
2012 2014 NA's 
   1    3  595 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.129e+07 9.752e+07 1.087e+08 4.047e+08 1.050e+09 1.054e+09       371 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   3.472   4.000   8.000      33 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1500  0.1500  0.1691  0.1700  0.6700      33 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   13.00   23.00   20.28   28.00   30.00      33 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 566   33 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.417   9.000  12.000      33 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.348   4.000  10.000      33 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.300   3.000   3.760   3.853   4.270   7.200      33 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
 549   17   33 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   20.00   26.70   24.43   26.70   40.00      33 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2007    2009    2009    2010    2014      33 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
8.957e+07 9.751e+07 1.087e+08 4.057e+08 1.050e+09 1.054e+09       372 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   5.000   6.000   5.749   6.000   8.000      41 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1500  0.1500  0.1646  0.1700  0.6700      41 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   13.00   20.00   20.27   28.00   30.00      41 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 558   41 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.495   9.000  12.000      41 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.419   5.000  10.000      41 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.840   2.000   2.120   2.546   2.985   8.000      41 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
 541   17   41 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   13.30   13.30   18.41   13.30   60.00      41 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2007    2009    2009    2010    2014      41 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
8.958e+07 9.752e+07 1.087e+08 4.210e+08 1.050e+09 1.054e+09       389 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   3.426   5.000   8.000      59 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1500  0.1583  0.1700  0.6700      59 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   13.00   20.00   19.97   28.00   30.00      59 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 540   59 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.439   9.000  12.000      59 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.206   4.000   9.000      59 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  185.0   278.0   520.0   729.9   914.0  2280.0      59 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.040   2.800   3.000   3.309   3.340  13.400      59 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
 540   59 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.80   20.00   20.00   23.65   20.00   60.00      59 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2007    2009    2009    2010    2012      59 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 89573766  89573817  97515339  95178147  97516259 103949990       499 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   1    2    5    8 NA's 
   2    1  400   22  174 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1500  0.1500  0.1722  0.1700  0.6700     174 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   13.00   20.00   19.12   28.00   30.00     174 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
 425  174 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   6.261   9.000  11.000     174 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.209   4.000  11.000     174 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  186.0   279.0   392.0   496.1   595.0  1353.0     174 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.320   1.860   2.000   2.227   2.240   6.700     174 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
 425  174 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   10.00   13.30   14.36   13.30   40.00     174 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2007 2008 2009 2010 NA's 
 159   73   93  100  174 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 9.754e+07 9.754e+07 5.204e+08 1.050e+09 1.050e+09       358 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   5.000   4.732   8.000  10.000     229 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.1500  0.1518  0.1700  0.6700     229 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    3.00    7.00   10.19   18.00   27.00     229 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
 370  229 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   5.000   5.373  11.000  12.000     229 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.351   6.000  12.000     229 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  209.0   512.0  1003.0   931.6  1492.0  2294.0     229 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.100   0.530   1.075   1.798   2.000  64.790     229 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22 NA's 
 370  229 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.60    3.33    6.70   18.40   40.00  341.00     229 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2009    2010    2010    2011    2012     229 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 9.754e+07 1.050e+09 6.841e+08 1.050e+09 1.076e+09       438 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   8.000   6.581   9.000   9.000     384 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.1400  0.1252  0.1700  0.5700     384 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   3.000   7.000   9.433  15.000  29.000     384 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
 215  384 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   4.000   2.902   4.000  11.000     384 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   3.846   5.000  12.000     384 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.140   1.500   2.000   2.289   3.750   9.990     384 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  22   23 NA's 
 214    1  384 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.685  30.000  26.581  40.000  60.000     384 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2008    2010    2010    2010    2011    2014     384 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 1.050e+09 1.050e+09 8.216e+08 1.050e+09 1.076e+09       538 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000   8.000   7.079   8.000   9.000     536 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.050   0.050   0.050   0.091   0.130   0.570     536 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   3.000   3.000   8.365  13.000  29.000     536 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
  63  536 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   2.444   4.000   9.000     536 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   2.714   4.000  10.000     536 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.340   1.500   2.000   1.712   2.000   2.080     536 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  22   23 NA's 
  60    3  536 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   13.00   40.00   30.24   40.00   40.00     536 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2011    2011    2011    2011    2014     536 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.087e+08 1.210e+08 1.678e+08 4.882e+08 1.050e+09 1.076e+09       578 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.center_id:
   5    8    9 NA's 
   7    7    7  578 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.0500  0.0800  0.1033  0.1500  0.1700     578 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    3.00    4.00   11.76   21.00   29.00     578 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe:
  no NA's 
  21  578 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   4.000   4.619   8.000  10.000     578 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_column:
   1    2    4    5 NA's 
  10    1    5    5  578 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.500   2.000   2.485   2.000   6.400     578 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.source_center:
  22   23 NA's 
  15    6  578 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   13.00   30.00   26.94   40.00   40.00     578 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2011    2012    2012    2013    2015     578 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.129e+07 9.752e+07 1.040e+08 2.602e+08 1.087e+08 1.054e+09       372 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    5    6    7 NA's 
 522   32   36    3    6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.1500  0.1500  0.1772  0.1700  0.6700       6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   14.00   23.00   20.41   28.00   30.00       6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 593    6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.189   9.000  12.000       6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.381   4.000  10.000       6 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.560   1.000   1.010   1.518   1.140   7.320       6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
 576   17    6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.000   6.670   6.700   8.724   6.700  33.300       6 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2007    2007    2009    2009    2010    2014       6 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   78.97  211.35  259.39  395.00 1189.73       6 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
                               dna genomeplex (rubicon) amplified dna 
                               587                                  3 
                               rna                          total rna 
                                 1                                  2 
                              NA's 
                                 6 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
   d    g    r    t NA's 
 587    3    1    2    6 



patient.samples.sample.portions.portion.analytes.analyte.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
portioning study             NA's 
               2              597 

patient.samples.sample.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1500  0.1500  0.1775  0.1700  0.6700       6 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
 590    9 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
 150  440    9 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             593 
                                            NA's 
                                               6 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
 593    6 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
 adna preparation type allprep rna extraction              genomplex 
                   587                      1                      3 
             total rna                   NA's 
                     2                      6 

patient.samples.sample.portions.portion.analytes.analyte.rna.ratio_28s_18s:
   1 NA's 
   1  598 

patient.samples.sample.portions.portion.analytes.analyte.rna.rinvalue:
 8.3 NA's 
   1  598 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
       17       576         6 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
  60    9  530 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.00    4.00   10.44   17.00   31.00       5 

patient.samples.sample.portions.portion.is_ffpe:
  no NA's 
 594    5 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   4.557   8.000  12.000       5 

patient.samples.sample.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   2.377   1.000  31.000       5 

patient.samples.sample.portions.portion.portion_sequence:
   1    2    3 NA's 
  22    6    1  570 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
 562   37 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   0   50   70  100 NA's 
  40    1    1    7  550 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   0   50  100 NA's 
  12    1    1  585 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   5.000   9.904  15.000 100.000      37 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   0   80  100 NA's 
  15    1    4  579 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.182   0.000 100.000      56 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   4.301   5.000  90.000      45 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   80.00   90.00   83.93   98.00  100.00      40 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   80.00   90.00   88.54  100.00  100.00      37 

patient.samples.sample.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
    38    524     37 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
 594    5 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00   13.56    0.00  100.00     527 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   5.649   0.000 100.000     562 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    5.00   10.91   18.00   85.00       5 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00   10.65    0.00  100.00     556 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00    2.47    0.00  100.00      24 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   4.976   5.000 100.000      10 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   75.00   85.00   81.67   95.00  100.00       8 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0      80      90      88     100     100       5 

patient.samples.sample.portions.portion.slides.slide.section_location:
bottom    top   NA's 
   524     70      5 

patient.samples.sample.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   18.0    71.0   112.0   109.5   139.0   872.0      94 

patient.samples.sample.portions.portion.year_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2008    2009    2009    2009    2010    2014       5 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
 232  367 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
 232  367 


patient.samples.sample.portions.shipment_portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   13.00   21.00   18.66   21.00   41.00     367 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   1    2    3 NA's 
   1   11    1  586 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
   9   12   20 NA's 
  16    2  214  367 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   3    7 NA's 
   2  230  367 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2011 2012 2014 NA's 
 214    2   16  367 

patient.samples.sample.sample_type:
primary tumor          NA's 
          595             4 

patient.samples.sample.sample_type_id:
   1 NA's 
 595    4 

patient.samples.sample.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0250  0.3000  0.4000  0.4352  0.6000  1.0000      93 

patient.samples.sample.vial_number:
   a    b    c NA's 
 543   46    6    4 


Study:
Glioblastoma multiforme 
                    599 

BCR:
IGC NCH 
592   7 

Whole.exome:
 No Yes 
277 322 

Whole.genome:
 No Yes 
547  52 

RNAseq:
 No Yes 
445 154 

SNP6:
 No Yes 
 28 571 

U133a:
 No Yes 
 71 528 

HM450:
 No Yes 
470 129 

HM27:
 No Yes 
314 285 

RPPA:
 No Yes 
384 215 

Histology:
glioblastoma         NA's 
         590            9 

Grade:
  G4 NA's 
 590    9 

Age..years.at.diagnosis.:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   50.00   59.00   57.82   68.00   89.00       9 

Gender:
female   male   NA's 
   228    362      9 

Survival..months.:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
  0.06571   5.23214  10.53000  14.81416  17.56916 127.50987         9 

Vital.status..1.dead.:
   0    1 NA's 
 145  444   10 

Karnofsky.Performance.Score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   70.00   80.00   77.45   80.00  100.00     164 

Mutation.Count:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   40.00   51.00   51.81   63.00  190.00     292 

Percent.aneuploidy:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
0.000007 0.150216 0.188242 0.252049 0.278424 0.947696       17 

IDH.status:
Mutant     WT   NA's 
    38    441    120 

X1p.19q.codeletion:
    codel non-codel      NA's 
        2       575        22 

IDH.codel.subtype:
    IDHmut-codel IDHmut-non-codel            IDHwt             NA's 
               2               33              426              138 

MGMT.promoter.status:
  Methylated Unmethylated         NA's 
         188          226          185 

Chr.7.gain.Chr.10.loss:
Gain chr 7 & loss chr 10          No combined CNA                     NA's 
                     378                      193                       28 

Chr.19.20.co.gain:
   Gain chr 19/20 No chr 19/20 gain              NA's 
               93               478                28 

TERT.promoter.status:
Mutant     WT   NA's 
    32      5    562 

TERT.expression..log2.:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   2.807   4.357   3.983   5.484  10.510     445 

TERT.expression.status:
    Expressed Not expressed          NA's 
          127            27           445 

ATRX.status:
Mutant     WT   NA's 
    21    286    292 

DAXX.status:
Mutant     WT   NA's 
     2    305    292 

Telomere.Maintenance:
ATRX TERT NA's 
   5   30  564 

BRAF.V600E.status:
Mutant     WT   NA's 
     4    303    292 

BRAF.KIAA1549.fusion:
  WT NA's 
 154  445 

ABSOLUTE.purity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1700  0.6250  0.7800  0.7338  0.8800  1.0000      56 

ABSOLUTE.ploidy:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.080   1.950   2.000   2.259   2.060   6.620      56 

ESTIMATE.stromal.score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -264.3   802.1  1266.2  1340.7  1719.6  3537.9     445 

ESTIMATE.immune.score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  177.6  1343.7  2030.7  1967.5  2559.8  4285.2     445 

ESTIMATE.combined.score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -61.17 2212.34 3277.18 3308.16 4327.24 7521.69     445 


Transcriptome.Subtype:
  CL   ME   NE   PN NA's 
 157  178   62  107   95 







RPPA.cluster:
  K1   K2 NA's 
 214    1  384 

Telomere.length.estimate.in.blood.normal..Kb.:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.239   5.122   6.034   5.795   6.430   8.870     547 

Telomere.length.estimate.in.tumor..Kb.:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.238   2.390   3.960   4.933   5.423  16.872     547 

}}
\keyword{datasets}
