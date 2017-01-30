\name{ OV }
\alias{ OV }
\docType{data}
\title{ OV }

\details{
\preformatted{
> OV
A MultiAssayExperiment object of 12 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 12: 
 [1] RNASeqGene: ExpressionSet with 19990 rows and 299 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 307 columns 
 [3] miRNASeqGene: ExpressionSet with 705 rows and 461 columns 
 [4] CNASNP: RangedRaggedAssay with 873768 rows and 1145 columns 
 [5] CNVSNP: RangedRaggedAssay with 254437 rows and 1141 columns 
 [6] CNACGH: RangedRaggedAssay with 126479 rows and 472 columns 
 [7] Methylation: SummarizedExperiment with 27578 rows and 591 columns 
 [8] mRNAArray: ExpressionSet with 18632 rows and 575 columns 
 [9] miRNAArray: ExpressionSet with 821 rows and 573 columns 
 [10] RPPAArray: ExpressionSet with 208 rows and 427 columns 
 [11] gistica: SummarizedExperiment with 24776 rows and 573 columns 
 [12] gistict: SummarizedExperiment with 24776 rows and 573 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( OV )
CharacterList of length 12
[["RNASeqGene"]] AADACL3 AADACL4 ABCA4 ABCB10 ABCD3 ... UTY VCY XGPY2 XKRY2 ZFY
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1:61735-603590 1:625470-32650911 ... 24:28659717-59018259
[["CNVSNP"]] 1:3218610-31621427 1:31623667-31625389 ... 23:35717552-154905589
[["CNACGH"]] 1:554267-736482 1:742532-1888036 ... 24:23283688-57440808
[["Methylation"]] cg00000292 cg00002426 cg00003994 ... cg27662877 cg27665659
[["mRNAArray"]] C9orf152 ELMO2 RPS11 CREB3L1 PNMA1 ... SNRPD2 SLC39A6 CTSC AQP7
[["miRNAArray"]] DarkCorner dmr_285 dmr_3 ... NegativeControl SCorner3
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
...
<2 more elements>

> colnames( OV )
CharacterList of length 12
[["RNASeqGene"]] TCGA-04-1348-01A-01R-1565-13 ... TCGA-61-2113-01A-01R-1568-13
[["RNASeq2GeneNorm"]] TCGA-04-1348-01A-01R-1565-13 ...
[["miRNASeqGene"]] TCGA-04-1331-01A-01R-1569-13 ...
[["CNASNP"]] TCGA-04-1331-01A-01D-0428-01 ... TCGA-WR-A838-10A-01D-A402-01
[["CNVSNP"]] TCGA-04-1331-01A-01D-0428-01 ... TCGA-WR-A838-10A-01D-A402-01
[["CNACGH"]] TCGA-04-1331-01A-01D-0431-02 ... TCGA-25-1329-10A-01D-0459-02
[["Methylation"]] TCGA-04-1331-01A-01D-0432-05 ... TCGA-72-4241-01A-01D-1011-05
[["mRNAArray"]] TCGA-04-1331-01A-01R-0435-03 ... TCGA-61-2614-01A-01R-1140-03
[["miRNAArray"]] TCGA-04-1331-01A-01T-0438-07 ... TCGA-61-2614-01A-01T-1142-07
[["RPPAArray"]] TCGA-04-1335-01A-21-1561-20 ... TCGA-WR-A838-01A-21-A45I-20
...
<2 more elements>

Sizes of each ExperimentList element:

             assay size.Mb
1       RNASeqGene  1.1 Mb
2  RNASeq2GeneNorm  1.2 Mb
3     miRNASeqGene  0.1 Mb
4           CNASNP 68.6 Mb
5           CNVSNP 23.5 Mb
6           CNACGH  9.8 Mb
7      Methylation  4.4 Mb
8        mRNAArray  1.1 Mb
9       miRNAArray  0.2 Mb
10       RPPAArray  0.1 Mb
11         gistica  4.4 Mb
12         gistict  4.4 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   247 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 343.00  343.00    2.94    2.76    3.17 


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  26.00   51.00   59.00   59.78   69.00   89.00      21 

vital_status:
  0   1 
246 344 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0   567.5  1073.0  1147.4  1557.0  4624.0     247 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     349     889    1094    1560    5481      75 

tumor_tissue_site:
         omentum            ovary peritoneum ovary             NA's 
               3              575                2               10 

gender:
female   NA's 
   580     10 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1992    2001    2004    2004    2007    2013      10 

radiation_therapy:
  no  yes NA's 
 556    5   29 

karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  40.00   60.00   80.00   75.83   80.00  100.00     506 

histological_type:
serous cystadenocarcinoma                      NA's 
                      580                        10 

residual_tumor:
  r0   r1   r2   rx NA's 
  15   31    5    3  536 


ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    11                    338                    241 

admin.bcr:
international genomics consortium    nationwide children's hospital 
                               10                               580 

admin.day_of_dcc_upload:
  1  15 
580  10 

admin.disease_code:
 ov 
590 


admin.month_of_dcc_upload:
 10  12 
580  10 

admin.patient_withdrawal.withdrawn:
false  NA's 
  580    10 

admin.project_code:
tcga NA's 
 580   10 

admin.year_of_dcc_upload:
2010 2015 
  10  580 

patient.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  26.00   51.00   59.00   59.76   69.00   89.00      10 

patient.anatomic_neoplasm_subdivision:
bilateral      left     right      NA's 
      395        82        69        44 


patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    9.00   12.76   24.00   31.00      10 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -32853  -25213  -21582  -22012  -18757   -9760      21 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      8     527    1021    1064    1470    4624     294 

patient.days_to_initial_pathologic_diagnosis:
   0 NA's 
 580   10 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   276.0   802.0   958.9  1373.0  5481.0      81 



patient.drugs.drug.10.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   12.25   17.00   16.98   23.75   29.00     540 

patient.drugs.drug.10.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   79.0   659.5   995.0  1098.4  1473.0  2745.0     547 

patient.drugs.drug.10.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    6.0   509.5   938.5  1039.3  1382.0  3808.0     542 


patient.drugs.drug.10.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.00    5.00    6.86   11.00   12.00     540 

patient.drugs.drug.10.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.227   6.000  17.000     546 

patient.drugs.drug.10.prescribed_dose:
 300 NA's 
   1  589 

patient.drugs.drug.10.prescribed_dose_units:
   auc     mg mg/day  mg/m2   NA's 
     3     29      1      7    550 

patient.drugs.drug.10.regimen_indication:
               adjuvant other: specify in notes             progression 
                      7                       1                      25 
             recurrence                    NA's 
                     17                     540 

patient.drugs.drug.10.regimen_indication_notes:
persistant       NA's 
         1        589 

patient.drugs.drug.10.regimen_number:
   6 NA's 
   1  589 

patient.drugs.drug.10.route_of_administrations.route_of_administration:
intra-peritoneal (ip)      intravenous (iv)                  oral 
                    2                    39                     2 
                 NA's 
                  547 

patient.drugs.drug.10.route_of_administrations.route_of_administration.2:
intravenous (iv)             NA's 
               1              589 

patient.drugs.drug.10.therapy_ongoing:
  no  yes NA's 
  48    1  541 

patient.drugs.drug.10.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                        49                          1 
                      NA's 
                       540 

patient.drugs.drug.10.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      2      90     500    7144    2275  210000     551 

patient.drugs.drug.10.total_dose_units:
   auc     mg mg/day  mg/m2   NA's 
     3     29      1      7    550 

patient.drugs.drug.10.year_of_form_completion:
2009 2010 2012 NA's 
  34   15    1  540 



patient.drugs.drug.11.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    11.5    16.0    16.7    24.0    28.0     547 

patient.drugs.drug.11.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    117     606     946    1097    1425    2951     553 

patient.drugs.drug.11.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     14     547     827    1004    1398    2819     551 


patient.drugs.drug.11.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   6.233  11.000  12.000     547 

patient.drugs.drug.11.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   5.938   7.250  18.000     558 

patient.drugs.drug.11.prescribed_dose:
  72 NA's 
   1  589 

patient.drugs.drug.11.prescribed_dose_units:
  auc    mg mg/kg mg/m2  NA's 
    2    25     1     6   556 

patient.drugs.drug.11.regimen_indication:
               adjuvant other: specify in notes             progression 
                      7                       1                      21 
             recurrence                    NA's 
                     14                     547 

patient.drugs.drug.11.regimen_indication_notes:
recurrent and progression                      NA's 
                        1                       589 

patient.drugs.drug.11.regimen_number:
   4 NA's 
   1  589 

patient.drugs.drug.11.route_of_administrations.route_of_administration:
intra-peritoneal (ip)      intravenous (iv)                  oral 
                    1                    30                     4 
                 NA's 
                  555 

patient.drugs.drug.11.route_of_administrations.route_of_administration.2:
intravenous (iv)             NA's 
               1              589 

patient.drugs.drug.11.therapy_ongoing:
  no  yes NA's 
  41    1  548 

patient.drugs.drug.11.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                        37                          1 
targeted molecular therapy                       NA's 
                         5                        547 

patient.drugs.drug.11.total_dose:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
    0.665    32.722   117.500  1004.534  1375.000 11299.900       556 

patient.drugs.drug.11.total_dose_units:
  auc    mg mg/kg mg/m2  NA's 
    2    25     1     6   556 

patient.drugs.drug.11.year_of_form_completion:
2009 2010 2012 NA's 
  27   15    1  547 



patient.drugs.drug.12.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   13.00   19.00   18.42   26.00   28.00     559 

patient.drugs.drug.12.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  167.0   612.8  1142.5  1192.5  1559.8  3018.0     562 

patient.drugs.drug.12.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      2     557    1104    1131    1659    2788     561 


patient.drugs.drug.12.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   5.806  10.000  12.000     559 

patient.drugs.drug.12.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.750   5.500   5.542   7.250  17.000     566 

patient.drugs.drug.12.prescribed_dose:
 400 NA's 
   1  589 

patient.drugs.drug.12.prescribed_dose_units:
  auc    mg mg/kg mg/m2  NA's 
    1    17     1     3   568 

patient.drugs.drug.12.regimen_indication:
   adjuvant progression  recurrence        NA's 
          2          17          12         559 

patient.drugs.drug.12.regimen_number:
   5 NA's 
   1  589 

patient.drugs.drug.12.route_of_administrations.route_of_administration:
intra-peritoneal (ip)      intravenous (iv)                  oral 
                    1                    24                     2 
                 NA's 
                  563 

patient.drugs.drug.12.route_of_administrations.route_of_administration.2:
intravenous (iv)             NA's 
               1              589 

patient.drugs.drug.12.therapy_ongoing:
  no  yes NA's 
  30    1  559 

patient.drugs.drug.12.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                        27                          1 
             immunotherapy targeted molecular therapy 
                         1                          2 
                      NA's 
                       559 

patient.drugs.drug.12.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0   204.0   461.5  3071.9  1008.8 39200.0     568 

patient.drugs.drug.12.total_dose_units:
  auc    mg mg/kg mg/m2  NA's 
    1    17     1     3   568 

patient.drugs.drug.12.year_of_form_completion:
2009 2010 2012 NA's 
  18   12    1  559 



patient.drugs.drug.13.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   10.50   17.00   17.55   24.00   28.00     568 

patient.drugs.drug.13.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  110.0   280.8   613.0   819.2   865.0  2737.0     570 

patient.drugs.drug.13.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    6.0   140.0   539.0   720.3   749.0  2627.0     571 


patient.drugs.drug.13.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   5.591   7.750  12.000     568 

patient.drugs.drug.13.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.000   6.000   6.222   7.000  14.000     572 

patient.drugs.drug.13.prescribed_dose:
  60 NA's 
   1  589 

patient.drugs.drug.13.prescribed_dose_units:
   mg mg/m2  NA's 
   15     1   574 

patient.drugs.drug.13.regimen_indication:
   adjuvant progression  recurrence        NA's 
          5           7          10         568 

patient.drugs.drug.13.regimen_number:
   3 NA's 
   1  589 

patient.drugs.drug.13.route_of_administrations.route_of_administration:
intra-peritoneal (ip)      intravenous (iv)                  oral 
                    1                    16                     1 
other (specify below)                  NA's 
                    1                   571 

patient.drugs.drug.13.therapy_ongoing:
  no  yes NA's 
  21    1  568 

patient.drugs.drug.13.therapy_types.therapy_type:
   chemotherapy hormone therapy            NA's 
             21               1             568 

patient.drugs.drug.13.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   32.4   108.2  1190.0  2207.4  4342.5  7398.0     574 

patient.drugs.drug.13.total_dose_units:
   mg mg/m2  NA's 
   15     1   574 

patient.drugs.drug.13.year_of_form_completion:
2009 2010 2012 NA's 
  12    9    1  568 



patient.drugs.drug.14.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   12.75   20.00   18.29   24.00   28.00     576 

patient.drugs.drug.14.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  110.0   186.5   509.0   648.4   933.5  2052.0     579 

patient.drugs.drug.14.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0    35.0   509.0   726.2   743.0  3124.0     577 


patient.drugs.drug.14.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.250   4.500   5.929   7.750  11.000     576 

patient.drugs.drug.14.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.750   6.000   6.583   7.250  12.000     578 

patient.drugs.drug.14.prescribed_dose_units:
 auc   mg NA's 
   2   10  578 

patient.drugs.drug.14.regimen_indication:
   adjuvant progression  recurrence        NA's 
          5           5           4         576 

patient.drugs.drug.14.route_of_administrations.route_of_administration:
intra-peritoneal (ip)      intravenous (iv)                  NA's 
                    3                     9                   578 

patient.drugs.drug.14.route_of_administrations.route_of_administration.2:
intravenous (iv)             NA's 
               3              587 

patient.drugs.drug.14.therapy_ongoing:
  no  yes NA's 
  12    2  576 

patient.drugs.drug.14.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                        13                          1 
                      NA's 
                       576 

patient.drugs.drug.14.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   98.85 1080.00 1788.62 2887.50 6250.00     578 

patient.drugs.drug.14.total_dose_units:
 auc   mg NA's 
   2   10  578 

patient.drugs.drug.14.year_of_form_completion:
2009 2010 NA's 
   8    6  576 



patient.drugs.drug.15.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   12.00   16.00   17.33   24.00   28.00     581 

patient.drugs.drug.15.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  110.0   498.5   714.5   958.4  1355.8  2395.0     582 

patient.drugs.drug.15.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0   276.5   557.0   666.0   941.0  1668.0     583 


patient.drugs.drug.15.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   4.000   5.333   7.000  11.000     581 

patient.drugs.drug.15.number_cycles:
   3    5    6    8 NA's 
   1    2    2    1  584 

patient.drugs.drug.15.prescribed_dose_units:
  mg NA's 
   5  585 

patient.drugs.drug.15.regimen_indication:
   adjuvant progression  recurrence        NA's 
          2           4           3         581 

patient.drugs.drug.15.route_of_administrations.route_of_administration:
intra-peritoneal (ip)      intravenous (iv)                  NA's 
                    1                     7                   582 

patient.drugs.drug.15.route_of_administrations.route_of_administration.2:
intravenous (iv)             NA's 
               1              589 

patient.drugs.drug.15.therapy_ongoing:
  no NA's 
   9  581 

patient.drugs.drug.15.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         8                          1 
                      NA's 
                       581 

patient.drugs.drug.15.total_dose:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
    5.00    25.25   740.00  4089.07  5225.00 17378.00      583 

patient.drugs.drug.15.total_dose_units:
  mg NA's 
   5  585 

patient.drugs.drug.15.year_of_form_completion:
2009 2010 NA's 
   5    4  581 



patient.drugs.drug.16.day_of_form_completion:
   5   16   24   28 NA's 
   2    1    1    1  585 

patient.drugs.drug.16.days_to_drug_therapy_end:
1008 1112 1732 1781 NA's 
   1    1    1    1  586 

patient.drugs.drug.16.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    875    1112    1564    1580    1590    2758     585 


patient.drugs.drug.16.month_of_form_completion:
   4    7    8   11 NA's 
   2    1    1    1  585 

patient.drugs.drug.16.number_cycles:
   1    7    9   11 NA's 
   1    1    1    1  586 

patient.drugs.drug.16.prescribed_dose_units:
   mg mg/m2  NA's 
    4     1   585 

patient.drugs.drug.16.regimen_indication:
progression  recurrence        NA's 
          3           2         585 

patient.drugs.drug.16.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               4              586 

patient.drugs.drug.16.therapy_ongoing:
  no  yes NA's 
   4    1  585 

patient.drugs.drug.16.therapy_types.therapy_type:
   chemotherapy hormone therapy            NA's 
              4               1             585 

patient.drugs.drug.16.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     20      60     305    9024   12240   32496     585 

patient.drugs.drug.16.total_dose_units:
   mg mg/m2  NA's 
    4     1   585 

patient.drugs.drug.16.year_of_form_completion:
2009 2010 NA's 
   4    1  585 

patient.drugs.drug.17.bcr_drug_barcode:
tcga-09-1662-d24247 tcga-24-1467-d26058                NA's 
                  1                   1                 588 

patient.drugs.drug.17.bcr_drug_uuid:
dc5a3d32-2094-4917-8a07-9bbfa17045f5 faac15d8-07b9-42b6-908d-b0fbc92e4927 
                                   1                                    1 
                                NA's 
                                 588 

patient.drugs.drug.17.day_of_form_completion:
   5   16 NA's 
   1    1  588 

patient.drugs.drug.17.days_to_drug_therapy_end:
 141 3059 NA's 
   1    1  588 

patient.drugs.drug.17.days_to_drug_therapy_start:
  32 2989 NA's 
   1    1  588 

patient.drugs.drug.17.drug_name:
bevacizumab      gemzar        NA's 
          1           1         588 

patient.drugs.drug.17.month_of_form_completion:
   4    8 NA's 
   1    1  588 

patient.drugs.drug.17.number_cycles:
   6 NA's 
   1  589 

patient.drugs.drug.17.prescribed_dose_units:
   mg mg/m2  NA's 
    1     1   588 

patient.drugs.drug.17.regimen_indication:
   adjuvant progression        NA's 
          1           1         588 

patient.drugs.drug.17.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               2              588 

patient.drugs.drug.17.therapy_ongoing:
  no NA's 
   2  588 

patient.drugs.drug.17.therapy_types.therapy_type:
              chemotherapy targeted molecular therapy 
                         1                          1 
                      NA's 
                       588 

patient.drugs.drug.17.total_dose:
 600 8871 NA's 
   1    1  588 

patient.drugs.drug.17.total_dose_units:
   mg mg/m2  NA's 
    1     1   588 

patient.drugs.drug.17.year_of_form_completion:
2009 NA's 
   2  588 

patient.drugs.drug.18.bcr_drug_barcode:
tcga-09-1662-d24249 tcga-24-1467-d26052                NA's 
                  1                   1                 588 

patient.drugs.drug.18.bcr_drug_uuid:
0deb02e0-1be4-4b66-9b33-f2ab838a750e 588fd714-20ca-4600-90be-76b54f2c0ef8 
                                   1                                    1 
                                NA's 
                                 588 

patient.drugs.drug.18.day_of_form_completion:
   5   16 NA's 
   1    1  588 

patient.drugs.drug.18.days_to_drug_therapy_end:
1233 2889 NA's 
   1    1  588 

patient.drugs.drug.18.days_to_drug_therapy_start:
1128 2819 NA's 
   1    1  588 

patient.drugs.drug.18.drug_name:
 cytoxen taxotere     NA's 
       1        1      588 

patient.drugs.drug.18.month_of_form_completion:
   4    8 NA's 
   1    1  588 

patient.drugs.drug.18.number_cycles:
   6 NA's 
   1  589 

patient.drugs.drug.18.prescribed_dose_units:
   mg mg/m2  NA's 
    1     1   588 

patient.drugs.drug.18.regimen_indication:
progression  recurrence        NA's 
          1           1         588 

patient.drugs.drug.18.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               2              588 

patient.drugs.drug.18.therapy_ongoing:
  no  yes NA's 
   1    1  588 

patient.drugs.drug.18.therapy_types.therapy_type:
chemotherapy         NA's 
           2          588 

patient.drugs.drug.18.total_dose:
  50   60 NA's 
   1    1  588 

patient.drugs.drug.18.total_dose_units:
   mg mg/m2  NA's 
    1     1   588 

patient.drugs.drug.18.year_of_form_completion:
2009 NA's 
   2  588 

patient.drugs.drug.19.bcr_drug_barcode:
tcga-09-1662-d24246 tcga-24-1467-d26045                NA's 
                  1                   1                 588 

patient.drugs.drug.19.bcr_drug_uuid:
678644d3-1d86-43d5-828b-5ff5529a97d5 825e30c9-5a77-4cd9-8b02-62cb9cb2000f 
                                   1                                    1 
                                NA's 
                                 588 

patient.drugs.drug.19.day_of_form_completion:
   5   16 NA's 
   1    1  588 

patient.drugs.drug.19.days_to_drug_therapy_end:
  15 1909 NA's 
   1    1  588 

patient.drugs.drug.19.days_to_drug_therapy_start:
  15 1804 NA's 
   1    1  588 

patient.drugs.drug.19.drug_name:
taxol  NA's 
    2   588 

patient.drugs.drug.19.month_of_form_completion:
   4    8 NA's 
   1    1  588 

patient.drugs.drug.19.number_cycles:
   1    6 NA's 
   1    1  588 

patient.drugs.drug.19.prescribed_dose_units:
   mg mg/m2  NA's 
    1     1   588 

patient.drugs.drug.19.regimen_indication:
  adjuvant recurrence       NA's 
         1          1        588 

patient.drugs.drug.19.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               2              588 

patient.drugs.drug.19.therapy_ongoing:
  no  yes NA's 
   1    1  588 

patient.drugs.drug.19.therapy_types.therapy_type:
chemotherapy         NA's 
           2          588 

patient.drugs.drug.19.total_dose:
 175 1620 NA's 
   1    1  588 

patient.drugs.drug.19.total_dose_units:
   mg mg/m2  NA's 
    1     1   588 

patient.drugs.drug.19.year_of_form_completion:
2009 NA's 
   2  588 



patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   16.00   15.78   24.00   31.00      75 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0   142.5   178.0   362.4   343.5  3494.0     151 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -8.0    24.0    41.0   254.6   265.0  3389.0     109 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease            complete response 
                           1                            5 
            partial response                         NA's 
                           2                          582 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.802  11.000  12.000      75 

patient.drugs.drug.2.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   5.535   6.000  18.000     162 

patient.drugs.drug.2.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0   108.8   330.0   421.8   559.5  1600.0     560 

patient.drugs.drug.2.prescribed_dose_units:
   auc     mg mg/day  mg/m2   NA's 
    12    225      2    111    240 

patient.drugs.drug.2.regimen_indication:
               adjuvant other: specify in notes             progression 
                    361                      12                      62 
             recurrence                    NA's 
                     71                      84 


patient.drugs.drug.2.route_of_administrations.route_of_administration:
intra-peritoneal (ip)      intravenous (iv)                  oral 
                   59                   400                     3 
other (specify below)                  NA's 
                    1                   127 

patient.drugs.drug.2.route_of_administrations.route_of_administration.2:
intravenous (iv)             NA's 
              29              561 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
 457   11  122 

patient.drugs.drug.2.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                       497                          8 
             immunotherapy targeted molecular therapy 
                         5                          4 
                      NA's 
                        76 

patient.drugs.drug.2.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0   160.0   579.5  1570.5  2096.0 22920.0     248 

patient.drugs.drug.2.total_dose_units:
   auc     mg mg/day  mg/m2   NA's 
    12    225      2    111    240 

patient.drugs.drug.2.tx_on_clinical_trial:
  no NA's 
   9  581 

patient.drugs.drug.2.year_of_form_completion:
2009 2010 2013 2014 NA's 
 400  106    4    5   75 

patient.drugs.drug.20.bcr_drug_barcode:
tcga-09-1662-d24250                NA's 
                  1                 589 

patient.drugs.drug.20.bcr_drug_uuid:
31879e3c-bfe5-4c2f-955d-f4e7af0fbdfb                                 NA's 
                                   1                                  589 

patient.drugs.drug.20.day_of_form_completion:
   5 NA's 
   1  589 

patient.drugs.drug.20.days_to_drug_therapy_end:
1732 NA's 
   1  589 

patient.drugs.drug.20.days_to_drug_therapy_start:
1590 NA's 
   1  589 

patient.drugs.drug.20.drug_name:
carboplatin        NA's 
          1         589 

patient.drugs.drug.20.month_of_form_completion:
   8 NA's 
   1  589 

patient.drugs.drug.20.number_cycles:
   9 NA's 
   1  589 

patient.drugs.drug.20.regimen_indication:
recurrence       NA's 
         1        589 

patient.drugs.drug.20.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              589 

patient.drugs.drug.20.therapy_ongoing:
  no NA's 
   1  589 

patient.drugs.drug.20.therapy_types.therapy_type:
chemotherapy         NA's 
           1          589 

patient.drugs.drug.20.total_dose:
   5 NA's 
   1  589 

patient.drugs.drug.20.year_of_form_completion:
2009 NA's 
   1  589 

patient.drugs.drug.21.bcr_drug_barcode:
tcga-09-1662-d24256                NA's 
                  1                 589 

patient.drugs.drug.21.bcr_drug_uuid:
1161bec0-3bbb-49c2-88af-0a7123eb3c6a                                 NA's 
                                   1                                  589 

patient.drugs.drug.21.day_of_form_completion:
   5 NA's 
   1  589 

patient.drugs.drug.21.days_to_drug_therapy_end:
2650 NA's 
   1  589 

patient.drugs.drug.21.days_to_drug_therapy_start:
2426 NA's 
   1  589 

patient.drugs.drug.21.drug_name:
topotecan      NA's 
        1       589 

patient.drugs.drug.21.month_of_form_completion:
   8 NA's 
   1  589 

patient.drugs.drug.21.number_cycles:
   9 NA's 
   1  589 

patient.drugs.drug.21.prescribed_dose_units:
mg/m2  NA's 
    1   589 

patient.drugs.drug.21.regimen_indication:
progression        NA's 
          1         589 

patient.drugs.drug.21.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              589 

patient.drugs.drug.21.therapy_ongoing:
  no NA's 
   1  589 

patient.drugs.drug.21.therapy_types.therapy_type:
chemotherapy         NA's 
           1          589 

patient.drugs.drug.21.total_dose:
   2 NA's 
   1  589 

patient.drugs.drug.21.total_dose_units:
mg/m2  NA's 
    1   589 

patient.drugs.drug.21.year_of_form_completion:
2009 NA's 
   1  589 

patient.drugs.drug.22.bcr_drug_barcode:
tcga-09-1662-d24265                NA's 
                  1                 589 

patient.drugs.drug.22.bcr_drug_uuid:
de3b0446-d6ae-4fee-ad51-8c322e73ee15                                 NA's 
                                   1                                  589 

patient.drugs.drug.22.day_of_form_completion:
   5 NA's 
   1  589 

patient.drugs.drug.22.days_to_drug_therapy_end:
2517 NA's 
   1  589 

patient.drugs.drug.22.days_to_drug_therapy_start:
2503 NA's 
   1  589 

patient.drugs.drug.22.drug_name:
avastin    NA's 
      1     589 

patient.drugs.drug.22.month_of_form_completion:
   8 NA's 
   1  589 

patient.drugs.drug.22.prescribed_dose_units:
mg/kg  NA's 
    1   589 

patient.drugs.drug.22.regimen_indication:
progression        NA's 
          1         589 

patient.drugs.drug.22.therapy_ongoing:
  no NA's 
   1  589 

patient.drugs.drug.22.therapy_types.therapy_type:
targeted molecular therapy                       NA's 
                         1                        589 

patient.drugs.drug.22.total_dose:
  10 NA's 
   1  589 

patient.drugs.drug.22.total_dose_units:
mg/kg  NA's 
    1   589 

patient.drugs.drug.22.year_of_form_completion:
2009 NA's 
   1  589 

patient.drugs.drug.23.bcr_drug_barcode:
tcga-09-1662-d24255                NA's 
                  1                 589 

patient.drugs.drug.23.bcr_drug_uuid:
7bf5f35f-f997-4287-83d2-d5da4cbd556a                                 NA's 
                                   1                                  589 

patient.drugs.drug.23.day_of_form_completion:
   5 NA's 
   1  589 

patient.drugs.drug.23.days_to_drug_therapy_end:
2399 NA's 
   1  589 

patient.drugs.drug.23.days_to_drug_therapy_start:
2073 NA's 
   1  589 

patient.drugs.drug.23.drug_name:
doxil  NA's 
    1   589 

patient.drugs.drug.23.month_of_form_completion:
   8 NA's 
   1  589 

patient.drugs.drug.23.number_cycles:
  12 NA's 
   1  589 

patient.drugs.drug.23.prescribed_dose_units:
mg/m2  NA's 
    1   589 

patient.drugs.drug.23.regimen_indication:
recurrence       NA's 
         1        589 

patient.drugs.drug.23.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              589 

patient.drugs.drug.23.therapy_ongoing:
  no NA's 
   1  589 

patient.drugs.drug.23.therapy_types.therapy_type:
chemotherapy         NA's 
           1          589 

patient.drugs.drug.23.total_dose:
  40 NA's 
   1  589 

patient.drugs.drug.23.total_dose_units:
mg/m2  NA's 
    1   589 

patient.drugs.drug.23.year_of_form_completion:
2009 NA's 
   1  589 



patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   16.00   15.46   23.00   30.00     221 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0   142.0   187.0   412.5   509.0  2629.0     269 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -8.0    28.0    59.0   314.5   424.0  2585.0     233 


patient.drugs.drug.3.measure_of_response:
complete response  partial response              NA's 
                1                 1               588 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.707  11.000  12.000     221 

patient.drugs.drug.3.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   5.468   6.000  20.000     276 

patient.drugs.drug.3.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   38.0   135.0   282.5   475.4   600.0  3140.0     566 

patient.drugs.drug.3.prescribed_dose_units:
  auc    mg mg/kg mg/m2  NA's 
   14   177     2    73   324 

patient.drugs.drug.3.regimen_indication:
               adjuvant other: specify in notes             progression 
                    228                       6                      61 
             recurrence                    NA's 
                     72                     223 

patient.drugs.drug.3.regimen_indication_notes:
adjuvant and progression            consolidation              neoadjuvant 
                       2                        1                        1 
              persistent                     NA's 
                       1                      585 

patient.drugs.drug.3.regimen_number:
   2 NA's 
   1  589 

patient.drugs.drug.3.route_of_administrations.route_of_administration:
intra-peritoneal (ip)      intravenous (iv)                  oral 
                   46                   279                     5 
other (specify below)                  NA's 
                    1                   259 

patient.drugs.drug.3.route_of_administrations.route_of_administration.2:
intravenous (iv)             NA's 
              21              569 

patient.drugs.drug.3.therapy_ongoing:
  no  yes NA's 
 335    9  246 

patient.drugs.drug.3.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                       344                         11 
             immunotherapy targeted molecular therapy 
                         2                         11 
                      NA's 
                       222 

patient.drugs.drug.3.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    91.0   445.5  1629.9  1965.0 19320.0     332 

patient.drugs.drug.3.total_dose_units:
  auc    mg mg/kg mg/m2  NA's 
   14   177     2    73   324 

patient.drugs.drug.3.tx_on_clinical_trial:
  no NA's 
   2  588 

patient.drugs.drug.3.year_of_form_completion:
2009 2010 2011 2014 NA's 
 283   83    1    2  221 



patient.drugs.drug.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   15.00   14.67   20.00   30.00     330 

patient.drugs.drug.4.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   15.0   153.0   355.0   574.9   846.0  3101.0     366 

patient.drugs.drug.4.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    31.0   251.0   470.5   744.0  3073.0     345 


patient.drugs.drug.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.896  11.000  12.000     330 

patient.drugs.drug.4.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   5.315   6.000  14.000     374 

patient.drugs.drug.4.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     40     137     296     514     546    2704     573 

patient.drugs.drug.4.prescribed_dose_units:
   auc     mg mg/day  mg/m2   NA's 
     9    138      1     40    402 

patient.drugs.drug.4.regimen_indication:
               adjuvant other: specify in notes             progression 
                    120                       9                      64 
             recurrence                    NA's 
                     66                     331 


patient.drugs.drug.4.regimen_number:
   2 NA's 
   1  589 

patient.drugs.drug.4.route_of_administrations.route_of_administration:
intra-peritoneal (ip)      intravenous (iv)                  oral 
                   26                   203                     7 
                 NA's 
                  354 

patient.drugs.drug.4.route_of_administrations.route_of_administration.2:
intravenous (iv)             NA's 
              17              573 

patient.drugs.drug.4.therapy_ongoing:
  no  yes NA's 
 240    9  341 

patient.drugs.drug.4.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                       244                          7 
             immunotherapy targeted molecular therapy 
                         3                          5 
                      NA's 
                       331 

patient.drugs.drug.4.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0   116.5   700.0  1969.7  2180.0 28756.0     407 

patient.drugs.drug.4.total_dose_units:
   auc     mg mg/day  mg/m2   NA's 
     9    138      1     40    402 

patient.drugs.drug.4.tx_on_clinical_trial:
 yes NA's 
   1  589 

patient.drugs.drug.4.year_of_form_completion:
2009 2010 2011 2014 NA's 
 187   71    1    1  330 



patient.drugs.drug.5.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   14.00   14.87   22.75   30.00     396 

patient.drugs.drug.5.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0   180.5   618.5   730.5  1080.8  2697.0     420 

patient.drugs.drug.5.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   51.25  460.50  620.55  972.50 2684.00     408 


patient.drugs.drug.5.measure_of_response:
complete response              NA's 
                1               589 

patient.drugs.drug.5.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.892  11.000  12.000     396 

patient.drugs.drug.5.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   5.571   7.000  29.000     428 


patient.drugs.drug.5.prescribed_dose_units:
  auc    mg mg/m2  NA's 
    8   110    29   443 

patient.drugs.drug.5.regimen_indication:
               adjuvant other: specify in notes             progression 
                     71                       6                      63 
             recurrence                    NA's 
                     53                     397 

patient.drugs.drug.5.regimen_indication_notes:
                         persistence persitant disease; given with gemzar 
                                   1                                    1 
          progression and recurrence              switched from tamozifen 
                                   1                                    1 
                                NA's 
                                 586 

patient.drugs.drug.5.regimen_number:
   2    3 NA's 
   2    2  586 

patient.drugs.drug.5.route_of_administrations.route_of_administration:
intra-peritoneal (ip)      intravenous (iv)                  oral 
                   13                   156                     5 
                 NA's 
                  416 

patient.drugs.drug.5.route_of_administrations.route_of_administration.2:
intravenous (iv)             NA's 
               7              583 

patient.drugs.drug.5.therapy_ongoing:
  no  yes NA's 
 189    1  400 

patient.drugs.drug.5.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                       185                          3 
targeted molecular therapy                       NA's 
                         5                        397 

patient.drugs.drug.5.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      4     142     575    1997    1931   24798     450 

patient.drugs.drug.5.total_dose_units:
  auc    mg mg/m2  NA's 
    8   110    29   443 

patient.drugs.drug.5.tx_on_clinical_trial:
 yes NA's 
   1  589 

patient.drugs.drug.5.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2009    2009    2009    2010    2014     396 



patient.drugs.drug.6.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   15.00   15.31   23.50   30.00     443 

patient.drugs.drug.6.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0   230.5   743.0   860.0  1175.2  3165.0     458 

patient.drugs.drug.6.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0   145.8   584.5   737.3  1086.5  3079.0     450 


patient.drugs.drug.6.measure_of_response:
clinical progressive disease                         NA's 
                           1                          589 

patient.drugs.drug.6.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   6.796  11.000  12.000     443 

patient.drugs.drug.6.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   5.543   6.000  19.000     463 

patient.drugs.drug.6.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0   113.5   242.0   336.9   543.0   800.0     583 

patient.drugs.drug.6.prescribed_dose_units:
  auc    mg mg/kg mg/m2  NA's 
    4    90     3    20   473 

patient.drugs.drug.6.regimen_indication:
               adjuvant other: specify in notes             progression 
                     42                       6                      47 
             recurrence                    NA's 
                     51                     444 


patient.drugs.drug.6.regimen_number:
   2    4 NA's 
   2    1  587 

patient.drugs.drug.6.route_of_administrations.route_of_administration:
intra-peritoneal (ip)      intravenous (iv)                  oral 
                    6                   119                     6 
other (specify below)                  NA's 
                    1                   458 

patient.drugs.drug.6.route_of_administrations.route_of_administration.2:
intravenous (iv)             NA's 
               4              586 

patient.drugs.drug.6.therapy_ongoing:
  no  yes NA's 
 142    3  445 

patient.drugs.drug.6.therapy_types.therapy_type:
              chemotherapy              immunotherapy 
                       139                          2 
targeted molecular therapy                       NA's 
                         5                        444 

patient.drugs.drug.6.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0   131.5   675.0  1915.5  2255.5 18000.0     475 

patient.drugs.drug.6.total_dose_units:
  auc    mg mg/kg mg/m2  NA's 
    4    90     3    20   473 

patient.drugs.drug.6.tx_on_clinical_trial:
  no NA's 
   1  589 

patient.drugs.drug.6.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2009    2009    2009    2010    2014     443 



patient.drugs.drug.7.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   15.00   15.13   22.75   30.00     472 

patient.drugs.drug.7.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   43.0   213.0   738.0   823.8  1140.5  2737.0     491 

patient.drugs.drug.7.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0    99.0   615.0   738.7  1017.0  3124.0     481 


patient.drugs.drug.7.measure_of_response:
stable disease           NA's 
             1            589 

patient.drugs.drug.7.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   6.585  11.000  12.000     472 

patient.drugs.drug.7.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   5.711   7.000  20.000     493 

patient.drugs.drug.7.prescribed_dose:
 2.5  267  454  800 NA's 
   1    1    1    1  586 

patient.drugs.drug.7.prescribed_dose_units:
  auc    mg mg/m2  NA's 
    3    70    13   504 

patient.drugs.drug.7.regimen_indication:
               adjuvant other: specify in notes             progression 
                     31                       6                      44 
             recurrence                    NA's 
                     36                     473 


patient.drugs.drug.7.regimen_number:
   3    5 NA's 
   1    1  588 

patient.drugs.drug.7.route_of_administrations.route_of_administration:
intra-peritoneal (ip)      intravenous (iv)                  oral 
                    8                    89                     5 
                 NA's 
                  488 

patient.drugs.drug.7.route_of_administrations.route_of_administration.2:
intravenous (iv)             NA's 
               3              587 

patient.drugs.drug.7.therapy_ongoing:
  no  yes NA's 
 114    2  474 

patient.drugs.drug.7.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                       113                          2 
             immunotherapy targeted molecular therapy 
                         1                          2 
                      NA's 
                       472 

patient.drugs.drug.7.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      2     130     820    1799    2000   20684     505 

patient.drugs.drug.7.total_dose_units:
  auc    mg mg/m2  NA's 
    3    70    13   504 

patient.drugs.drug.7.tx_on_clinical_trial:
  no NA's 
   1  589 

patient.drugs.drug.7.year_of_form_completion:
2009 2010 2011 2012 NA's 
  79   36    1    2  472 



patient.drugs.drug.8.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   16.00   15.39   23.00   30.00     501 

patient.drugs.drug.8.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0   390.8   836.0   973.4  1381.0  3165.0     512 

patient.drugs.drug.8.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0   277.8   703.5   880.6  1314.0  3079.0     506 


patient.drugs.drug.8.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   6.697  11.000  12.000     501 

patient.drugs.drug.8.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    5.50    5.46    7.00   17.00     516 

patient.drugs.drug.8.prescribed_dose:
 675 NA's 
   1  589 

patient.drugs.drug.8.prescribed_dose_units:
   auc     mg mg/day  mg/m2   NA's 
     5     49      1      9    526 

patient.drugs.drug.8.regimen_indication:
               adjuvant other: specify in notes             progression 
                     18                       3                      43 
             recurrence                    NA's 
                     25                     501 

patient.drugs.drug.8.regimen_indication_notes:
                persistent progression and recurrence 
                         1                          1 
                      NA's 
                       588 

patient.drugs.drug.8.regimen_number:
   6 NA's 
   1  589 

patient.drugs.drug.8.route_of_administrations.route_of_administration:
intra-peritoneal (ip)      intravenous (iv)                  oral 
                    5                    71                     3 
                 NA's 
                  511 

patient.drugs.drug.8.route_of_administrations.route_of_administration.2:
intravenous (iv)             NA's 
               4              586 

patient.drugs.drug.8.therapy_ongoing:
  no  yes NA's 
  85    2  503 

patient.drugs.drug.8.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                        84                          3 
             immunotherapy targeted molecular therapy 
                         1                          1 
                      NA's 
                       501 

patient.drugs.drug.8.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.5    50.0   279.0   976.8   810.0 15000.0     525 

patient.drugs.drug.8.total_dose_units:
   auc     mg mg/day  mg/m2   NA's 
     5     49      1      9    526 

patient.drugs.drug.8.year_of_form_completion:
2009 2010 2012 NA's 
  61   27    1  501 



patient.drugs.drug.9.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   16.00   16.08   23.00   30.00     524 

patient.drugs.drug.9.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     79     570     832    1021    1414    3154     532 

patient.drugs.drug.9.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0   477.2   778.5   994.1  1464.5  3808.0     526 


patient.drugs.drug.9.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   6.667  11.000  12.000     524 

patient.drugs.drug.9.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.047   6.000  28.000     537 

patient.drugs.drug.9.prescribed_dose:
 266 NA's 
   1  589 



patient.drugs.drug.9.regimen_indication_notes:
                                 persistant 
                                          1 
persistant disease; given 1x with cisplatin 
                                          1 
                                 persistent 
                                          1 
                                       NA's 
                                        587 

patient.drugs.drug.9.regimen_number:
   4 NA's 
   1  589 

patient.drugs.drug.9.route_of_administrations.route_of_administration:
intra-peritoneal (ip)      intravenous (iv)                  oral 
                    5                    50                     1 
                 NA's 
                  534 

patient.drugs.drug.9.route_of_administrations.route_of_administration.2:
intravenous (iv)             NA's 
               4              586 

patient.drugs.drug.9.therapy_ongoing:
  no  yes NA's 
  63    1  526 

patient.drugs.drug.9.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                        58                          4 
             immunotherapy targeted molecular therapy 
                         1                          3 
                      NA's 
                       524 

patient.drugs.drug.9.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1     120     560    2141    1848   20400     537 


patient.drugs.drug.9.year_of_form_completion:
2009 2010 2012 NA's 
  45   20    1  524 



patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.50   16.00   15.78   24.00   31.00      67 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0   142.0   173.0   371.8   325.0  3273.0     137 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -8.0    26.0    42.0   257.2   245.0  3168.0      99 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                           1                            6 
            partial response                         NA's 
                           1                          582 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.795  11.000  12.000      67 

patient.drugs.drug.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   5.000   6.000   5.608   6.000  20.000     149 

patient.drugs.drug.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   15.0   180.0   309.0   401.1   564.0  1120.0     561 


patient.drugs.drug.regimen_indication:
               adjuvant other: specify in notes             progression 
                    374                      11                      62 
             recurrence                    NA's 
                     67                      76 


patient.drugs.drug.regimen_number:
   2 NA's 
   1  589 

patient.drugs.drug.route_of_administrations.route_of_administration:
intra-peritoneal (ip)      intravenous (iv)                  oral 
                   63                   405                    10 
other (specify below)                  NA's 
                    1                   111 

patient.drugs.drug.route_of_administrations.route_of_administration.2:
intravenous (iv)             NA's 
              37              553 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
 463   11  116 

patient.drugs.drug.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                       502                          8 
             immunotherapy targeted molecular therapy 
                         1                         11 
                      NA's 
                        68 

patient.drugs.drug.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.5    75.0   607.5  1682.4  2007.8 30000.0     252 


patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
   8    1  581 

patient.drugs.drug.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2009    2009    2009    2009    2014      67 

patient.eastern_cancer_oncology_group:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.5484  1.0000  4.0000     466 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    11                    338                    241 




patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   30.00   30.00   29.17   31.00   31.00     507 

patient.follow_ups.follow_up.2.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    317    1145    1552    1703    2012    3819     545 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    442    2192    2541    2660    3475    4665     552 

patient.follow_ups.follow_up.2.eastern_cancer_oncology_group:
   1 NA's 
   2  588 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             2                              1 
                          NA's 
                           587 

patient.follow_ups.follow_up.2.followup_treatment_success:
complete remission/response         progressive disease 
                         16                          46 
             stable disease                        NA's 
                          9                         519 

patient.follow_ups.follow_up.2.lost_follow_up:
  no NA's 
   3  587 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   7.000   6.843   7.000   8.000     507 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
 yes NA's 
   3  587 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no NA's 
   3  587 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
 616 NA's 
   1  589 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 286  697 NA's 
   1    1  588 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
metastatic       NA's 
         1        589 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   1  589 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.progression_determined_by:
imaging study          NA's 
            1           589 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
 yes NA's 
   7  583 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   7  583 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  226.0   378.0   578.0   720.8   921.5  2997.0     523 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
progression of disease             recurrence                   NA's 
                     3                      4                    583 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.progression_determined_by:
      imaging study molecular marker(s)                NA's 
                  3                   1                 586 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  12   62  516 

patient.follow_ups.follow_up.2.performance_status_scale_timing:
post-adjuvant therapy                  NA's 
                    2                   588 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
        18         59        513 

patient.follow_ups.follow_up.2.postoperative_rx_tx:
  no  yes NA's 
   1    8  581 

patient.follow_ups.follow_up.2.primary_therapy_outcome_success:
complete remission/response         progressive disease 
                          4                           3 
             stable disease                        NA's 
                          1                         582 

patient.follow_ups.follow_up.2.radiation_therapy:
  no NA's 
   9  581 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
   38    45   507 

patient.follow_ups.follow_up.2.year_of_form_completion:
2011 2012 2014 2015 NA's 
   5    1    2   75  507 

patient.follow_ups.follow_up.3.bcr_followup_barcode:
tcga-29-a5nz-f59527 tcga-42-2582-f34655 tcga-42-2589-f39385 tcga-42-2591-f41420 
                  1                   1                   1                   1 
               NA's 
                586 

patient.follow_ups.follow_up.3.bcr_followup_uuid:
01f5c3af-b966-47cb-80a1-cc1b65e03cb8 45f7924c-d860-471c-8589-a438ddf08bd9 
                                   1                                    1 
70638d89-1fbf-4a94-a6fc-242ed16a6937 b059fe0d-3a99-4fb1-afeb-7e44ca131810 
                                   1                                    1 
                                NA's 
                                 586 

patient.follow_ups.follow_up.3.day_of_form_completion:
  14   15   18 NA's 
   1    1    2  586 

patient.follow_ups.follow_up.3.days_to_death:
1088 1447 NA's 
   1    1  588 

patient.follow_ups.follow_up.3.days_to_last_followup:
1240 1278 NA's 
   1    1  588 

patient.follow_ups.follow_up.3.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             2                              2 
                          NA's 
                           586 

patient.follow_ups.follow_up.3.followup_treatment_success:
complete remission/response  partial remission/response 
                          1                           1 
        progressive disease                        NA's 
                          2                         586 

patient.follow_ups.follow_up.3.lost_follow_up:
  no NA's 
   4  586 

patient.follow_ups.follow_up.3.month_of_form_completion:
   3    5 NA's 
   3    1  586 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
 yes NA's 
   3  587 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
   2    1  587 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 364  396 1073 NA's 
   1    1    1  587 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
recurrence       NA's 
         1        589 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.2.progression_determined_by:
imaging study          NA's 
            2           588 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
 yes NA's 
   1  589 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
  no NA's 
   1  589 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
1028 NA's 
   1  589 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.3.progression_determined_by:
imaging study          NA's 
            1           589 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
 yes NA's 
   3  587 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
   2    1  587 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
 396 1028 1073 NA's 
   1    1    1  587 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
progression of disease             recurrence                   NA's 
                     2                      1                    587 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.progression_determined_by:
imaging study          NA's 
            2           588 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   1    3  586 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
         1          3        586 

patient.follow_ups.follow_up.3.postoperative_rx_tx:
 yes NA's 
   4  586 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success:
complete remission/response              stable disease 
                          3                           1 
                       NA's 
                        586 

patient.follow_ups.follow_up.3.radiation_therapy:
  no NA's 
   4  586 

patient.follow_ups.follow_up.3.vital_status:
alive  dead  NA's 
    2     2   586 

patient.follow_ups.follow_up.3.year_of_form_completion:
2013 2014 NA's 
   3    1  586 

patient.follow_ups.follow_up.4.bcr_followup_barcode:
tcga-42-2582-f34657 tcga-42-2589-f41366                NA's 
                  1                   1                 588 

patient.follow_ups.follow_up.4.bcr_followup_uuid:
099527cd-4d0d-4c42-a39c-5a80120abfa4 0bd8d506-45ac-46fd-b54c-7fec74ee4880 
                                   1                                    1 
                                NA's 
                                 588 

patient.follow_ups.follow_up.4.day_of_form_completion:
  15   18 NA's 
   1    1  588 

patient.follow_ups.follow_up.4.days_to_death:
1447 NA's 
   1  589 

patient.follow_ups.follow_up.4.days_to_last_followup:
1278 NA's 
   1  589 

patient.follow_ups.follow_up.4.followup_case_report_form_submission_reason:
additional new tumor event                       NA's 
                         2                        588 

patient.follow_ups.follow_up.4.followup_treatment_success:
progressive disease                NA's 
                  2                 588 

patient.follow_ups.follow_up.4.lost_follow_up:
  no NA's 
   2  588 

patient.follow_ups.follow_up.4.month_of_form_completion:
   3 NA's 
   2  588 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
 yes NA's 
   1  589 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
 yes NA's 
   1  589 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
1179 NA's 
   1  589 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.2.progression_determined_by:
imaging study          NA's 
            1           589 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
 yes NA's 
   2  588 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.additional_radiation_therapy:
 yes NA's 
   2  588 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
1179 1274 NA's 
   1    1  588 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
progression of disease                   NA's 
                     1                    589 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event.progression_determined_by:
imaging study          NA's 
            1           589 

patient.follow_ups.follow_up.4.new_tumor_events.new_tumor_event_after_initial_treatment:
 yes NA's 
   2  588 

patient.follow_ups.follow_up.4.person_neoplasm_cancer_status:
with tumor       NA's 
         2        588 

patient.follow_ups.follow_up.4.postoperative_rx_tx:
 yes NA's 
   2  588 

patient.follow_ups.follow_up.4.primary_therapy_outcome_success:
complete remission/response              stable disease 
                          1                           1 
                       NA's 
                        588 

patient.follow_ups.follow_up.4.radiation_therapy:
  no NA's 
   2  588 

patient.follow_ups.follow_up.4.vital_status:
alive  dead  NA's 
    1     1   588 

patient.follow_ups.follow_up.4.year_of_form_completion:
2013 NA's 
   2  588 



patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    9.00   12.71   24.00   31.00      17 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0   538.5  1024.0  1072.7  1473.2  4624.0     298 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    8.0   284.5   817.5   961.7  1381.8  5481.0      82 

patient.follow_ups.follow_up.eastern_cancer_oncology_group:
   0    1    2    3 NA's 
  73   35    7    3  472 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             1                             12 
                          NA's 
                           577 

patient.follow_ups.follow_up.followup_treatment_success:
complete remission/response         progressive disease 
                          4                           2 
             stable disease                        NA's 
                          1                         583 

patient.follow_ups.follow_up.karnofsky_performance_score:
  40   60   80  100 NA's 
   2   20   49    7  512 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
  10    2  578 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.056   6.000  12.000      17 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
 yes NA's 
  91  499 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
  80   13  497 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   22.0   356.0   624.0   777.1   985.5  3378.0     495 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 879 NA's 
   1  589 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
both locoregional and distant metastasis 
                                       3 
                    locoregional disease 
                                      54 
                              metastatic 
                                      38 
                                    NA's 
                                     495 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
 yes NA's 
  94  496 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_pharmaceutical_therapy:
 yes NA's 
   1  589 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
 yes NA's 
   1  589 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_additional_surgery_procedure:
 670 NA's 
   1  589 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
metastatic       NA's 
         1        589 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   1  589 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.3.progression_determined_by:
imaging study          NA's 
            1           589 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
 yes NA's 
 316  274 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
 290   28  272 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  119.0   155.0   206.0   325.1   230.0  1181.0     583 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   27.0   293.8   441.0   530.4   646.5  3378.0     274 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
  locoregional disease             metastatic progression of disease 
                     6                      1                     34 
            recurrence                   NA's 
                   282                    267 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   7  583 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   8    1  581 

patient.follow_ups.follow_up.performance_status_scale_timing:
                other post-adjuvant therapy  pre-adjuvant therapy 
                   20                    46                    89 
        pre-operative                  NA's 
                   30                   405 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       143        362         85 

patient.follow_ups.follow_up.postoperative_rx_tx:
  no  yes NA's 
  29  526   35 

patient.follow_ups.follow_up.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                        323                          63 
        progressive disease              stable disease 
                         42                          31 
                       NA's 
                        131 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
 552    5   33 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
  280   293    17 

patient.follow_ups.follow_up.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2009    2009    2009    2009    2015      17 

patient.gender:
female   NA's 
   580     10 

patient.histological_type:
serous cystadenocarcinoma                      NA's 
                      580                        10 

patient.history_of_neoadjuvant_treatment:
  no  yes NA's 
 579    1   10 

patient.icd_10:
c48.0 c48.1 c48.2 c56.9  NA's 
    1     4     2   573    10 

patient.icd_o_3_histology:
8440/3 8441/3 8460/3   NA's 
     1    575      4     10 

patient.icd_o_3_site:
c48.0 c48.1 c48.2 c56.9  NA's 
    1     4     2   573    10 

patient.informed_consent_verified:
yes 
590 


patient.jewish_origin:
ashkenazi      NA's 
       24       566 

patient.karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  40.00   60.00   80.00   75.83   80.00  100.00     506 

patient.lymphatic_invasion:
  no  yes NA's 
  84  138  368 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.059   6.000  12.000      10 


patient.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
 yes NA's 
   4  586 

patient.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no NA's 
   3  587 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 145  245  358  794 NA's 
   1    1    1    1  586 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
recurrence       NA's 
         1        589 

patient.new_tumor_events.new_tumor_event.2.progression_determined_by:
imaging study          NA's 
            3           587 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
 yes NA's 
   4  586 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no NA's 
   3  587 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
 145  245  704  794 NA's 
   1    1    1    1  586 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
progression of disease             recurrence                   NA's 
                     2                      2                    586 

patient.new_tumor_events.new_tumor_event.progression_determined_by:
imaging study          NA's 
            3           587 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   5    3  582 


patient.performance_status_scale_timing:
                other post-adjuvant therapy  pre-adjuvant therapy 
                   22                    47                    93 
        pre-operative                  NA's 
                   32                   396 

patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       144        365         81 

patient.postoperative_rx_tx:
  no  yes NA's 
   1    9  580 

patient.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                          7                           1 
        progressive disease                        NA's 
                          1                         581 

patient.prior_dx:
                                              no 
                                               8 
yes, history of synchronous/bilateral malignancy 
                                               3 
                                            NA's 
                                             579 


patient.radiation_therapy:
  no NA's 
   9  581 

patient.radiations.radiation.2.anatomic_treatment_site:
   local recurrence primary tumor field                NA's 
                  1                   1                 588 

patient.radiations.radiation.2.bcr_radiation_barcode:
tcga-36-1577-r27882 tcga-61-2101-r28435                NA's 
                  1                   1                 588 

patient.radiations.radiation.2.bcr_radiation_uuid:
126c8f98-b503-4cff-ae77-232b4ee73e54 2e12c990-cd4b-46fc-a84c-c6f3478884e1 
                                   1                                    1 
                                NA's 
                                 588 

patient.radiations.radiation.2.day_of_form_completion:
   7   18 NA's 
   1    1  588 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
 156 1327 NA's 
   1    1  588 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
 111 1309 NA's 
   1    1  588 

patient.radiations.radiation.2.month_of_form_completion:
   8   12 NA's 
   1    1  588 

patient.radiations.radiation.2.numfractions:
   2   22 NA's 
   1    1  588 

patient.radiations.radiation.2.radiation_dosage:
 600 2250 NA's 
   1    1  588 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no NA's 
   2  588 

patient.radiations.radiation.2.radiation_type:
external beam          NA's 
            2           588 

patient.radiations.radiation.2.regimen_indication:
  adjuvant palliative       NA's 
         1          1        588 

patient.radiations.radiation.2.units:
 cgy NA's 
   2  588 

patient.radiations.radiation.2.year_of_form_completion:
2009 2010 NA's 
   1    1  588 

patient.radiations.radiation.anatomic_treatment_site:
 distant recurrence    local recurrence primary tumor field                NA's 
                 13                   9                   7                 561 



patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    4.00    9.00   10.36   13.00   29.00     557 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  156.0   575.2  1072.0  1052.8  1353.5  2331.0     570 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  111.0   545.2   987.5  1007.4  1313.8  2270.0     560 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   6.000   6.182   8.000  12.000     557 

patient.radiations.radiation.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    9.25   12.50  327.00   28.25 5000.00     574 

patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     20    2030    3500    3779    4750   14290     571 

patient.radiations.radiation.radiation_treatment_ongoing:
  no NA's 
  30  560 

patient.radiations.radiation.radiation_type:
external beam radioisotopes          NA's 
           26             1           563 


patient.radiations.radiation.regimen_indication_notes:
one time intraperitoneal  progression, recurrence    progression/pallative 
                       1                        2                        1 
                    NA's 
                     586 

patient.radiations.radiation.units:
 cgy NA's 
  19  571 

patient.radiations.radiation.year_of_form_completion:
2009 2010 NA's 
  27    6  557 

patient.residual_tumor:
  r0   r1   r2   rx NA's 
  15   31    5    3  536 



patient.surgeries.surgery.procedure_type:
surgical resection               NA's 
                10                580 

patient.tissue_prospective_collection_indicator:
  no  yes NA's 
  11    6  573 

patient.tissue_retrospective_collection_indicator:
  no  yes NA's 
   6   11  573 


patient.tumor_residual_disease:
                >20 mm                1-10 mm               11-20 mm 
                   105                    254                     36 
no macroscopic disease                   NA's 
                   118                     77 

patient.tumor_tissue_site:
         omentum            ovary peritoneum ovary             NA's 
               3              575                2               10 

patient.venous_invasion:
  no  yes NA's 
  73   90  427 

patient.vital_status:
alive  dead  NA's 
  279   298    13 

patient.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2009    2009    2009    2009    2014      10 

patient.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1992    2001    2004    2004    2007    2013      10 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason:
    case matched methylation control incomplete distribution of specimens 
                                   4                                    1 
                 other non-canonical primary tumor and recurrent tumor(s) 
                                  10                                   11 
                                NA's 
                                 564 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus    non-canonical 
             564               15               11 

patient.biospecimen_cqcf.consent_or_death_status:
consented  deceased      NA's 
      281       169       140 

patient.biospecimen_cqcf.country:
      nigeria united states          NA's 
            1             9           580 

patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     -8     515    1691    1907    2853    6272     237 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -5.000   0.000   0.000   7.053   5.500  54.000     571 

patient.biospecimen_cqcf.digital_image_submitted:
  no  yes NA's 
   8    3  579 

patient.biospecimen_cqcf.ethnicity:
not hispanic or latino                   NA's 
                     8                    582 

patient.biospecimen_cqcf.frozen_specimen_anatomic_site:
omentum   other   ovary    NA's 
     16       1     430     143 

patient.biospecimen_cqcf.histological_type:
serous cystadenocarcinoma                      NA's 
                      447                       143 

patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
                                              no 
                                             441 
yes, pharmaceutical treatment prior to resection 
                                               1 
               yes, radiation prior to resection 
                                               1 
                                            NA's 
                                             147 

patient.biospecimen_cqcf.method_of_sample_procurement:
        gross total resection other method (please specify) 
                            7                            16 
           subtotal resection                          NA's 
                            2                           565 

patient.biospecimen_cqcf.normal_controls.normal_control.2.bcr_sample_uuid:
97cda627-03b8-4e0e-8f96-c30deb4421f7                                 NA's 
                                   1                                  589 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1 NA's 
   1  589 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -15.00   -6.50   -5.00   36.50   -0.25  361.00     580 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
   0.0910    0.2500    0.2500   44.3860    0.7162 1131.9000       450 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   20.00   20.00   22.48   25.00   67.26     449 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    50.0   100.0   125.9   120.0   700.0     449 

patient.biospecimen_cqcf.normal_controls.normal_control.method_of_normal_sample_procurement:
blood draw       NA's 
        10        580 


patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
   1 NA's 
 424  166 


patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
 yes NA's 
  11  579 

patient.biospecimen_cqcf.path_confirm_report_attached:
  no  yes NA's 
  77  252  261 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
 yes NA's 
 409  181 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
  no  yes NA's 
   1  413  176 

patient.biospecimen_cqcf.race:
black or african american                     white                      NA's 
                       10                         3                       577 

patient.biospecimen_cqcf.sample_prescreened:
  no  yes NA's 
  35  411  144 

patient.biospecimen_cqcf.site_of_disease:
omentum   other   ovary    NA's 
     16       1     430     143 

patient.biospecimen_cqcf.site_of_disease_description:
other  NA's 
    1   589 

patient.biospecimen_cqcf.site_of_disease_text:
pelvic area        NA's 
          1         589 

patient.biospecimen_cqcf.submitted_for_lce:
  no NA's 
  10  580 

patient.biospecimen_cqcf.top_slide_submitted:
  no  yes NA's 
   2   12  576 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   5.000   8.041  20.000  90.000     191 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   80.00   80.00   82.92   90.00  100.00     180 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  200.0   300.0   300.0   433.5   500.0  1400.0     559 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
   1 NA's 
 448  142 

patient.biospecimen_cqcf.vessel_used:
cassette cryomold cryovial     NA's 
       2        2        6      580 

patient.days_to_index:
   0 NA's 
 580   10 

patient.samples.sample.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
primary tumor and recurrent tumor(s)                                 NA's 
                                  11                                  579 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0   120.5   320.5   496.9   804.2  1211.0     580 

patient.samples.sample.2.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.4000  0.5000  0.7000  0.7492  1.0000  1.8000     464 

patient.samples.sample.2.is_ffpe:
  no NA's 
 580   10 

patient.samples.sample.2.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.400   0.900   1.200   1.185   1.500   2.900     464 

patient.samples.sample.2.oct_embedded:
false  NA's 
   10   580 



patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio:
 1.8  1.9 NA's 
  11    1  578 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
76814654 76816255 76816305 85280962 97530686 98887142      571 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   3    8    9   10 NA's 
  12   65    6   26  481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
0.14 0.15 0.16  0.5 NA's 
   3    5    4   97  481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00   10.00   11.01   16.00   31.00     481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 109  481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    6.00    7.00    6.89    9.00   12.00     481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   5.101   9.000  12.000     481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  420.0   552.0   617.0   647.7   699.0  1140.0     481 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.34   30.00  200.00  134.59  200.00  200.00     481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
 109  481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
16.7   60  400 NA's 
  12   28   69  481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2009 2010 NA's 
  90   19  481 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
91290151 91290198 91290244 93665416 97545472 98886758      571 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7    9   10 NA's 
  12   91    6  481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.14 0.15 0.16  0.5 NA's 
   3    5    4   97  481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00   10.00   11.01   16.00   31.00     481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 109  481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    6.00    7.00    6.89    9.00   12.00     481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   5.202   9.000  12.000     481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  421.0   553.0   619.0   649.3   701.0  1141.0     481 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.86   30.00  200.00  134.53  200.00  200.00     481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22 NA's 
 109  481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
13.3   60  400 NA's 
  12   28   69  481 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2009 2010 NA's 
  90   19  481 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97515222  97515507  97530591  97763485  97530636 101188281       569 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.center_id:
   7    8    9   10 NA's 
  12    8    4    1  565 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.concentration:
0.14 0.15 0.16  0.5 NA's 
   3    5    4   13  565 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00    7.00   14.00   14.72   14.00   31.00     565 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  25  565 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    4.00    6.00    5.04    6.00    9.00     565 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    2.00    3.56    4.00   12.00     565 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  495.0   894.0  1042.0   918.5  1042.0  1231.0     565 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.50    0.50    2.00   33.52    2.13  200.00     565 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.source_center:
  22 NA's 
  25  565 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.volume:
   1 13.3   20  400 NA's 
   8   12    1    4  565 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2009 2010 NA's 
   4   21  565 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.050e+09 1.050e+09 8.601e+08 1.050e+09 1.050e+09       580 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.center_id:
   7   13 NA's 
   2    8  580 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.concentration:
0.14 0.15 0.16 NA's 
   2    4    4  580 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.day_of_shipment:
  16   26 NA's 
   8    2  580 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  10  580 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.month_of_shipment:
   3    7 NA's 
   8    2  580 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    3.00    4.70    7.75   11.00     580 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1151    1564    1564    1483    1567    1569     580 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.330   2.850   3.000   2.793   3.200   3.200     580 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.source_center:
  22 NA's 
  10  580 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.volume:
 9.5 13.3   20 NA's 
   1    1    8  580 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.5.year_of_shipment:
2010 2011 NA's 
   2    8  580 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-29-1770-02a-01r-1567-13                         NA's 
                           1                          589 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.bcr_aliquot_uuid:
ea119111-5217-4b8a-88e0-36dc77782241                                 NA's 
                                   1                                  589 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.biospecimen_barcode_bottom:
1049928319       NA's 
         1        589 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.center_id:
  13 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.concentration:
0.15 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.day_of_shipment:
  16 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.month_of_shipment:
   3 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.plate_column:
   3 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.plate_id:
1567 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.plate_row:
   h NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.quantity:
   3 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.source_center:
  22 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.volume:
  20 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.6.year_of_shipment:
2011 NA's 
   1  589 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
85092007 85092054 85092100 90075001 97545471 98864774      571 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   1    8    9   10 NA's 
  12   32  459   65   22 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0200  0.1500  0.2400  0.2426  0.2700  0.5000      22 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.000   6.000   6.961   6.000  31.000      22 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 568   22 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000  10.000  10.000   9.403  10.000  12.000      22 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   9.000   8.401  10.000  12.000      22 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    419    1306    1311    1185    1315    1318      22 


patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.80   12.45   15.57   40.48   30.00  200.00      22 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
 568   22 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0    60.0    79.4   140.0   173.0   400.0      22 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2009 2010 NA's 
  90  478   22 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   36.00   79.51  110.50 1020.00      22 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
genomeplex (rubicon) amplified dna               repli-g (qiagen) dna 
                               459                                 97 
                               rna                               NA's 
                                12                                 22 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
   g    r    w NA's 
 459   12   97   22 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0200  0.1500  0.2400  0.2426  0.2700  0.5000      22 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
 556   34 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no NA's 
 556   34 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             568 
                                            NA's 
                                              22 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 568   22 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
allprep rna extraction              genomplex                repli-g 
                    12                    459                     97 
                  NA's 
                    22 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.975   1.200   1.183   1.450   1.900     578 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.900   7.775   8.150   7.767   8.600   9.000     578 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
    568      22 

patient.samples.sample.2.portions.portion.analytes.analyte.3.a260_a280_ratio:
 1.8 NA's 
   4  586 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
91290104 97530024 97530050 96999233 97530361 97530736      543 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   3    8    9   10 NA's 
   4  186   42   41  317 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
0.15 0.16  0.5 NA's 
   2    2  269  317 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    8.00   12.00   16.62   31.00   31.00     317 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 273  317 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   5.157   7.000  12.000     317 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000   9.000   8.341  11.000  12.000     317 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  371.0   420.0   492.0   591.9   631.0  1092.0     317 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
2.51 2.67   30  200 NA's 
   2    2   62  207  317 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
 273  317 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
16.7   60  400 NA's 
   4   62  207  317 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2008 2009 2010 NA's 
  37  189   47  317 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
91289977 97545576 97545603 97012899 97545646 97545733      543 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.center_id:
   7    9   10 NA's 
   4  227   42  317 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.concentration:
0.15 0.16  0.5 NA's 
   2    2  269  317 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    8.00   12.00   16.62   31.00   31.00     317 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 273  317 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   5.157   7.000  12.000     317 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000   9.000   8.355  11.000  12.000     317 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  372.0   421.0   494.0   593.6   633.0  1094.0     317 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.quantity:
   2 2.13   30  200 NA's 
   2    2   62  207  317 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.source_center:
  22 NA's 
 273  317 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.volume:
13.3   60  400 NA's 
   4   62  207  317 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.3.year_of_shipment:
2008 2009 2010 NA's 
  37  189   47  317 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 9.649e+07 9.752e+07 1.502e+08 9.752e+07 1.050e+09       572 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.center_id:
   8    9   13 NA's 
   6   17    1  566 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.concentration:
0.16  0.5 NA's 
   1   23  566 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   13.75   28.00   22.12   28.75   31.00     566 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  24  566 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.month_of_shipment:
   3    4    7 NA's 
   7   14    3  566 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   3.625   4.000  11.000     566 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  489.0   794.2   936.0   878.7   977.0  1565.0     566 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.500   2.525  10.000  58.592  80.000 200.000     566 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.source_center:
  22 NA's 
  24  566 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.volume:
   1   20   80  400 NA's 
   6    9    3    6  566 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.4.year_of_shipment:
2009 2010 2011 NA's 
   6   17    1  566 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 76816135 100502670 100511831  99120672 100518446 100518942       283 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   1    7    8   10 NA's 
   4   12  331  186   57 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.1 0.15 0.16  0.5 NA's 
  12    2    2  517   57 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   3.000   9.086  12.000  31.000      57 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 533   57 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   9.000   6.884   9.000  12.000      57 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   9.000   8.497  11.000  12.000      57 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  370.0   490.0  1089.0   886.4  1215.0  1218.0      57 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   40.00   40.00   99.82  200.00  200.00      57 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
 533   57 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    80.0    80.0   199.9   400.0   400.0      57 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2008 2009 2010 NA's 
  37  189  307   57 

patient.samples.sample.2.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   128.0   155.0   191.1   215.6  1020.0      57 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                  rna            total rna 
                 517                    4                   12 
                NA's 
                  57 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id:
   r    t    w NA's 
   4   12  517   57 



patient.samples.sample.2.portions.portion.analytes.analyte.3.concentration:
 0.1 0.15 0.16  0.5 NA's 
  12    2    2  517   57 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
 517   73 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
 517   73 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             533 
                                            NA's 
                                              57 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 533   57 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
allprep rna extraction                repli-g              total rna 
                     4                    517                     12 
                  NA's 
                    57 

patient.samples.sample.2.portions.portion.analytes.analyte.3.rna.ratio_28s_18s:
 0.5    1  1.5 NA's 
   2    1    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.3.rna.rinvalue:
 6.8    7    8  8.1 NA's 
   1    1    1    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
    533      57 



patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.129e+07 9.129e+07 9.129e+07 1.143e+08 9.129e+07 1.053e+09       548 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.center_id:
   8    9   10 NA's 
 186    1    1  402 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
 188  402 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00   16.00   15.37   23.00   28.00     402 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 188  402 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   9.000   9.000   8.436  12.000  12.000     402 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000   9.000   8.144  11.000  12.000     402 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  617.0   637.0   699.0   705.9   723.0  1639.0     402 


patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.quantity:
   5   30  200 NA's 
   1  142   45  402 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.source_center:
  22 NA's 
 188  402 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.volume:
  10   60  400 NA's 
   1  142   45  402 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2009 2010 2011 NA's 
 146   41    1  402 



patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
91289974 91290278 91290303 91442874 91290328 97545714      549 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.center_id:
   9   10 NA's 
 186    1  403 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.concentration:
 0.5 NA's 
 187  403 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     2.0    16.0    15.3    23.0    27.0     403 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 187  403 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.month_of_shipment:
   1    7    9   12 NA's 
  40    1   72   74  403 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000   9.000   8.182  11.000  12.000     403 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  619.0   639.0   701.0   702.9   725.0  1094.0     403 


patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.quantity:
  30  200 NA's 
 142   45  403 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.source_center:
  22 NA's 
 187  403 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.volume:
  60  400 NA's 
 142   45  403 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.3.year_of_shipment:
2009 2010 NA's 
 146   41  403 



patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
97515237 97515508 97515550 97515517 97515559 97515582      560 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.center_id:
   8 NA's 
  30  560 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.concentration:
 0.5 NA's 
  30  560 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.day_of_shipment:
   7 NA's 
  30  560 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  30  560 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.month_of_shipment:
   4 NA's 
  30  560 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     8.0    10.0     8.6    11.0    12.0     560 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.plate_id:
 894  895 NA's 
  25    5  560 


patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.quantity:
 0.5 NA's 
  30  560 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.source_center:
  22 NA's 
  30  560 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.volume:
   1 NA's 
  30  560 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.4.year_of_shipment:
2010 NA's 
  30  560 



patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
91289235 91290007 91290575 92817387 91290618 97545488      537 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   7    8   10 NA's 
   4    1  197  388 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.1  0.5 NA's 
   4  198  388 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.00   16.00   15.48   23.00   27.00     388 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 202  388 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   9.000   8.203  12.000  12.000     388 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   9.000   7.753  11.000  12.000     388 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  455.0   635.0   698.0   711.7   797.0  1090.0     388 


patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
   1   10   30  200 NA's 
   4   11  142   45  388 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
 202  388 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  10   20   60  400 NA's 
   4   11  142   45  388 

patient.samples.sample.2.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2009 2010 NA's 
 149   53  388 

patient.samples.sample.2.portions.portion.analytes.analyte.4.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0     0.0   159.2    79.0  1020.0     388 

patient.samples.sample.2.portions.portion.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna            total rna                 NA's 
                 198                    4                  388 

patient.samples.sample.2.portions.portion.analytes.analyte.4.analyte_type_id:
   t    w NA's 
   4  198  388 



patient.samples.sample.2.portions.portion.analytes.analyte.4.concentration:
 0.1  0.5 NA's 
   4  198  388 

patient.samples.sample.2.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
 198  392 

patient.samples.sample.2.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
 198  392 

patient.samples.sample.2.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             202 
                                            NA's 
                                             388 

patient.samples.sample.2.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
 202  388 

patient.samples.sample.2.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
  repli-g total rna      NA's 
      198         4       388 

patient.samples.sample.2.portions.portion.analytes.analyte.4.spectrophotometer_method:
uv spec    NA's 
    202     388 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-04-1644-01b-01w-1091-09 tcga-09-2055-01b-01w-0799-08 
                           1                            1 
tcga-10-0925-01b-01w-0699-08 tcga-13-0901-01b-01w-0492-08 
                           1                            1 
                        NA's 
                         586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.bcr_aliquot_uuid:
2ecf7c25-b626-4635-ba33-a299ddf228a9 63c21c57-9539-4a17-84cc-e9efb303144d 
                                   1                                    1 
96caff1d-35bd-4457-a71e-87cde4f6357d a4bbe173-9e12-4a0d-89d6-25ecc56aad3b 
                                   1                                    1 
                                NA's 
                                 586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.biospecimen_barcode_bottom:
91291952 97530015     NA's 
       1        1      588 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.center_id:
   8    9 NA's 
   3    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.concentration:
 0.5 NA's 
   4  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.day_of_shipment:
   2   12   27   31 NA's 
   1    1    1    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   4  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.month_of_shipment:
   1    3    7   12 NA's 
   1    1    1    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_column:
   1    2    5 NA's 
   2    1    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_id:
 492  699  799 1091 NA's 
   1    1    1    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_row:
   b    c    f    g NA's 
   1    1    1    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.quantity:
  30  200 NA's 
   3    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.source_center:
  22 NA's 
   4  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.volume:
  60  400 NA's 
   3    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.2.year_of_shipment:
2009 2010 NA's 
   2    2  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-04-1644-01b-01w-1093-10 tcga-09-2055-01b-01w-0801-09 
                           1                            1 
tcga-10-0925-01b-01w-0701-09 tcga-13-0901-01b-01w-0494-09 
                           1                            1 
                        NA's 
                         586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.bcr_aliquot_uuid:
256f1555-0952-4670-882f-280c182dbda2 4ccc9539-039c-42d7-af06-0402dd25c404 
                                   1                                    1 
a66099b8-967f-474a-9ff8-ff1c9a75f0b9 ad973e90-48b5-4cf2-b856-d39142a9d448 
                                   1                                    1 
                                NA's 
                                 586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.biospecimen_barcode_bottom:
91290320 97545567     NA's 
       1        1      588 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.center_id:
   9   10 NA's 
   3    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.concentration:
 0.5 NA's 
   4  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.day_of_shipment:
   2   12   27   31 NA's 
   1    1    1    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   4  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.month_of_shipment:
   1    3    7   12 NA's 
   1    1    1    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.plate_column:
   1    2    5 NA's 
   2    1    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.plate_id:
 494  701  801 1093 NA's 
   1    1    1    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.plate_row:
   b    c    f    g NA's 
   1    1    1    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.quantity:
  30  200 NA's 
   3    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.source_center:
  22 NA's 
   4  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.volume:
  60  400 NA's 
   3    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.3.year_of_shipment:
2009 2010 NA's 
   2    2  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-10-0925-01b-01w-0894-08                         NA's 
                           1                          589 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.4.bcr_aliquot_uuid:
82db78f7-22e7-4830-9c5f-4bf695cf2c10                                 NA's 
                                   1                                  589 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.4.biospecimen_barcode_bottom:
97515566     NA's 
       1      589 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.4.center_id:
   8 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.4.concentration:
 0.5 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.4.day_of_shipment:
   7 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.4.month_of_shipment:
   4 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.4.plate_column:
   1 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.4.plate_id:
 894 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.4.plate_row:
   c NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.4.quantity:
 0.5 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.4.source_center:
  22 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.4.volume:
   1 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.4.year_of_shipment:
2010 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.bcr_aliquot_barcode:
tcga-04-1644-01b-01w-1089-08 tcga-09-2055-01b-01w-0797-10 
                           1                            1 
tcga-10-0925-01b-01w-0697-10 tcga-13-0901-01b-01w-0490-10 
                           1                            1 
                        NA's 
                         586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.bcr_aliquot_uuid:
4757ed90-2f16-4525-b114-138fdeb51296 47b21121-4237-4544-89a9-edcb85328318 
                                   1                                    1 
8085ed2c-22b6-4bc3-9484-903fa7ac51ff 99e1613f-8d49-4f45-934a-9b91a12507ec 
                                   1                                    1 
                                NA's 
                                 586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.biospecimen_barcode_bottom:
91290608 97530399     NA's 
       1        1      588 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.center_id:
   8   10 NA's 
   1    3  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.concentration:
 0.5 NA's 
   4  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.day_of_shipment:
   2   12   27   31 NA's 
   1    1    1    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   4  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.month_of_shipment:
   1    3    7   12 NA's 
   1    1    1    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_column:
   1    2    5 NA's 
   2    1    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_id:
 490  697  797 1089 NA's 
   1    1    1    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_row:
   b    c    f    g NA's 
   1    1    1    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.quantity:
  30  200 NA's 
   3    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.source_center:
  22 NA's 
   4  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.volume:
  60  400 NA's 
   3    1  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.aliquots.aliquot.year_of_shipment:
2009 2010 NA's 
   2    2  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.amount:
     0     79    100 150.42   NA's 
     1      1      1      1    586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.analyte_type:
repli-g (qiagen) dna                 NA's 
                   4                  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.analyte_type_id:
   w NA's 
   4  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.bcr_analyte_barcode:
tcga-04-1644-01b-01w tcga-09-2055-01b-01w tcga-10-0925-01b-01w 
                   1                    1                    1 
tcga-13-0901-01b-01w                 NA's 
                   1                  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.bcr_analyte_uuid:
47f18327-3c97-412f-82db-d843b27049ed 528f3469-856b-4a43-bd2f-cd68c811b57d 
                                   1                                    1 
5e2c0047-45ec-45cb-9077-6351ec99e284 8df15532-d037-48e8-9d6f-95a5670ebaa9 
                                   1                                    1 
                                NA's 
                                 586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.concentration:
 0.5 NA's 
   4  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.dna.normal_tumor_genotype_match:
 yes NA's 
   4  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.dna.pcr_amplification_successful:
  no NA's 
   4  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               4 
                                            NA's 
                                             586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.is_derived_from_ffpe:
  no NA's 
   4  586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      4     586 

patient.samples.sample.2.portions.portion.analytes.analyte.5.spectrophotometer_method:
uv spec    NA's 
      4     586 

patient.samples.sample.2.portions.portion.analytes.analyte.6.aliquots.aliquot.bcr_aliquot_barcode:
tcga-09-2055-01b-01w-1217-08 tcga-10-0925-01b-01w-1215-08 
                           1                            1 
                        NA's 
                         588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.aliquots.aliquot.bcr_aliquot_uuid:
076821ad-b94c-4dfc-8586-f71f30d4b4c0 07b168f4-fd8e-4cf8-a27c-bd984134adb8 
                                   1                                    1 
                                NA's 
                                 588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.aliquots.aliquot.biospecimen_barcode_bottom:
100513020 100518898      NA's 
        1         1       588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.aliquots.aliquot.center_id:
   8 NA's 
   2  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.aliquots.aliquot.concentration:
 0.5 NA's 
   2  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.aliquots.aliquot.day_of_shipment:
   1 NA's 
   2  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.aliquots.aliquot.month_of_shipment:
   9 NA's 
   2  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.aliquots.aliquot.plate_column:
   3    5 NA's 
   1    1  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.aliquots.aliquot.plate_id:
1215 1217 NA's 
   1    1  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.aliquots.aliquot.plate_row:
   e    g NA's 
   1    1  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.aliquots.aliquot.quantity:
  40 NA's 
   2  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.aliquots.aliquot.source_center:
  22 NA's 
   2  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.aliquots.aliquot.volume:
  80 NA's 
   2  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.aliquots.aliquot.year_of_shipment:
2010 NA's 
   2  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.amount:
 130  156 NA's 
   1    1  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.analyte_type:
repli-g (qiagen) dna                 NA's 
                   2                  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.analyte_type_id:
   w NA's 
   2  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.bcr_analyte_barcode:
tcga-09-2055-01b-01w tcga-10-0925-01b-01w                 NA's 
                   1                    1                  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.bcr_analyte_uuid:
47f18327-3c97-412f-82db-d843b27049ed 8df15532-d037-48e8-9d6f-95a5670ebaa9 
                                   1                                    1 
                                NA's 
                                 588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.concentration:
 0.5 NA's 
   2  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.dna.normal_tumor_genotype_match:
 yes NA's 
   2  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.dna.pcr_amplification_successful:
  no NA's 
   2  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.is_derived_from_ffpe:
  no NA's 
   2  588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      2     588 

patient.samples.sample.2.portions.portion.analytes.analyte.6.spectrophotometer_method:
uv spec    NA's 
      2     588 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.600   1.900   1.900   1.892   1.900   2.000      22 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 76816711  97530192  97530224 104309437  97530270 167780529       519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    4    6    9 NA's 
  15  488   65   10   12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.1400  0.1500  0.1509  0.1600  0.2100      12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0     9.0    15.0    16.2    23.0    28.0      12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 578   12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.491  10.000  12.000      12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   9.000   8.623  10.000  12.000      12 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   3.740   4.010   3.703   4.270   4.940      12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
 568   10   12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   26.70   26.70   24.67   26.70   40.00      12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2008 2009 2010 2014 NA's 
  73  434   61   10   12 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
85091434 97526545 97526575 95393868 97526614 98864647      529 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   4    6 NA's 
  80  488   22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1200  0.1400  0.1500  0.1522  0.1600  0.2100      22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    9.00   15.00   16.32   23.00   28.00      22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 568   22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.518  10.000  12.000      22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.00    9.00    8.65   10.00   12.00      22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  358.0   458.0   561.0   584.2   665.0  1136.0      22 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.600   1.860   2.000   2.292   2.240   4.540      22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
 568   22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
11.3 11.8 13.3 26.7 NA's 
   1   37  450   80   22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2008 2009 2010 NA's 
  73  434   61   22 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
85090970 97529519 97529549 95397896 97529575 98887571      529 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   2    6 NA's 
 553   15   22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1200  0.1400  0.1500  0.1522  0.1600  0.2100      22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    9.00   15.00   16.24   23.00   28.00      22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 568   22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.518  10.000  12.000      22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.00    9.00    8.65   10.00   12.00      22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  360.0   459.0   562.0   585.5   666.0  1137.0      22 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.500   2.800   3.000   2.985   3.200   3.700      22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
 568   22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  10 13.3 17.6   20 NA's 
   2   15   37  514   22 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2008 2009 2010 NA's 
  73  434   61   22 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
case matched methylation control                             NA's 
                               1                              589 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
8.509e+07 9.753e+07 9.753e+07 2.769e+08 1.539e+08 1.054e+09       372 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   3.777   8.000  10.000     298 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.050   0.130   0.150   0.133   0.160   0.210     298 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0     9.0     9.0    11.9    19.0    27.0     298 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
 292  298 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   5.308   6.000  12.000     298 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   8.000   7.233  10.000  12.000     298 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.500   0.570   1.752   3.000  22.660     298 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22   23 NA's 
 272   20  298 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    3.33    3.33   17.15   17.70  133.30     298 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2008    2010    2010    2010    2010    2013     298 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.751e+07 9.753e+07 9.753e+07 1.487e+08 1.539e+08 1.050e+09       490 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   8.000   5.945   8.000  12.000     444 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1500  0.1402  0.1600  0.2100     444 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    9.00   19.00   14.59   19.00   26.00     444 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
 146  444 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.000   6.000   7.884  12.000  12.000     444 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.219   9.000  12.000     444 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.500   1.388   0.940  15.000     444 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22   23 NA's 
 123   23  444 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    3.33   11.37   13.38  100.00     444 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2009 2010 2011 2013 NA's 
   1   73   49   23  444 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.753e+07 1.292e+08 1.539e+08 2.231e+08 1.539e+08 1.050e+09       568 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   1    8    9   10 NA's 
   3   20    2    8  557 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.05    0.07    0.14    0.12    0.17    0.19     557 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    9.00   19.00   16.85   26.00   26.00     557 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
  33  557 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   9.000   8.515  12.000  12.000     557 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    4.00    3.97    6.00   10.00     557 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.9400  0.8233  1.0000  5.0000     557 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  22   23 NA's 
  19   14  557 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000  13.400   9.413  13.400  33.300     557 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2010 2011 2013 2015 NA's 
   4   15   13    1  557 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.210e+08 1.457e+08 1.539e+08 2.594e+08 1.574e+08 1.050e+09       582 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
   1    9   10 NA's 
   2    1    5  582 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
0.05 0.07 0.08 0.15 NA's 
   1    4    1    2  582 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
   2    5    9   21 NA's 
   1    2    4    1  582 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   8  582 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   1    2    9   12 NA's 
   1    1    4    2  582 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   1    2    4 NA's 
   2    4    2  582 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id:
1525 2069 a324 a46a NA's 
   1    2    4    1  582 


patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.350   1.000   1.400   1.394   1.450   3.000     582 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  22   23 NA's 
   3    5  582 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
   5 6.67   20   60 NA's 
   1    2    4    1  582 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2011 2013 2015 NA's 
   3    4    1  582 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_barcode:
tcga-13-1489-02a-01d-2069-01                         NA's 
                           1                          589 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_uuid:
ea12bb70-f529-4ed7-bcd1-690045a5ef99                                 NA's 
                                   1                                  589 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom:
121031688      NA's 
        1       589 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.center_id:
   1 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.concentration:
0.15 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.day_of_shipment:
   5 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe:
  no NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.month_of_shipment:
  12 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_column:
   1 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_id:
2069 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_row:
   b NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.quantity:
   1 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.source_center:
  22 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.volume:
6.67 NA's 
   1  589 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.9.year_of_shipment:
2011 NA's 
   1  589 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 85090863  97529712  97529757 105591040  97529790 167780537       519 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1 NA's 
 578   12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1200  0.1400  0.1500  0.1613  0.1600  0.5000      12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0     9.0    15.0    16.2    23.0    28.0      12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 578   12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.491  10.000  12.000      12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   9.000   8.548  10.000  12.000      12 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.800   0.940   1.010   6.177   1.070 200.000      12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
 568   10   12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
 5.9  6.7  400 NA's 
  37  526   15   12 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2008 2009 2010 2014 NA's 
  73  434   61   10   12 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   10.65   87.07   87.97  910.97      12 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
 578   12 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
 578   12 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1200  0.1400  0.1500  0.1522  0.1600  0.2100      12 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
 578   12 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
  15  563   12 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             578 
                                            NA's 
                                              12 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
 578   12 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type     chemical lysis dna extraction 
                               44                               332 
pre-extracted dna received by tss                              NA's 
                              202                                12 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
       10       568        12 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   14.00   14.00   15.65   17.00   30.00      10 

patient.samples.sample.2.portions.portion.is_ffpe:
  no NA's 
 580   10 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000   7.000   6.712   7.000  12.000      10 

patient.samples.sample.2.portions.portion.portion_number:
   1    2 NA's 
 579    1   10 

patient.samples.sample.2.portions.portion.portion_sequence:
   1 NA's 
  10  580 




patient.samples.sample.2.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
  78  512 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   1 NA's 
   1  589 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   0 NA's 
   1  589 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   5.235  10.000  20.000     573 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   0 NA's 
   1  589 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_normal_cells:
   0    1 NA's 
  16    1  573 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   10.00   15.76   25.00   40.00     573 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  50.00   70.00   80.00   78.25   90.00   95.00     574 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  75.00   85.00   90.00   87.94   90.00   95.00     573 

patient.samples.sample.2.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
     1     77    512 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
 109  481 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   1 NA's 
   1  589 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   0 NA's 
   1  589 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   5.000   5.412  10.000  20.000     573 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0 NA's 
   1  589 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   0    1 NA's 
  16    1  573 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   10.00   13.53   20.00   30.00     573 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  50.00   74.75   80.00   79.81   90.00   94.00     574 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  70.00   85.00   90.00   87.35   90.00   95.00     573 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
bottom    top   NA's 
    77     32    481 

patient.samples.sample.2.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.0    97.5   113.0   110.4   134.5   174.0     459 

patient.samples.sample.2.portions.portion.year_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2008    2009    2011    2010    2011    2014      10 


patient.samples.sample.2.portions.shipment_portion.center_id:
  20 NA's 
   7  583 

patient.samples.sample.2.portions.shipment_portion.is_ffpe:
  no NA's 
   7  583 

patient.samples.sample.2.portions.shipment_portion.plate_id:
2189 NA's 
   7  583 

patient.samples.sample.2.portions.shipment_portion.portion_number:
  21   31 NA's 
   5    2  583 


patient.samples.sample.2.portions.shipment_portion.shipment_portion_day_of_shipment:
   5 NA's 
   7  583 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_month_of_shipment:
   3 NA's 
   7  583 

patient.samples.sample.2.portions.shipment_portion.shipment_portion_year_of_shipment:
2012 NA's 
   7  583 

patient.samples.sample.2.sample_type:
blood derived normal        primary tumor      recurrent tumor 
                 442                    5                   12 
 solid tissue normal                 NA's 
                 121                   10 

patient.samples.sample.2.sample_type_id:
   1    2   10   11 NA's 
   5   12  442  121   10 

patient.samples.sample.2.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.2000  0.3000  0.3103  0.4000  0.7000     464 

patient.samples.sample.2.vial_number:
   a    b    c    r NA's 
 554   20    5    1   10 



patient.samples.sample.3.intermediate_dimension:
 0.4  0.7  0.8 NA's 
   2    1    1  586 

patient.samples.sample.3.is_ffpe:
  no NA's 
  17  573 

patient.samples.sample.3.longest_dimension:
 0.8    1  1.1  1.5 NA's 
   1    1    1    1  586 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
97530695     NA's 
       1      589 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   8    9 NA's 
   4    1  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
 0.5 NA's 
   5  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   2   12   16 NA's 
   1    1    3  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   5  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   7    9   12 NA's 
   1    3    1  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     5.0     8.0     7.4    11.0    12.0     585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_id:
 631  632  699 1092 NA's 
   2    1    1    1  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_row:
   a    b    e    f NA's 
   2    1    1    1  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
  30  200 NA's 
   2    3  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22 NA's 
   5  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  60  400 NA's 
   2    3  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2009 2010 NA's 
   4    1  585 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
97545695     NA's 
       1      589 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   9   10 NA's 
   4    1  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
 0.5 NA's 
   5  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   2   12   16 NA's 
   1    1    3  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   5  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   7    9   12 NA's 
   1    3    1  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     5.0     8.0     7.4    11.0    12.0     585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
 633  634  701 1094 NA's 
   2    1    1    1  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   a    b    e    f NA's 
   2    1    1    1  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
  30  200 NA's 
   2    3  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22 NA's 
   5  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
  60  400 NA's 
   2    3  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2009 2010 NA's 
   4    1  585 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-10-0925-11a-01w-0894-08 tcga-29-1705-10a-01w-0927-10 
                           1                            1 
tcga-29-1710-10a-01w-0927-10 tcga-29-1770-10a-01w-0927-10 
                           1                            1 
                        NA's 
                         586 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_aliquot_uuid:
14bd9268-cfaf-4cba-82fd-d3f8caea6821 1a07af71-9c81-4aff-8fa1-8c45bf2e2935 
                                   1                                    1 
200309f4-12ac-404f-b7a3-92e807bf4186 690e2175-28d0-4644-876e-21d649546bf2 
                                   1                                    1 
                                NA's 
                                 586 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.biospecimen_barcode_bottom:
97512753 97512765 97512789 97515583     NA's 
       1        1        1        1      586 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.center_id:
   8   10 NA's 
   1    3  586 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.concentration:
 0.5 NA's 
   4  586 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
   7   19 NA's 
   1    3  586 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   4  586 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
   4 NA's 
   4  586 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   6    7    8 NA's 
   1    2    1  586 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_id:
 894  927 NA's 
   1    3  586 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_row:
   a    b    d    e NA's 
   1    1    1    1  586 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.quantity:
 0.5   10   11   13 NA's 
   1    1    1    1  586 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.source_center:
  22 NA's 
   4  586 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.volume:
   1   20   22   26 NA's 
   1    1    1    1  586 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2010 NA's 
   4  586 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
97545521     NA's 
       1      589 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   8    9   10 NA's 
   1   12    4  573 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.2000  0.2500  0.2906  0.5000  0.5000     573 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   2    6   12   16 NA's 
   1   12    1    3  573 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  17  573 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   7    9   10   12 NA's 
   1    3   12    1  573 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000  10.000   8.823  11.000  12.000     573 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    629    1090    1308    1142    1316    1318     573 


patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.65   13.28   19.18   50.23   30.00  200.00     573 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
  17  573 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   46.1    60.0    79.4   143.3   173.0   400.0     573 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2009 2010 NA's 
   4   13  573 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    1.00   41.00   56.89   94.90  126.90     573 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
genomeplex (rubicon) amplified dna               repli-g (qiagen) dna 
                                12                                  5 
                              NA's 
                               573 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   g    w NA's 
  12    5  573 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.2000  0.2500  0.2906  0.5000  0.5000     573 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
  17  573 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no NA's 
  17  573 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              17 
                                            NA's 
                                             573 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
  17  573 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
genomplex   repli-g      NA's 
       12         5       573 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
     17     573 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
91290857 97529667     NA's 
       1        1      588 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   8    9   10 NA's 
   7    1    1  581 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
   9  581 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   10.00   16.00   19.11   31.00   31.00     581 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   9  581 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   4.444   7.000   9.000     581 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  8.000   8.000  10.000   9.889  11.000  12.000     581 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  371.0   486.0   492.0   591.4   631.0  1091.0     581 


patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
  30  200 NA's 
   2    7  581 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
   9  581 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  60  400 NA's 
   2    7  581 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2008 2009 2010 NA's 
   1    6    2  581 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.biospecimen_barcode_bottom:
91289991 97545574     NA's 
       1        1      588 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.center_id:
   9   10 NA's 
   8    1  581 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.concentration:
 0.5 NA's 
   9  581 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   10.00   16.00   19.11   31.00   31.00     581 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   9  581 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   4.444   7.000   9.000     581 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  8.000   8.000  10.000   9.889  11.000  12.000     581 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  372.0   488.0   494.0   593.2   633.0  1093.0     581 


patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.quantity:
  30  200 NA's 
   2    7  581 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.source_center:
  22 NA's 
   9  581 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.volume:
  60  400 NA's 
   2    7  581 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.3.year_of_shipment:
2008 2009 2010 NA's 
   1    6    2  581 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
97512729 97512740 97512752 97512758 97512774 97512801      584 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.center_id:
  10 NA's 
   6  584 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.concentration:
 0.5 NA's 
   6  584 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.day_of_shipment:
  19 NA's 
   6  584 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   6  584 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.month_of_shipment:
   4 NA's 
   6  584 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.plate_column:
   5    6    7 NA's 
   2    3    1  584 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.plate_id:
 927 NA's 
   6  584 


patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   12.50   13.00   13.58   14.25   18.50     584 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.source_center:
  22 NA's 
   6  584 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  20.00   25.00   26.00   27.17   28.50   37.00     584 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.4.year_of_shipment:
2010 NA's 
   6  584 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 91290547  98276062 100515936  98481774 100518892 100518913       584 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8   10 NA's 
   6    7  577 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
  13  577 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00   10.00   13.54   27.00   31.00     577 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  13  577 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.000   5.846   9.000   9.000     577 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  8.000   8.000  10.000   9.769  11.000  12.000     577 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  370.0   484.0   629.0   782.5  1215.0  1217.0     577 


patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  30   40  200 NA's 
   2    4    7  577 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
  13  577 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  60   80  400 NA's 
   2    4    7  577 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2008 2009 2010 NA's 
   1    6    6  577 

patient.samples.sample.3.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    82.0   153.0   175.6   274.2   386.9     577 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                  13                  577 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id:
   w NA's 
  13  577 



patient.samples.sample.3.portions.portion.analytes.analyte.3.concentration:
 0.5 NA's 
  13  577 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
  13  577 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
  13  577 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              13 
                                            NA's 
                                             577 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
  13  577 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
     13     577 

patient.samples.sample.3.portions.portion.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
     13     577 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-09-2055-10a-01w-0799-08 tcga-61-2008-11a-01w-0722-08 
                           1                            1 
tcga-61-2095-11a-01w-0723-08                         NA's 
                           1                          587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.bcr_aliquot_uuid:
4df7e671-6a1f-4e16-b710-e030fbb819cf e223cf7e-fff7-4b09-bb3d-df9dd9c64da6 
                                   1                                    1 
ef06ee0a-f02a-4d61-a0bc-b6d85c7514ac                                 NA's 
                                   1                                  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.biospecimen_barcode_bottom:
91291895     NA's 
       1      589 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.center_id:
   8 NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
  16   27 NA's 
   2    1  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
   1   12 NA's 
   1    2  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   1    8   11 NA's 
   1    1    1  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_id:
 722  723  799 NA's 
   1    1    1  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_row:
   b    e    g NA's 
   1    1    1  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.quantity:
  30 NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.source_center:
  22 NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.volume:
  60 NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2009 2010 NA's 
   2    1  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-09-2055-10a-01w-0801-09 tcga-61-2008-11a-01w-0724-09 
                           1                            1 
tcga-61-2095-11a-01w-0725-09                         NA's 
                           1                          587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_uuid:
0e27b5b1-a252-4641-8c12-ad5ac65ef392 27c1957a-d92a-4ca4-849a-a9e83a758d96 
                                   1                                    1 
2ce213aa-4a41-4f41-8057-4679ad3c33ca                                 NA's 
                                   1                                  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.biospecimen_barcode_bottom:
91290263     NA's 
       1      589 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.center_id:
   9 NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.concentration:
 0.5 NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.day_of_shipment:
  16   27 NA's 
   2    1  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.month_of_shipment:
   1   12 NA's 
   1    2  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_column:
   1    8   11 NA's 
   1    1    1  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_id:
 724  725  801 NA's 
   1    1    1  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_row:
   b    e    g NA's 
   1    1    1  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.quantity:
  30 NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.source_center:
  22 NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.volume:
  60 NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.3.year_of_shipment:
2009 2010 NA's 
   2    1  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-61-2008-11a-01w-0927-10 tcga-61-2095-11a-01w-0927-10 
                           1                            1 
                        NA's 
                         588 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.4.bcr_aliquot_uuid:
62b7fcdc-e810-42cd-b1dc-ba85d169f984 8e492611-d1ff-4ff0-96c7-4cdabd486b56 
                                   1                                    1 
                                NA's 
                                 588 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.4.biospecimen_barcode_bottom:
97512128 97512799     NA's 
       1        1      588 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.4.center_id:
  10 NA's 
   2  588 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.4.concentration:
 0.5 NA's 
   2  588 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.4.day_of_shipment:
  19 NA's 
   2  588 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   2  588 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.4.month_of_shipment:
   4 NA's 
   2  588 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.4.plate_column:
   8 NA's 
   2  588 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.4.plate_id:
 927 NA's 
   2  588 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.4.plate_row:
   a    c NA's 
   1    1  588 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.4.quantity:
  10 NA's 
   2  588 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.4.source_center:
  22 NA's 
   2  588 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.4.volume:
  20 NA's 
   2  588 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.4.year_of_shipment:
2010 NA's 
   2  588 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_barcode:
tcga-09-2055-10a-01w-0797-10 tcga-61-2008-11a-01w-0720-10 
                           1                            1 
tcga-61-2095-11a-01w-0721-10                         NA's 
                           1                          587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_uuid:
10018095-2005-4d44-9db4-291741c3748c 2c363a7a-721f-44c2-95a4-39eddc20a762 
                                   1                                    1 
6718d1a7-b769-4145-88a6-838d75c7279d                                 NA's 
                                   1                                  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
91290551     NA's 
       1      589 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
  10 NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
  16   27 NA's 
   2    1  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   1   12 NA's 
   1    2  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   1    8   11 NA's 
   1    1    1  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id:
 720  721  797 NA's 
   1    1    1  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_row:
   b    e    g NA's 
   1    1    1  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
  30 NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  60 NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2009 2010 NA's 
   2    1  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.amount:
   0 NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                   3                  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.analyte_type_id:
   w NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.bcr_analyte_barcode:
tcga-09-2055-10a-01w tcga-61-2008-11a-01w tcga-61-2095-11a-01w 
                   1                    1                    1 
                NA's 
                 587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.bcr_analyte_uuid:
171c93ec-1f1a-4408-ab73-f0b952382609 d63131d2-c986-4783-b922-5e6f544d76aa 
                                   1                                    1 
f3641881-d801-4efc-ae76-53e340a7929b                                 NA's 
                                   1                                  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.concentration:
 0.5 NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               3 
                                            NA's 
                                             587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   3  587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      3     587 

patient.samples.sample.3.portions.portion.analytes.analyte.4.spectrophotometer_method:
uv spec    NA's 
      3     587 

patient.samples.sample.3.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.800   1.880   1.900   1.883   1.900   2.000     573 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
97530261 98864799     NA's 
       1        1      588 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    4    6 NA's 
   1   12    4  573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1506  0.1600  0.1800     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   14.00   18.00   18.59   26.00   28.00     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  17  573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   7.000   6.941  10.000  12.000     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000  10.000   8.647  10.000  11.000     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  357.0   457.0   560.0   608.8   664.0  1135.0     573 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   2.000   4.010   3.381   4.270   4.540     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
  17  573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
 6.7 13.3 23.5 26.7 NA's 
   1    4    1   11  573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2008 2009 2010 NA's 
   2   13    2  573 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
97526613 98863071     NA's 
       1        1      588 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   4    6 NA's 
   5   12  573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1506  0.1600  0.1800     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00   14.00   18.00   18.65   26.00   28.00     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  17  573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   7.000   6.941  10.000  12.000     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000  10.000   8.647  10.000  11.000     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  358.0   458.0   561.0   609.8   665.0  1136.0     573 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.73    2.00    2.13    2.54    3.47    4.01     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
  17  573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
11.8 13.3 26.7 NA's 
   1   11    5  573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2008 2009 2010 NA's 
   2   13    2  573 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
97529589 98887455     NA's 
       1        1      588 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   2    6 NA's 
  16    1  573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1506  0.1600  0.1800     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   14.00   18.00   18.59   26.00   28.00     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  17  573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   7.000   6.941  10.000  12.000     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000  10.000   8.647  10.000  11.000     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  360.0   459.0   562.0   610.9   666.0  1137.0     573 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.860   2.800   3.000   2.931   3.200   3.400     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
  17  573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
13.3 17.6   20 NA's 
   1    1   15  573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2008 2009 2010 NA's 
   2   13    2  573 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
97513003 97513025 97513047 97517396 97517085 97531159      578 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.250  12.000   8.643  12.000  12.000     576 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1521  0.1600  0.1700     576 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.00   19.00   19.00   18.14   19.00   23.00     576 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  14  576 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   4    6   10 NA's 
  10    3    1  576 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  5.000   6.000   6.500   6.786   7.000  11.000     576 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  403.0   926.0   926.0   883.3   926.0  1047.0     576 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.530   2.200   6.420   7.031  12.332  16.000     576 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
  14  576 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.33   14.40   47.50   46.23   75.12  100.00     576 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2008 2009 2010 NA's 
   1    1   12  576 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.751e+07 9.785e+07 5.744e+08 5.740e+08 1.050e+09 1.050e+09       584 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.500  10.000   8.857  11.000  12.000     583 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.05 0.14 0.17 0.18 NA's 
   3    2    1    1  583 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   2    9   19 NA's 
   3    1    3  583 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   7  583 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   2    4    6   12 NA's 
   3    2    1    1  583 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   1    2    5    6 NA's 
   2    2    2    1  583 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  926.0   983.5  1525.0  1367.9  1525.5  2106.0     583 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.285   3.000   3.754   6.485   7.740     583 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22 NA's 
   7  583 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   20.66   42.00   48.48   51.50  153.00     583 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2010 2011 NA's 
   3    4  583 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-13-0913-10a-01d-1036-01 tcga-13-1489-10a-01d-2082-10 
                           1                            1 
                        NA's 
                         588 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid:
9c33b8a7-2507-4f17-9354-01e555ed6a92 dccdd4c9-7c18-4f4e-9c6a-ca1d93491775 
                                   1                                    1 
                                NA's 
                                 588 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
  97531063 1061603901       NA's 
         1          1        588 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   1   10 NA's 
   1    1  588 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
0.14 NA's 
   2  588 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   9   12 NA's 
   1    1  588 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   2  588 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   6   12 NA's 
   1    1  588 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   1    8 NA's 
   1    1  588 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_id:
1036 2082 NA's 
   1    1  588 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_row:
   a    g NA's 
   1    1  588 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
0.47 0.94 NA's 
   1    1  588 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  22 NA's 
   2  588 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
3.33  6.7 NA's 
   1    1  588 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2010 2011 NA's 
   1    1  588 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
97529781 98864415     NA's 
       1        1      588 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1 NA's 
  17  573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1718  0.1600  0.5000     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   14.00   18.00   18.59   26.00   28.00     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  17  573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   7.000   6.941  10.000  12.000     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   7.000  10.000   8.588  10.000  11.000     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  356.0   452.0   559.0   607.6   663.0  1134.0     573 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.87    0.94    1.01   12.71    1.07  200.00     573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22 NA's 
  17  573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
 5.9  6.7  400 NA's 
   1   15    1  573 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2008 2009 2010 NA's 
   2   13    2  573 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     2.0     6.5    24.2    20.0   152.0     573 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
  17  573 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
  17  573 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1506  0.1600  0.1800     573 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
  17  573 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
  17  573 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              17 
                                            NA's 
                                             573 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
  17  573 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
    chemical lysis dna extraction pre-extracted dna received by tss 
                               10                                 7 
                             NA's 
                              573 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
     17     573 



patient.samples.sample.3.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   14.00   14.00   18.06   19.00   30.00     573 

patient.samples.sample.3.portions.portion.is_ffpe:
  no NA's 
  17  573 

patient.samples.sample.3.portions.portion.month_of_creation:
   7    8    9   11 NA's 
  11    3    1    2  573 

patient.samples.sample.3.portions.portion.portion_number:
   1 NA's 
  17  573 

patient.samples.sample.3.portions.portion.portion_sequence:
   1 NA's 
   1  589 

patient.samples.sample.3.portions.portion.slides.slide.2.bcr_slide_barcode:
tcga-04-1644-11a-01-ts1 tcga-61-2008-11a-01-ts1 tcga-61-2095-11a-01-ts1 
                      1                       1                       1 
tcga-61-2610-11a-01-ts1                    NA's 
                      1                     586 

patient.samples.sample.3.portions.portion.slides.slide.2.bcr_slide_uuid:
7d627c98-07a7-4238-aeb9-aa43b96a54bc 90174e79-36f4-4f1f-a1c2-357161dd09cb 
                                   1                                    1 
c12713fe-c9a2-47e0-a941-261f7536fbbc fd64789b-5cb2-4e23-8a07-558207cc70d4 
                                   1                                    1 
                                NA's 
                                 586 

patient.samples.sample.3.portions.portion.slides.slide.2.image_file_name:
tcga-04-1644-11a-01-ts1.c12713fe-c9a2-47e0-a941-261f7536fbbc.svs 
                                                               1 
tcga-61-2008-11a-01-ts1.7d627c98-07a7-4238-aeb9-aa43b96a54bc.svs 
                                                               1 
tcga-61-2095-11a-01-ts1.fd64789b-5cb2-4e23-8a07-558207cc70d4.svs 
                                                               1 
tcga-61-2610-11a-01-ts1.90174e79-36f4-4f1f-a1c2-357161dd09cb.svs 
                                                               1 
                                                            NA's 
                                                             586 

patient.samples.sample.3.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   4  586 

patient.samples.sample.3.portions.portion.slides.slide.2.section_location:
 top NA's 
   4  586 

patient.samples.sample.3.portions.portion.slides.slide.bcr_slide_barcode:
tcga-04-1644-11a-01-bs1 tcga-61-2008-11a-01-bs1 tcga-61-2095-11a-01-bs1 
                      1                       1                       1 
tcga-61-2610-11a-01-bs1                    NA's 
                      1                     586 

patient.samples.sample.3.portions.portion.slides.slide.bcr_slide_uuid:
33c7e0e1-7a2a-4964-835a-ad17c0061da1 49ff86b0-b714-4c97-8556-64f6e4e60128 
                                   1                                    1 
bf371520-600a-4644-8f73-792b4bbbefb9 e47fe9c6-94a0-43c5-bee2-d3d8844fd7b6 
                                   1                                    1 
                                NA's 
                                 586 

patient.samples.sample.3.portions.portion.slides.slide.image_file_name:
tcga-04-1644-11a-01-bs1.33c7e0e1-7a2a-4964-835a-ad17c0061da1.svs 
                                                               1 
tcga-61-2008-11a-01-bs1.49ff86b0-b714-4c97-8556-64f6e4e60128.svs 
                                                               1 
tcga-61-2095-11a-01-bs1.bf371520-600a-4644-8f73-792b4bbbefb9.svs 
                                                               1 
tcga-61-2610-11a-01-bs1.e47fe9c6-94a0-43c5-bee2-d3d8844fd7b6.svs 
                                                               1 
                                                            NA's 
                                                             586 

patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
   4  586 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
bottom   NA's 
     4    586 

patient.samples.sample.3.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1      20     120      84     133     146     585 

patient.samples.sample.3.portions.portion.year_of_creation:
2009 2011 2012 NA's 
   6   10    1  573 

patient.samples.sample.3.sample_type:
blood derived normal  solid tissue normal                 NA's 
                  12                    5                  573 

patient.samples.sample.3.sample_type_id:
  10   11 NA's 
  12    5  573 

patient.samples.sample.3.shortest_dimension:
 0.1  0.3  0.4 NA's 
   1    2    1  586 

patient.samples.sample.3.vial_number:
   a    b NA's 
  16    1  573 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   11.0   120.5   320.5   489.2   779.5  1211.0     580 


patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.2:
9b58474c-dac0-4d45-b13c-0a1ea9e1bc32                                 NA's 
                                   1                                  589 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    130     215     245     372     385    1040     580 

patient.samples.sample.intermediate_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.2000  0.6000  0.8000  0.8795  1.0000  3.0000      20 

patient.samples.sample.is_ffpe:
 no 
590 

patient.samples.sample.longest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.300   1.000   1.300   1.383   1.500   8.000      20 

patient.samples.sample.oct_embedded:
false  true  NA's 
    6     4   580 





patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.center_id:
   7    9 NA's 
   2    5  583 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0800  0.0850  0.1000  0.1214  0.1450  0.2100     583 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   6   27 NA's 
   5    2  583 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   7  583 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   1   10 NA's 
   2    5  583 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_column:
   1    5 NA's 
   5    2  583 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_id:
 438 1307 1308 NA's 
   2    3    2  583 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.plate_row:
   b    d    e    f NA's 
   2    2    1    2  583 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.42   13.84   14.29   20.80   28.77     583 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
   7  583 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.volume:
  10  137  173 NA's 
   2    2    3  583 

patient.samples.sample.portions.portion.2.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2009 2010 NA's 
   2    5  583 

patient.samples.sample.portions.portion.2.analytes.analyte.2.amount:
   0   15   36  119 NA's 
   3    1    2    1  583 

patient.samples.sample.portions.portion.2.analytes.analyte.2.analyte_type:
genomeplex (rubicon) amplified dna                          total rna 
                                 5                                  2 
                              NA's 
                               583 

patient.samples.sample.portions.portion.2.analytes.analyte.2.analyte_type_id:
   g    t NA's 
   5    2  583 



patient.samples.sample.portions.portion.2.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0800  0.0850  0.1000  0.1214  0.1450  0.2100     583 

patient.samples.sample.portions.portion.2.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   5  585 

patient.samples.sample.portions.portion.2.analytes.analyte.2.dna.pcr_amplification_successful:
  no NA's 
   5  585 

patient.samples.sample.portions.portion.2.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               7 
                                            NA's 
                                             583 

patient.samples.sample.portions.portion.2.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   7  583 

patient.samples.sample.portions.portion.2.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
genomplex total rna      NA's 
        5         2       583 

patient.samples.sample.portions.portion.2.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      7     583 

patient.samples.sample.portions.portion.2.analytes.analyte.3.a260_a280_ratio:
 1.8 NA's 
   2  588 



patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.center_id:
   3    8 NA's 
   2    3  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.concentration:
0.15  0.5 NA's 
   2    3  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
   6   22   31 NA's 
   1    2    2  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   5  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   1    3   10 NA's 
   1    2    2  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   1    4    5 NA's 
   3    1    1  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.plate_id:
 405  420  486 NA's 
   2    1    2  585 


patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.quantity:
2.51  200 NA's 
   2    3  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
   5  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.volume:
16.7  400 NA's 
   2    3  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2008 2009 NA's 
   2    3  585 



patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.center_id:
   7    9 NA's 
   2    3  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.concentration:
0.15  0.5 NA's 
   2    3  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.day_of_shipment:
   6   22   31 NA's 
   1    2    2  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   5  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.month_of_shipment:
   1    3   10 NA's 
   1    2    2  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.plate_column:
   1    4    5 NA's 
   3    1    1  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.plate_id:
 406  421  488 NA's 
   2    1    2  585 


patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.quantity:
   2  200 NA's 
   2    3  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.source_center:
  22 NA's 
   5  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.volume:
13.3  400 NA's 
   2    3  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.3.year_of_shipment:
2008 2009 NA's 
   2    3  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-10-0930-01a-02r-1564-13 tcga-10-0934-01a-02r-1564-13 
                           1                            1 
tcga-13-0807-01b-02w-1132-09                         NA's 
                           1                          587 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.bcr_aliquot_uuid:
5b759ce4-2632-4d85-89fe-30a85069e9a8 b34ed0d7-1130-4cc8-b19d-b56c1cd2cc2e 
                                   1                                    1 
faddd7d1-b48d-4d86-88fa-d97c2ceb103d                                 NA's 
                                   1                                  587 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.biospecimen_barcode_bottom:
  98864370 1049968602 1049968626       NA's 
         1          1          1        587 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.center_id:
   9   13 NA's 
   1    2  587 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.concentration:
0.15  0.5 NA's 
   2    1  587 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.day_of_shipment:
  16   21 NA's 
   2    1  587 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   3  587 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.month_of_shipment:
   3    7 NA's 
   2    1  587 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.plate_column:
   4    6 NA's 
   1    2  587 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.plate_id:
1132 1564 NA's 
   1    2  587 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.plate_row:
   a    b    c NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.quantity:
   3   40 NA's 
   2    1  587 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.source_center:
  22 NA's 
   3  587 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.volume:
  20   80 NA's 
   2    1  587 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.4.year_of_shipment:
2010 2011 NA's 
   1    2  587 



patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.center_id:
   1   10 NA's 
   2    3  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.concentration:
0.15  0.5 NA's 
   2    3  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   6   22   31 NA's 
   1    2    2  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   5  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   1    3   10 NA's 
   1    2    2  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_column:
   1    4    5 NA's 
   3    1    1  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.plate_id:
 404  419  484 NA's 
   2    1    2  585 


patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.quantity:
   3  200 NA's 
   2    3  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
   5  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.volume:
  20  400 NA's 
   2    3  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2008 2009 NA's 
   2    3  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  158.0   331.0   352.7   331.7   365.6   451.2     585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                   3                    2                  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.analyte_type_id:
   r    w NA's 
   2    3  585 



patient.samples.sample.portions.portion.2.analytes.analyte.3.concentration:
0.15  0.5 NA's 
   2    3  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   3  587 

patient.samples.sample.portions.portion.2.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
   3  587 

patient.samples.sample.portions.portion.2.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               5 
                                            NA's 
                                             585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   5  585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
allprep rna extraction                repli-g                   NA's 
                     2                      3                    585 

patient.samples.sample.portions.portion.2.analytes.analyte.3.rna.ratio_28s_18s:
 0.9  1.8 NA's 
   1    1  588 

patient.samples.sample.portions.portion.2.analytes.analyte.3.rna.rinvalue:
 7.9  9.2 NA's 
   1    1  588 

patient.samples.sample.portions.portion.2.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
      5     585 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-10-0930-01a-02w-0420-08 tcga-10-0934-01a-02w-0420-08 
                           1                            1 
                        NA's 
                         588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.bcr_aliquot_uuid:
d9e22b3a-7ae9-4410-8678-b1302c486e1f fa7a9c14-21aa-4462-808b-c8b502088098 
                                   1                                    1 
                                NA's 
                                 588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.center_id:
   8 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
   6 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
   1 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   5 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.plate_id:
 420 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.plate_row:
   b    e NA's 
   1    1  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.quantity:
 200 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.source_center:
  22 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.volume:
 400 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2009 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-10-0930-01a-02w-0421-09 tcga-10-0934-01a-02w-0421-09 
                           1                            1 
                        NA's 
                         588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.bcr_aliquot_uuid:
b8562fc7-88ec-4197-ac7c-0c7827755c28 ec98ed86-1d2f-4e54-b2d4-5976469bf0b8 
                                   1                                    1 
                                NA's 
                                 588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.center_id:
   9 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.concentration:
 0.5 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.day_of_shipment:
   6 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.month_of_shipment:
   1 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.plate_column:
   5 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.plate_id:
 421 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.plate_row:
   b    e NA's 
   1    1  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.quantity:
 200 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.source_center:
  22 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.volume:
 400 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.3.year_of_shipment:
2009 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-10-0930-01a-02w-0977-09                         NA's 
                           1                          589 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.bcr_aliquot_uuid:
42a97f19-f920-4186-873d-2d7967abed08                                 NA's 
                                   1                                  589 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.biospecimen_barcode_bottom:
96488833     NA's 
       1      589 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.center_id:
   9 NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.concentration:
 0.5 NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.day_of_shipment:
  28 NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.month_of_shipment:
   4 NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.plate_column:
   1 NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.plate_id:
 977 NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.plate_row:
   a NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.quantity:
  10 NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.source_center:
  22 NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.volume:
  20 NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.4.year_of_shipment:
2010 NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_barcode:
tcga-10-0930-01a-02w-0419-10 tcga-10-0934-01a-02w-0419-10 
                           1                            1 
                        NA's 
                         588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_uuid:
0fe90554-2041-4723-9811-70dc0a2925d8 e925be78-2505-4750-bc78-ec9c5bd5b380 
                                   1                                    1 
                                NA's 
                                 588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.center_id:
  10 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   6 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   1 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_column:
   5 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_id:
 419 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.plate_row:
   b    e NA's 
   1    1  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.quantity:
 200 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.volume:
 400 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2009 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.amount:
 234  309 NA's 
   1    1  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna                 NA's 
                   2                  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.analyte_type_id:
   w NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.bcr_analyte_barcode:
tcga-10-0930-01a-02w tcga-10-0934-01a-02w                 NA's 
                   1                    1                  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.bcr_analyte_uuid:
45fe516d-a1d0-49d4-894a-d471b3203100 c544515b-7e1c-4965-93ad-ca5ddd5596ec 
                                   1                                    1 
                                NA's 
                                 588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.concentration:
 0.5 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      2     588 

patient.samples.sample.portions.portion.2.analytes.analyte.4.spectrophotometer_method:
uv spec    NA's 
      2     588 

patient.samples.sample.portions.portion.2.analytes.analyte.a260_a280_ratio:
 1.8  1.9 NA's 
   5    3  582 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.center_id:
   1    3    4 NA's 
   1    3    4  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.concentration:
0.14 0.15 0.16 NA's 
   2    3    3  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  22   27 NA's 
   4    4  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   8  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   1   10 NA's 
   4    4  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_column:
   1    4    5 NA's 
   5    1    2  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  400.0   400.0   416.5   416.5   430.5   435.0     582 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.070   2.630   3.205   3.053   3.808   4.010     582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
   8  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.volume:
 6.7 16.7 26.7 NA's 
   1    3    4  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2008 2009 NA's 
   4    4  582 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.center_id:
   4    6    7 NA's 
   1    4    3  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.concentration:
0.14 0.15 0.16 NA's 
   2    3    3  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  22   27 NA's 
   4    4  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   8  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   1   10 NA's 
   4    4  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_column:
   1    4    5 NA's 
   5    1    2  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  401.0   401.0   417.5   417.5   431.5   436.0     582 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.quantity:
1.86    2 2.13 4.27 NA's 
   2    3    2    1  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
   8  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.volume:
13.3 26.7 NA's 
   7    1  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2008 2009 NA's 
   4    4  582 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.center_id:
   1    2    5    6 NA's 
   1    2    3    1  583 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.concentration:
0.14 0.15 0.16 NA's 
   2    2    3  583 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   3    9   22   27 NA's 
   1    1    3    2  583 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   7  583 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   1    2    3   10 NA's 
   2    1    1    3  583 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_column:
   1    3    4    5 NA's 
   4    1    1    1  583 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  402.0   402.0   430.0   424.7   436.0   465.0     583 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.860   1.930   2.130   2.464   3.100   3.200     583 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
   7  583 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.volume:
13.3   20 NA's 
   4    3  583 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2008 2009 NA's 
   3    4  583 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
97530637     NA's 
       1      589 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.center_id:
   1    2    5    7 NA's 
   1    4    1    1  583 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.concentration:
0.14 0.15 0.16 NA's 
   2    2    3  583 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   3   14   22   27 NA's 
   1    1    3    2  583 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   7  583 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   1    3    6   10 NA's 
   2    1    1    3  583 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_column:
   1    2    4    5 NA's 
   4    1    1    1  583 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  403.0   403.0   431.0   511.3   448.5  1042.0     583 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   2.465   2.800   2.733   3.100   3.200     583 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
   7  583 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.volume:
13.3   20 NA's 
   2    5  583 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2008 2009 2010 NA's 
   3    3    1  583 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.753e+07 9.753e+07 9.753e+07 2.880e+08 9.753e+07 1.050e+09       585 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.center_id:
   1    5   13 NA's 
   4    1    1  584 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.concentration:
0.14 0.15 0.16 NA's 
   2    2    2  584 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   9   16   27 NA's 
   4    1    1  584 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   6  584 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   1    3    6 NA's 
   1    1    4  584 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.plate_column:
   1    5    6   11 NA's 
   2    1    2    1  584 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.plate_id:
 432 1035 1036 1564 NA's 
   1    2    2    1  584 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.quantity:
0.47  0.5 2.13  3.2 NA's 
   2    2    1    1  584 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.source_center:
  22 NA's 
   6  584 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.volume:
3.33 13.3   20 NA's 
   4    1    1  584 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2009 2010 2011 NA's 
   1    4    1  584 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-10-0934-01a-02d-2105-08 tcga-13-0807-01b-02d-1169-09 
                           1                            1 
                        NA's 
                         588 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.bcr_aliquot_uuid:
0c358da0-03dd-4f69-b49d-5a96b0a6209b a3bcf1b6-58b9-417a-9ac7-248878abc63f 
                                   1                                    1 
                                NA's 
                                 588 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
100507886      NA's 
        1       589 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.center_id:
   8    9 NA's 
   1    1  588 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.concentration:
0.14 0.15 NA's 
   1    1  588 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
  11   19 NA's 
   1    1  588 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   8   12 NA's 
   1    1  588 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.plate_column:
   1   10 NA's 
   1    1  588 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.plate_id:
1169 2105 NA's 
   1    1  588 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.plate_row:
   a    c NA's 
   1    1  588 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.quantity:
   0  0.5 NA's 
   1    1  588 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.source_center:
  22 NA's 
   2  588 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.volume:
   0 3.33 NA's 
   1    1  588 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2010 2011 NA's 
   1    1  588 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_barcode:
tcga-10-0934-01a-02d-a312-08                         NA's 
                           1                          589 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_uuid:
0664753b-7566-41e0-8006-7009c6735406                                 NA's 
                                   1                                  589 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
153878958      NA's 
        1       589 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.center_id:
   8 NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.concentration:
0.07 NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
  26 NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   6 NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.plate_column:
   7 NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.plate_id:
a312 NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.plate_row:
   b NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.quantity:
0.94 NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.source_center:
  23 NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.volume:
13.4 NA's 
   1  589 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2013 NA's 
   1  589 



patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.center_id:
   1 NA's 
   8  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.concentration:
0.14 0.15 0.16  0.5 NA's 
   2    3    2    1  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.day_of_shipment:
  22   27 NA's 
   4    4  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   8  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.month_of_shipment:
   1   10 NA's 
   4    4  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_column:
   1    4    5 NA's 
   5    1    2  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_id:
 399  404  428  434 NA's 
   3    1    2    2  582 


patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.quantity:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
  0.9400   0.9925   2.0050  26.6625   3.2000 200.0000      582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.source_center:
  22 NA's 
   8  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.volume:
 6.7   20  400 NA's 
   4    3    1  582 

patient.samples.sample.portions.portion.2.analytes.analyte.aliquots.aliquot.year_of_shipment:
2008 2009 NA's 
   4    4  582 

patient.samples.sample.portions.portion.2.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   5.003  32.500  91.880  60.943 521.000     582 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type:
 dna  rna NA's 
   5    3  582 

patient.samples.sample.portions.portion.2.analytes.analyte.analyte_type_id:
   d    r NA's 
   5    3  582 



patient.samples.sample.portions.portion.2.analytes.analyte.concentration:
0.14 0.15 0.16 NA's 
   2    3    3  582 

patient.samples.sample.portions.portion.2.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   5  585 

patient.samples.sample.portions.portion.2.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   5  585 

patient.samples.sample.portions.portion.2.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               8 
                                            NA's 
                                             582 

patient.samples.sample.portions.portion.2.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   8  582 

patient.samples.sample.portions.portion.2.analytes.analyte.protocols.protocol.experimental_protocol_type:
 adna preparation type allprep rna extraction                   NA's 
                     5                      3                    582 

patient.samples.sample.portions.portion.2.analytes.analyte.rna.ratio_28s_18s:
 1.1  1.2  2.1 NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.2.analytes.analyte.rna.rinvalue:
 7.1  7.5  8.3 NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.2.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      8     582 



patient.samples.sample.portions.portion.2.day_of_creation:
   4 NA's 
   8  582 

patient.samples.sample.portions.portion.2.is_ffpe:
  no NA's 
   8  582 

patient.samples.sample.portions.portion.2.month_of_creation:
   2 NA's 
   8  582 

patient.samples.sample.portions.portion.2.portion_number:
   2    3 NA's 
   7    1  582 




patient.samples.sample.portions.portion.2.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   7  583 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_lymphocyte_infiltration:
 100 NA's 
   1  589 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_necrosis:
   0    1    5 NA's 
   5    1    1  583 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     2.5     5.0    10.0    15.0    30.0     583 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   5.000   5.000   8.571  12.500  20.000     583 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  69.00   72.50   75.00   80.57   90.00   95.00     583 

patient.samples.sample.portions.portion.2.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  70.00   77.50   80.00   83.57   92.50   95.00     583 

patient.samples.sample.portions.portion.2.slides.slide.2.section_location:
bottom   NA's 
     7    583 




patient.samples.sample.portions.portion.2.slides.slide.is_derived_from_ffpe:
  no NA's 
   7  583 

patient.samples.sample.portions.portion.2.slides.slide.percent_lymphocyte_infiltration:
 100 NA's 
   1  589 

patient.samples.sample.portions.portion.2.slides.slide.percent_necrosis:
   0    1   10   20 NA's 
   4    1    1    1  583 

patient.samples.sample.portions.portion.2.slides.slide.percent_normal_cells:
   0    5   35 NA's 
   2    4    1  583 

patient.samples.sample.portions.portion.2.slides.slide.percent_stromal_cells:
   0    5   10   20 NA's 
   1    3    2    1  583 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  64.00   70.00   85.00   79.86   87.50   95.00     583 

patient.samples.sample.portions.portion.2.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   75.0    80.0    80.0    84.0    87.5    98.0     583 

patient.samples.sample.portions.portion.2.slides.slide.section_location:
 top NA's 
   7  583 

patient.samples.sample.portions.portion.2.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   69.0    91.5   114.0   108.3   126.8   138.0     584 

patient.samples.sample.portions.portion.2.year_of_creation:
2009 NA's 
   8  582 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-10-0935-01a-03g-1307-09 tcga-20-0996-01a-03g-1308-09 
                           1                            1 
tcga-23-1023-01a-03g-1308-09                         NA's 
                           1                          587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
089f09d8-3d1a-477e-9108-2a0aac5c65f5 80bb3730-d734-4b35-a54e-86f2cf347aa4 
                                   1                                    1 
b8fe830a-61fe-46ad-abd1-3f8397aa0857                                 NA's 
                                   1                                  587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.center_id:
   9 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.concentration:
0.09 0.21 0.23 NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   6 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
  10 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.plate_column:
   1    5 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.plate_id:
1307 1308 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.plate_row:
   e    f    g NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.quantity:
15.57 28.77 31.51  NA's 
    1     1     1   587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.source_center:
  22 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.volume:
 137  173 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.amount:
   0   36 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.analyte_type:
genomeplex (rubicon) amplified dna                               NA's 
                                 3                                587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.analyte_type_id:
   g NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.bcr_analyte_barcode:
tcga-10-0935-01a-03g tcga-20-0996-01a-03g tcga-23-1023-01a-03g 
                   1                    1                    1 
                NA's 
                 587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.bcr_analyte_uuid:
4ab480c3-cdb4-46ec-ab5a-a9ff207a3e96 69354c5f-4844-48dc-97a7-6b0828428ae5 
                                   1                                    1 
ab8e7561-8b12-451c-9d4f-0a9d43345d54                                 NA's 
                                   1                                  587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.concentration:
0.09 0.21 0.23 NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.dna.pcr_amplification_successful:
  no NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               3 
                                            NA's 
                                             587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
genomplex      NA's 
        3       587 

patient.samples.sample.portions.portion.3.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
      3     587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-10-0935-01a-03w-0420-08 tcga-20-0996-01a-03w-0486-08 
                           1                            1 
tcga-23-1023-01a-03w-0486-08                         NA's 
                           1                          587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_uuid:
0b5aee8b-a651-4f15-a260-27f08222add4 4f1a6979-b3a7-424d-8721-6d6a7696555a 
                                   1                                    1 
6d69c5a0-6dea-44f7-b6ca-9a8a3a3beec1                                 NA's 
                                   1                                  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.center_id:
   8 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
   6   31 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   1    3 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   1    5 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.plate_id:
 420  486 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.plate_row:
   d    f NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.quantity:
 200 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.volume:
 400 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2009 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-10-0935-01a-03w-0421-09 tcga-20-0996-01a-03w-0488-09 
                           1                            1 
tcga-23-1023-01a-03w-0488-09                         NA's 
                           1                          587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.bcr_aliquot_uuid:
8064455c-93ba-413c-9e3d-c7dcb0e4b36a 93229418-651f-487c-a7f6-b87a0b4eb841 
                                   1                                    1 
af0edbf4-9d90-4373-a9ce-0875ebbe1d04                                 NA's 
                                   1                                  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.center_id:
   9 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.concentration:
 0.5 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.day_of_shipment:
   6   31 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.month_of_shipment:
   1    3 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.plate_column:
   1    5 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.plate_id:
 421  488 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.plate_row:
   d    f NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.quantity:
 200 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.source_center:
  22 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.volume:
 400 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.3.year_of_shipment:
2009 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-10-0935-01a-03w-0977-09 tcga-23-1023-01a-03w-0927-10 
                           1                            1 
                        NA's 
                         588 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.bcr_aliquot_uuid:
7c797cfe-3ccd-4201-bf02-2837f94782f0 ab9ce32f-8ed9-462e-8a86-feb7275f3ad2 
                                   1                                    1 
                                NA's 
                                 588 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.biospecimen_barcode_bottom:
96488334 97512788     NA's 
       1        1      588 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.center_id:
   9   10 NA's 
   1    1  588 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.concentration:
 0.5 NA's 
   2  588 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.day_of_shipment:
  19   28 NA's 
   1    1  588 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   2  588 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.month_of_shipment:
   4 NA's 
   2  588 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.plate_column:
   1    6 NA's 
   1    1  588 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.plate_id:
 927  977 NA's 
   1    1  588 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.plate_row:
   b    c NA's 
   1    1  588 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.quantity:
  10 19.5 NA's 
   1    1  588 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.source_center:
  22 NA's 
   2  588 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.volume:
  20   39 NA's 
   1    1  588 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.4.year_of_shipment:
2010 NA's 
   2  588 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_barcode:
tcga-10-0935-01a-03w-0419-10 tcga-20-0996-01a-03w-0484-10 
                           1                            1 
tcga-23-1023-01a-03w-0484-10                         NA's 
                           1                          587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.bcr_aliquot_uuid:
1da8c6bd-59a5-4a23-b2ed-7a4577bfd0ca b9b4e9cc-57b1-46fa-91e6-521dde4ac892 
                                   1                                    1 
da5cda18-994e-4bee-a5c2-cc07a3268946                                 NA's 
                                   1                                  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.center_id:
  10 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   6   31 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   1    3 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.plate_column:
   1    5 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.plate_id:
 419  484 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.plate_row:
   d    f NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.quantity:
 200 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.volume:
 400 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2009 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.amount:
   349 374.44 401.78   NA's 
     1      1      1    587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.analyte_type:
repli-g (qiagen) dna                 NA's 
                   3                  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.analyte_type_id:
   w NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.bcr_analyte_barcode:
tcga-10-0935-01a-03w tcga-20-0996-01a-03w tcga-23-1023-01a-03w 
                   1                    1                    1 
                NA's 
                 587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.bcr_analyte_uuid:
958d20f2-5eb0-4857-8cdb-2089754c1933 e3dd3b26-68ba-4c7e-9a86-b53e407a27a2 
                                   1                                    1 
ffdd3185-a1ae-4c08-a78c-04f62238f057                                 NA's 
                                   1                                  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.concentration:
 0.5 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.dna.pcr_amplification_successful:
  no NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               3 
                                            NA's 
                                             587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      3     587 

patient.samples.sample.portions.portion.3.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
      3     587 

patient.samples.sample.portions.portion.3.analytes.analyte.a260_a280_ratio:
 1.9 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-10-0935-01a-03d-0400-04 tcga-20-0996-01a-03d-0429-04 
                           1                            1 
tcga-23-1023-01a-03d-0429-04                         NA's 
                           1                          587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
0913d89a-15fc-494a-8cbb-0e5784f35e3c 6a5a94f5-5b61-4737-8c9b-33554bcb71f6 
                                   1                                    1 
fcd38bdc-02c3-4db4-a2b9-67eaa5ce9686                                 NA's 
                                   1                                  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.center_id:
   4 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.concentration:
0.16 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  22   27 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   1   10 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.plate_column:
   1    5 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.plate_id:
 400  429 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.plate_row:
   b    d    f NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.quantity:
4.27 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.source_center:
  22 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.volume:
26.7 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2008 2009 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-10-0935-01a-03d-0401-06 tcga-20-0996-01a-03d-0430-06 
                           1                            1 
tcga-23-1023-01a-03d-0430-06                         NA's 
                           1                          587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
17e49bff-9168-4d14-b3a9-58b6668838b6 bcf209ff-9fd6-4152-b857-36b70258de7b 
                                   1                                    1 
cba7e2b0-7f7f-43a6-9792-632bb30a3c88                                 NA's 
                                   1                                  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.center_id:
   6 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.concentration:
0.16 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  22   27 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   1   10 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.plate_column:
   1    5 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.plate_id:
 401  430 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.plate_row:
   b    d    f NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.quantity:
2.13 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.source_center:
  22 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.volume:
13.3 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2008 2009 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-10-0935-01a-03d-0402-05 tcga-20-0996-01a-03d-0431-02 
                           1                            1 
tcga-23-1023-01a-03d-0431-02                         NA's 
                           1                          587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
49ea2eda-cbba-4fc8-8024-14988cef8a75 65ca3b0a-17d7-4375-8450-6fdb75a41b4a 
                                   1                                    1 
9c709361-faba-4bfc-ad22-14db407140dc                                 NA's 
                                   1                                  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.center_id:
   2    5 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.concentration:
0.16 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  22   27 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   1   10 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.plate_column:
   1    5 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.plate_id:
 402  431 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.plate_row:
   b    d    f NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.quantity:
2.13  3.2 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.volume:
13.3   20 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2008 2009 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-10-0935-01a-03d-0403-02 tcga-20-0996-01a-03d-0432-05 
                           1                            1 
tcga-23-1023-01a-03d-0432-05                         NA's 
                           1                          587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
8ac38c32-32a3-40da-a9ac-b8f65368ff61 ce0c7f4d-d347-46bb-a7fb-36e5f5f83798 
                                   1                                    1 
f6dd7c52-fd4d-4390-8f1e-6c068111c655                                 NA's 
                                   1                                  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.center_id:
   2    5 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.concentration:
0.16 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  22   27 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   1   10 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.plate_column:
   1    5 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.plate_id:
 403  432 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.plate_row:
   b    d    f NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.quantity:
2.13  3.2 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.volume:
13.3   20 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2008 2009 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-23-1023-01a-03d-0926-12                         NA's 
                           1                          589 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
0043e9a0-a999-44a0-a28f-b5459af41e26                                 NA's 
                                   1                                  589 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
97513069     NA's 
       1      589 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.center_id:
  12 NA's 
   1  589 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.concentration:
0.16 NA's 
   1  589 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
  19 NA's 
   1  589 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   1  589 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   4 NA's 
   1  589 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.plate_column:
   2 NA's 
   1  589 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.plate_id:
 926 NA's 
   1  589 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.plate_row:
   c NA's 
   1  589 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.quantity:
 0.4 NA's 
   1  589 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.source_center:
  22 NA's 
   1  589 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.volume:
2.47 NA's 
   1  589 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2010 NA's 
   1  589 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-10-0935-01a-03d-0399-01 tcga-20-0996-01a-03d-0428-01 
                           1                            1 
tcga-23-1023-01a-03d-0428-01                         NA's 
                           1                          587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
9eacb9f9-40d8-4ff0-803c-c93b169e7d9b a3a9276a-e5be-4e59-8a9c-ffea435ccdc8 
                                   1                                    1 
abd8131a-d16a-4a5f-9bca-2f0f1fede65d                                 NA's 
                                   1                                  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.center_id:
   1 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.concentration:
0.16 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.day_of_shipment:
  22   27 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.month_of_shipment:
   1   10 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.plate_column:
   1    5 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.plate_id:
 399  428 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.plate_row:
   b    d    f NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.quantity:
1.07 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.source_center:
  22 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.volume:
 6.7 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.aliquots.aliquot.year_of_shipment:
2008 2009 NA's 
   1    2  587 

patient.samples.sample.portions.portion.3.analytes.analyte.amount:
   0   10   50 NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.3.analytes.analyte.analyte_type:
 dna NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.analyte_type_id:
   d NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.bcr_analyte_barcode:
tcga-10-0935-01a-03d tcga-20-0996-01a-03d tcga-23-1023-01a-03d 
                   1                    1                    1 
                NA's 
                 587 

patient.samples.sample.portions.portion.3.analytes.analyte.bcr_analyte_uuid:
b28fa492-d3be-49f7-95c1-f5876b7ecb16 b92592de-087c-4c83-93aa-5d1bbbaed736 
                                   1                                    1 
fa0ccafe-9c08-4381-a43c-ecf6323d8854                                 NA's 
                                   1                                  587 

patient.samples.sample.portions.portion.3.analytes.analyte.concentration:
0.16 NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               3 
                                            NA's 
                                             587 

patient.samples.sample.portions.portion.3.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   3  587 

patient.samples.sample.portions.portion.3.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    3                   587 

patient.samples.sample.portions.portion.3.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      3     587 

patient.samples.sample.portions.portion.3.bcr_portion_barcode:
tcga-10-0935-01a-03 tcga-20-0996-01a-03 tcga-23-1023-01a-03                NA's 
                  1                   1                   1                 587 

patient.samples.sample.portions.portion.3.bcr_portion_uuid:
14a7dd3c-a5d2-43d1-8c37-04ccf5390ef7 2f03e8a7-0870-48f6-8d0d-404a09eff444 
                                   1                                    1 
438c1543-a2cd-4dd1-b5fd-73d98bf1d9a9                                 NA's 
                                   1                                  587 

patient.samples.sample.portions.portion.3.day_of_creation:
  17   30 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.is_ffpe:
  no NA's 
   3  587 

patient.samples.sample.portions.portion.3.month_of_creation:
   3   11 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.portion_number:
   3 NA's 
   3  587 

patient.samples.sample.portions.portion.3.slides.slide.bcr_slide_barcode:
tcga-10-0935-01a-01-ts1 tcga-20-0996-01a-01-bs1 tcga-23-1023-01a-01-bs1 
                      1                       1                       1 
                   NA's 
                    587 

patient.samples.sample.portions.portion.3.slides.slide.bcr_slide_uuid:
4c722f9c-5c74-4fa3-a5db-83777e788280 bb161bcf-6c16-46b3-bf16-45f5d64f550f 
                                   1                                    1 
f2260f08-0126-4fe9-bbb0-09e4fc2fb30e                                 NA's 
                                   1                                  587 

patient.samples.sample.portions.portion.3.slides.slide.image_file_name:
tcga-10-0935-01a-01-ts1.4c722f9c-5c74-4fa3-a5db-83777e788280.svs 
                                                               1 
tcga-20-0996-01a-01-bs1.bb161bcf-6c16-46b3-bf16-45f5d64f550f.svs 
                                                               1 
tcga-23-1023-01a-01-bs1.f2260f08-0126-4fe9-bbb0-09e4fc2fb30e.svs 
                                                               1 
                                                            NA's 
                                                             587 

patient.samples.sample.portions.portion.3.slides.slide.is_derived_from_ffpe:
  no NA's 
   3  587 

patient.samples.sample.portions.portion.3.slides.slide.percent_necrosis:
   0    1   20 NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.3.slides.slide.percent_normal_cells:
   5   35 NA's 
   2    1  587 

patient.samples.sample.portions.portion.3.slides.slide.percent_stromal_cells:
   0    5   10 NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.3.slides.slide.percent_tumor_cells:
  64   70   85 NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.3.slides.slide.percent_tumor_nuclei:
  75   80   85 NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.3.slides.slide.section_location:
 top NA's 
   3  587 

patient.samples.sample.portions.portion.3.weight:
 280 NA's 
   1  589 

patient.samples.sample.portions.portion.3.year_of_creation:
2009 2012 NA's 
   2    1  587 

patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.600   1.800   1.800   1.804   1.800   1.930     495 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
97542335 97543077 97543118 97927046 98551106 98887119      576 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   3   13 NA's 
  75   20  495 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1500  0.1532  0.1600  0.1800     495 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   12.00   15.00   14.66   18.00   28.00     495 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  95  495 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   5.000   5.358   7.000  12.000     495 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.547   6.000   6.000     495 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   2.340   2.510   2.309   2.670   3.200     495 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  22   23 NA's 
  85   10  495 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
   0 16.7   20 NA's 
  10   75   10  495 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2009 2010 2014 NA's 
  71   14   10  495 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
98886710 98886711 98886734 98886735     NA's 
       1        1        1        1      586 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7 NA's 
  75  515 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1512  0.1600  0.1700     515 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   12.00   15.00   13.51   15.00   28.00     515 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  75  515 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   4.000   6.000   5.453   7.000  12.000     515 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   4.013   6.000   6.000     515 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  456.0   504.0   540.0   563.8   566.0  1141.0     515 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.730   1.860   2.000   2.013   2.130   2.260     515 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  22 NA's 
  75  515 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
13.3 NA's 
  75  515 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2009 2010 NA's 
  71    4  515 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.753e+07 1.050e+09 1.050e+09 1.019e+09 1.050e+09 1.050e+09       528 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.center_id:
   7    9   13 NA's 
   8    1   60  521 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1400  0.1500  0.1516  0.1600  0.1700     521 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
  13   14   16 NA's 
   7    2   60  521 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  69  521 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
   3    4    5    6 NA's 
  60    1    7    1  521 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.652   8.000  12.000     521 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    514    1565    1566    1442    1566    1569     521 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.860   2.800   3.000   3.023   3.200  10.010     521 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.source_center:
  22 NA's 
  69  521 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.80   20.00   20.00   19.94   20.00   66.70     521 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2009 2010 2011 NA's 
   7    2   60  521 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
1.005e+08 1.050e+09 1.050e+09 9.313e+08 1.050e+09 1.050e+09       582 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.center_id:
   3   13 NA's 
   1    8  581 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.concentration:
0.13 0.14 0.15 0.17 NA's 
   1    3    3    2  581 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.day_of_shipment:
  16   19 NA's 
   8    1  581 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   9  581 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.month_of_shipment:
   3    5 NA's 
   8    1  581 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   3.000   3.111   4.000   5.000     581 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.plate_id:
 527 1566 1567 1569 NA's 
   1    6    1    1  581 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    2.6     2.8     3.0     3.2     3.4     5.0     581 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.source_center:
  22 NA's 
   9  581 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.volume:
  20 33.3 NA's 
   8    1  581 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.year_of_shipment:
2009 2011 NA's 
   1    8  581 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-13-1411-01a-01r-1565-13                         NA's 
                           1                          589 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.bcr_aliquot_uuid:
0069c63d-f699-41d4-9798-9a109d1c09df                                 NA's 
                                   1                                  589 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.biospecimen_barcode_bottom:
1049928684       NA's 
         1        589 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.center_id:
  13 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.concentration:
0.15 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.day_of_shipment:
  16 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.month_of_shipment:
   3 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.plate_column:
   5 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.plate_id:
1565 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.plate_row:
   g NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.quantity:
   3 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.source_center:
  22 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.volume:
  20 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.6.year_of_shipment:
2011 NA's 
   1  589 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97543887  97544823  98864726 127042705 167811972 167812259       566 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   1    7    9   31 NA's 
  75   14  479   10   12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0200  0.1400  0.1900  0.1821  0.2400  0.2800      12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   6.000   6.000   7.561   6.000  28.000      12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 578   12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   10.00   10.00    9.19   10.00   12.00      12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   3.561   5.000   7.000      12 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   11.04   13.25   16.22   20.76   48.44      12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  22   23 NA's 
 568   10   12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   47.08   78.10   95.58  173.00  173.00      12 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2008 2009 2010 2014 NA's 
   1   74  493   10   12 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.00   93.60   95.78  117.80  824.70      12 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
genomeplex (rubicon) amplified dna                                rna 
                               479                                 95 
                         total rna                               NA's 
                                 4                                 12 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
   g    r    t NA's 
 479   95    4   12 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0200  0.1400  0.1900  0.1821  0.2400  0.2800      12 

patient.samples.sample.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
 479  111 

patient.samples.sample.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
  no NA's 
 479  111 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             578 
                                            NA's 
                                              12 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
 578   12 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
   allprep rna extraction                 genomplex mirvana (allprep dna) rna 
                       85                       479                        10 
                total rna                      NA's 
                        4                        12 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.200   1.400   1.387   1.600   2.200     495 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  6.800   8.000   8.700   8.565   9.150  10.000     495 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
    578      12 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2 NA's 
   8    2  580 

patient.samples.sample.portions.portion.analytes.analyte.3.a260_a280_ratio:
 1.7  1.8  1.9 NA's 
  12  464    3  111 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
97529642 97530108 97530130 97530069 97530155 97530180      546 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   1    3    7 NA's 
  38  441    2  109 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.1500  0.1500  0.1523  0.1600  0.1900     109 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    9.00   18.00   16.86   25.00   28.00     109 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 481  109 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.000   6.834  10.000  12.000     109 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.214   4.000   7.000     109 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  362.0   454.0   565.0   586.6   669.0  1049.0     109 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.510   2.510   2.557   2.670   3.340     109 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  22 NA's 
 481  109 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  10 16.7 17.6 27.3 NA's 
   2  441   37    1  109 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2008 2009 2010 NA's 
  71  366   44  109 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
97528939 97529142 97529170 97529141 97529194 97529220      546 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.center_id:
   7 NA's 
 479  111 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1100  0.1500  0.1500  0.1525  0.1600  0.1900     111 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    9.00   18.00   16.86   25.00   28.00     111 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 479  111 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.000   6.825  10.000  12.000     111 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.211   4.000   6.000     111 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  363.0   456.0   566.0   587.5   670.0  1050.0     111 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.600   1.945   2.000   2.011   2.130   2.260     111 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.source_center:
  22 NA's 
 479  111 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.volume:
11.8 13.3 18.2 NA's 
  37  441    1  111 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.3.year_of_shipment:
2008 2009 2010 NA's 
  33  402   44  111 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 1.050e+09 1.050e+09 9.839e+08 1.050e+09 1.050e+09       244 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   13.00   13.00   11.11   13.00   13.00     177 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.110   0.150   0.150   0.153   0.160   0.190     177 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   16.00   16.00   14.56   16.00   28.00     177 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 413  177 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   3.000   3.041   3.000  10.000     177 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   5.729   8.000  12.000     177 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    441    1564    1566    1374    1568    1569     177 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.86    3.00    3.00    3.26    3.20   36.00     177 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.source_center:
  22 NA's 
 413  177 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.30   20.00   20.00   21.21   20.00  200.00     177 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.4.year_of_shipment:
2009 2010 2011 NA's 
  67   11  335  177 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 1.050e+09 1.050e+09 9.601e+08 1.050e+09 1.050e+09       537 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.25   13.00    9.96   13.00   13.00     516 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1581  0.1600  0.1900     516 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   16.00   16.00   13.64   16.00   26.00     516 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
  74  516 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   3.000   3.216   3.000   7.000     516 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   5.716   9.000  12.000     516 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    449     530    1564    1252    1564    1569     516 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    3.00    3.20    3.98    3.40   36.00     516 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.source_center:
  22 NA's 
  74  516 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.30   20.00   20.00   24.82   20.00  200.00     516 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.5.year_of_shipment:
2009 2010 2011 NA's 
  21    1   52  516 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.649e+07 1.050e+09 1.050e+09 9.235e+08 1.050e+09 1.050e+09       575 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.center_id:
   3    9   13 NA's 
   3    1   15  571 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.15    0.15    0.16    0.16    0.16    0.19     571 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.day_of_shipment:
  13   16   19   24 NA's 
   1   14    3    1  571 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
  19  571 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.month_of_shipment:
   3    5    8 NA's 
  14    4    1  571 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   6.000   5.947  11.000  12.000     571 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    515    1564    1564    1368    1565    1986     571 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   3.000   3.200   3.833   3.600  10.010     571 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.source_center:
  22 NA's 
  19  571 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.volume:
  20 27.8 31.3 66.7 NA's 
  15    1    2    1  571 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.6.year_of_shipment:
2009 2011 NA's 
   4   15  571 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.bcr_aliquot_barcode:
tcga-13-0720-01a-01r-1564-13 tcga-13-0725-01a-01r-1564-13 
                           1                            1 
tcga-24-0982-01a-01r-1565-13 tcga-24-1103-01a-01r-0527-03 
                           1                            1 
                        NA's 
                         586 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.bcr_aliquot_uuid:
0d474214-ec15-485d-abd3-cf476a14571c 1fe43034-df2b-4bef-be77-07dbb68083a4 
                                   1                                    1 
5551b356-0f0f-4e21-a2f5-cf539b5a490b ae158b16-5d93-42d5-a793-3294629af24d 
                                   1                                    1 
                                NA's 
                                 586 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.biospecimen_barcode_bottom:
1049928609 1049968621 1049968692       NA's 
         1          1          1        587 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.center_id:
   3   13 NA's 
   1    3  586 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.concentration:
0.15 0.16 0.18 NA's 
   1    2    1  586 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.day_of_shipment:
  16   19 NA's 
   3    1  586 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
   4  586 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.month_of_shipment:
   3    5 NA's 
   3    1  586 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.plate_column:
   1    2 NA's 
   2    2  586 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.plate_id:
 527 1564 1565 NA's 
   1    2    1  586 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.plate_row:
   a    c    e    h NA's 
   1    1    1    1  586 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.quantity:
 3.2  3.6    5 NA's 
   2    1    1  586 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.source_center:
  22 NA's 
   4  586 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.volume:
  20 33.3 NA's 
   3    1  586 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.7.year_of_shipment:
2009 2011 NA's 
   1    3  586 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.bcr_aliquot_barcode:
tcga-13-0725-01a-01r-1986-13 tcga-24-1103-01a-01r-1565-13 
                           1                            1 
                        NA's 
                         588 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.bcr_aliquot_uuid:
202abe19-20e0-4ea9-8653-bf5145da351c e9ac39c6-6c51-459b-8467-7b422e2ed704 
                                   1                                    1 
                                NA's 
                                 588 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.biospecimen_barcode_bottom:
 107043313 1049928630       NA's 
         1          1        588 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.center_id:
  13 NA's 
   2  588 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.concentration:
0.15 0.18 NA's 
   1    1  588 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.day_of_shipment:
  16   24 NA's 
   1    1  588 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   2  588 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.month_of_shipment:
   3    8 NA's 
   1    1  588 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.plate_column:
   1    2 NA's 
   1    1  588 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.plate_id:
1565 1986 NA's 
   1    1  588 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.plate_row:
   b NA's 
   2  588 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.quantity:
   3  3.6 NA's 
   1    1  588 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.source_center:
  22 NA's 
   2  588 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.volume:
  20 NA's 
   2  588 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.8.year_of_shipment:
2011 NA's 
   2  588 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
97529623 97529919 97529943 97643072 97529968 98887791      542 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   1    3    7 NA's 
 441   38   75   36 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.1400  0.1500  0.1454  0.1600  0.1900      36 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    9.50   15.00   16.41   23.00   28.00      36 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 554   36 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.639  10.000  12.000      36 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.319   5.000   6.000      36 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  361.0   453.0   564.0   582.6   668.0  1142.0      36 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.800   3.000   2.711   3.000   3.400      36 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  22 NA's 
 554   36 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  10 14.7   20 22.7 NA's 
  75   37  441    1   36 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2008 2009 2010 NA's 
  71  435   48   36 

patient.samples.sample.portions.portion.analytes.analyte.3.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   190.2   333.5   343.7   483.5  1109.0      36 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
      rna total rna      NA's 
      479        75        36 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
   r    t NA's 
 479   75   36 



patient.samples.sample.portions.portion.analytes.analyte.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1000  0.1400  0.1500  0.1454  0.1600  0.1900      36 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             554 
                                            NA's 
                                              36 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 554   36 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
allprep rna extraction              total rna                   NA's 
                   479                     75                     36 

patient.samples.sample.portions.portion.analytes.analyte.3.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.100   1.300   1.315   1.500   3.500     111 

patient.samples.sample.portions.portion.analytes.analyte.3.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    7.70    8.40    8.22    8.80    9.70     111 

patient.samples.sample.portions.portion.analytes.analyte.3.spectrophotometer_method:
uv spec    NA's 
    554      36 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
91291909 96507611 96507738 96414009 97530685 97530713      578 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.000   8.000   8.000   8.416  10.000  13.000     489 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.1  0.5 NA's 
  26   75  489 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    9.00   10.00   13.08   18.00   31.00     489 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 101  489 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   9.000   7.416   9.000  12.000     489 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.545   7.000  12.000     489 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  492.0   552.0   631.0   647.5   695.0  1092.0     489 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     5.0   200.0   120.3   200.0   200.0     489 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.source_center:
  22 NA's 
 101  489 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    50.0   400.0   243.8   400.0   400.0     489 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2009 2010 NA's 
  87   14  489 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
91290277 96507536 97545641 96355433 97545660 97545720      583 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.center_id:
   7    9   10   13 NA's 
   2   71    4    4  509 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.concentration:
 0.1  0.5 NA's 
   6   75  509 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    5.00   10.00   12.73   16.00   31.00     509 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  81  509 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   6.000   6.778   9.000  12.000     509 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   6.000   4.704   7.000  11.000     509 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  494.0   553.0   619.0   630.5   639.0  1094.0     509 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    30.0   200.0   149.7   200.0   200.0     509 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.source_center:
  22 NA's 
  81  509 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   10.0    60.0   400.0   301.6   400.0   400.0     509 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.3.year_of_shipment:
2009 2010 NA's 
  70   11  509 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-29-1770-01a-01w-0927-10 tcga-61-1734-01b-01t-0841-07 
                           1                            1 
tcga-61-1919-01a-01w-0894-08                         NA's 
                           1                          587 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.bcr_aliquot_uuid:
83374099-2981-47c0-aaf6-f34338b5d01f 850b9c9f-9341-4c75-81ed-36d117d0751e 
                                   1                                    1 
becba34c-18a7-4043-8117-7becb728a706                                 NA's 
                                   1                                  587 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.biospecimen_barcode_bottom:
96507692 97512776 97515525     NA's 
       1        1        1      587 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.center_id:
   7    8   10 NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.concentration:
 0.1  0.5 NA's 
   1    2  587 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.day_of_shipment:
   3    7   19 NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   3  587 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.month_of_shipment:
   3    4 NA's 
   1    2  587 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.plate_column:
   7   11 NA's 
   2    1  587 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.plate_id:
 841  894  927 NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.plate_row:
   c    e    f NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.quantity:
 0.5    1   10 NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.source_center:
  22 NA's 
   3  587 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.volume:
   1   10   20 NA's 
   1    1    1  587 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.4.year_of_shipment:
2010 NA's 
   3  587 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
91290565 97530490 97530514 97404309 97530540 97545542      541 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   7    8   10 NA's 
 479   30   45   36 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.1  0.5 NA's 
 479   75   36 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   10.00   14.00   16.39   23.00   31.00      36 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 554   36 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   7.000   6.874  10.000  12.000      36 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.417   5.000   7.000      36 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  364.0   455.0   567.0   591.8   671.0  1090.0      36 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
   1   30  200 NA's 
 479   17   58   36 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  22 NA's 
 554   36 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  10   60  400 NA's 
 479   17   58   36 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2008 2009 2010 NA's 
  33  472   49   36 

patient.samples.sample.portions.portion.analytes.analyte.4.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    44.0    77.5   114.1   127.0  1111.0      36 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type:
repli-g (qiagen) dna            total rna                 NA's 
                  75                  479                   36 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type_id:
   t    w NA's 
 479   75   36 



patient.samples.sample.portions.portion.analytes.analyte.4.concentration:
 0.1  0.5 NA's 
 479   75   36 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
  75  515 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
  no NA's 
  75  515 

patient.samples.sample.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             554 
                                            NA's 
                                              36 

patient.samples.sample.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
 554   36 

patient.samples.sample.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
  repli-g total rna      NA's 
       75       479        36 

patient.samples.sample.portions.portion.analytes.analyte.4.spectrophotometer_method:
uv spec    NA's 
    554      36 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
91290871 91291928 91291972 94375877 97530041 97530084      501 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.center_id:
   8    9   10 NA's 
 356   44   42  148 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.concentration:
 0.5 NA's 
 442  148 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    8.00   16.00   15.88   27.00   31.00     148 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 442  148 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    7.00    6.41    9.00   12.00     148 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   3.713   5.000   8.000     148 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  371.0   486.0   631.0   636.7   722.0  1091.0     148 


patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.quantity:
  30  200 NA's 
 196  246  148 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.source_center:
  22 NA's 
 442  148 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.volume:
  60  400 NA's 
 196  246  148 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.2.year_of_shipment:
2008 2009 2010 NA's 
  38  315   89  148 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
91289963 91290292 91290339 94382776 97545593 97545637      501 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.center_id:
   9   10 NA's 
 398   44  148 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.concentration:
 0.5 NA's 
 442  148 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    8.00   16.00   15.88   27.00   31.00     148 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 442  148 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    7.00    6.41    9.00   12.00     148 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   3.713   5.000   8.000     148 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  372.0   488.0   633.0   638.6   724.0  1093.0     148 


patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0    30.0   200.0   124.5   200.0   200.0     148 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.source_center:
  22 NA's 
 442  148 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   40.0    60.0   400.0   249.1   400.0   400.0     148 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.3.year_of_shipment:
2008 2009 2010 NA's 
  38  315   89  148 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
96488287 97512781 97515538 97381953 97515563 97515589      536 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.center_id:
   8    9   10 NA's 
  38    7    9  536 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.concentration:
 0.5 NA's 
  54  536 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.day_of_shipment:
   7   19   28 NA's 
  38    9    7  536 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
  54  536 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.month_of_shipment:
   4 NA's 
  54  536 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   3.963   6.000   8.000     536 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.plate_id:
 894  927  977 NA's 
  38    9    7  536 


patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.quantity:
 0.5    3   10 NA's 
  38    1   15  536 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.source_center:
  22 NA's 
  54  536 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.volume:
   1    6   20 NA's 
  38    1   15  536 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.4.year_of_shipment:
2010 NA's 
  54  536 



patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 91288260  91290619  97530467  97389000 100502992 100518906       415 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.center_id:
   8   10 NA's 
 172  356   62 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.concentration:
 0.5 NA's 
 528   62 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00   12.00   13.45   23.00   31.00      62 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 528   62 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.000   6.831   9.000  12.000      62 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   3.633   5.000   8.000      62 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  370.0   490.0   635.0   729.1   797.0  1217.0      62 


patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.quantity:
  30   40  200 NA's 
 196   86  246   62 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.source_center:
  22 NA's 
 528   62 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.volume:
  60   80  400 NA's 
 196   86  246   62 

patient.samples.sample.portions.portion.analytes.analyte.5.aliquots.aliquot.year_of_shipment:
2008 2009 2010 NA's 
  38  315  175   62 

patient.samples.sample.portions.portion.analytes.analyte.5.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0    96.0   174.1   243.8  1020.0      61 

patient.samples.sample.portions.portion.analytes.analyte.5.analyte_type:
repli-g (qiagen) dna                 NA's 
                 529                   61 

patient.samples.sample.portions.portion.analytes.analyte.5.analyte_type_id:
   w NA's 
 529   61 



patient.samples.sample.portions.portion.analytes.analyte.5.concentration:
 0.5 NA's 
 529   61 

patient.samples.sample.portions.portion.analytes.analyte.5.dna.normal_tumor_genotype_match:
 yes NA's 
 529   61 

patient.samples.sample.portions.portion.analytes.analyte.5.dna.pcr_amplification_successful:
  no NA's 
 529   61 

patient.samples.sample.portions.portion.analytes.analyte.5.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             529 
                                            NA's 
                                              61 

patient.samples.sample.portions.portion.analytes.analyte.5.is_derived_from_ffpe:
  no NA's 
 529   61 

patient.samples.sample.portions.portion.analytes.analyte.5.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
    529      61 

patient.samples.sample.portions.portion.analytes.analyte.5.spectrophotometer_method:
uv spec    NA's 
    529      61 



patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.2.center_id:
   8 NA's 
  37  553 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.2.concentration:
 0.5 NA's 
  37  553 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.2.day_of_shipment:
   2   16   23 NA's 
  16   10   11  553 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  37  553 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.2.month_of_shipment:
   9   12 NA's 
  27   10  553 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    4.00    3.73    5.00    7.00     553 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  617.0   631.0   637.0   649.6   699.0   722.0     553 


patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.2.quantity:
  30  200 NA's 
  21   16  553 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.2.source_center:
  22 NA's 
  37  553 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.2.volume:
  60  400 NA's 
  21   16  553 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.2.year_of_shipment:
2009 NA's 
  37  553 



patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.3.center_id:
   9 NA's 
  37  553 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.3.concentration:
 0.5 NA's 
  37  553 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.3.day_of_shipment:
   2   16   23 NA's 
  16   10   11  553 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  37  553 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.3.month_of_shipment:
   9   12 NA's 
  27   10  553 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    4.00    3.73    5.00    7.00     553 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.3.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  619.0   633.0   639.0   651.6   701.0   724.0     553 


patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.3.quantity:
  30  200 NA's 
  21   16  553 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.3.source_center:
  22 NA's 
  37  553 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.3.volume:
  60  400 NA's 
  21   16  553 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.3.year_of_shipment:
2009 NA's 
  37  553 



patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.4.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
97515516 97515521 97515536 97515541 97515563 97515570      584 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.4.center_id:
   8 NA's 
   6  584 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.4.concentration:
 0.5 NA's 
   6  584 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.4.day_of_shipment:
   7 NA's 
   6  584 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   6  584 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.4.month_of_shipment:
   4 NA's 
   6  584 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    2.25    3.50    4.00    5.50    7.00     584 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.4.plate_id:
 894 NA's 
   6  584 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.4.plate_row:
   b    d    f    g NA's 
   2    1    2    1  584 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.4.quantity:
 0.5 NA's 
   6  584 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.4.source_center:
  22 NA's 
   6  584 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.4.volume:
   1 NA's 
   6  584 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.4.year_of_shipment:
2010 NA's 
   6  584 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-30-1862-01a-02w-1215-08                         NA's 
                           1                          589 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.5.bcr_aliquot_uuid:
9b8efaad-1d68-470b-b0d2-ac247853545a                                 NA's 
                                   1                                  589 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.5.biospecimen_barcode_bottom:
100513090      NA's 
        1       589 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.5.center_id:
   8 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.5.concentration:
 0.5 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.5.day_of_shipment:
   1 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.5.month_of_shipment:
   9 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.5.plate_column:
   5 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.5.plate_id:
1215 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.5.plate_row:
   a NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.5.quantity:
  40 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.5.source_center:
  22 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.5.volume:
  80 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.5.year_of_shipment:
2010 NA's 
   1  589 



patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100502651 100511828 100518138 100514564 100518830 100518948       427 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.center_id:
   8   10 NA's 
 163   37  390 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.concentration:
 0.5 NA's 
 200  390 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.day_of_shipment:
   1    2   16   23 NA's 
 163   16   10   11  390 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 200  390 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.month_of_shipment:
   9   12 NA's 
 190   10  390 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   3.555   5.000   7.000     390 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    615    1213    1215    1111    1217    1218     390 


patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.quantity:
  30   40  200 NA's 
  21  163   16  390 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.source_center:
  22 NA's 
 200  390 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.volume:
  60   80  400 NA's 
  21  163   16  390 

patient.samples.sample.portions.portion.analytes.analyte.6.aliquots.aliquot.year_of_shipment:
2009 2010 NA's 
  37  163  390 

patient.samples.sample.portions.portion.analytes.analyte.6.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   135.8   153.0   187.1   171.2  1020.0     390 

patient.samples.sample.portions.portion.analytes.analyte.6.analyte_type:
repli-g (qiagen) dna                 NA's 
                 200                  390 

patient.samples.sample.portions.portion.analytes.analyte.6.analyte_type_id:
   w NA's 
 200  390 



patient.samples.sample.portions.portion.analytes.analyte.6.concentration:
 0.5 NA's 
 200  390 

patient.samples.sample.portions.portion.analytes.analyte.6.dna.normal_tumor_genotype_match:
 yes NA's 
 200  390 

patient.samples.sample.portions.portion.analytes.analyte.6.dna.pcr_amplification_successful:
  no NA's 
 200  390 

patient.samples.sample.portions.portion.analytes.analyte.6.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             200 
                                            NA's 
                                             390 

patient.samples.sample.portions.portion.analytes.analyte.6.is_derived_from_ffpe:
  no NA's 
 200  390 

patient.samples.sample.portions.portion.analytes.analyte.6.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
    200     390 

patient.samples.sample.portions.portion.analytes.analyte.6.spectrophotometer_method:
uv spec    NA's 
    200     390 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.300   1.900   1.900   1.897   1.900   2.000      22 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.bcr_aliquot_barcode:
tcga-04-1349-01a-01d-a324-10                         NA's 
                           1                          589 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.bcr_aliquot_uuid:
5533240d-8fed-440d-b781-6092785a77ce                                 NA's 
                                   1                                  589 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.biospecimen_barcode_bottom:
153880967      NA's 
        1       589 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.center_id:
  10 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.concentration:
0.07 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.day_of_shipment:
   9 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.is_derived_from_ffpe:
  no NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.month_of_shipment:
   9 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_column:
   1 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_id:
a324 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.plate_row:
   c NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.quantity:
 1.4 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.source_center:
  23 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.volume:
  20 NA's 
   1  589 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.10.year_of_shipment:
2013 NA's 
   1  589 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97529615  97530212  97530254 107941590  97543773 167780534       522 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   4.000   4.351   4.000   9.000      12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0100  0.1400  0.1500  0.1492  0.1600  0.1900      12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   11.00   15.00   16.56   24.00   28.00      12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 578   12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.559  10.000  12.000      12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.265   5.000   6.000      12 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.170   3.740   4.010   3.631   4.010   4.540      12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  22   23 NA's 
 568   10   12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   26.70   26.70   24.46   26.70   40.00      12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2008 2009 2010 2014 NA's 
  72  438   58   10   12 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97526536  97526582  97526626 107939183  97543694 167780468       522 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  4.000   6.000   6.000   5.796   6.000  10.000      12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0100  0.1400  0.1500  0.1505  0.1600  0.1900      12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   11.00   15.00   16.64   24.00   28.00      12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 578   12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.559  10.000  12.000      12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.265   5.000   6.000      12 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.130   1.860   2.000   2.236   2.130   4.540      12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  22   23 NA's 
 568   10   12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   13.30   13.30   14.84   13.30   26.70      12 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2008 2009 2010 2014 NA's 
  72  438   58   10   12 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
97529010 97529538 97529578 97625615 97529605 98887543      532 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   2.000   3.004   5.000   9.000      23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1200  0.1400  0.1500  0.1507  0.1600  0.1900      23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   10.00   15.00   16.47   24.00   28.00      23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 567   23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.591  10.000  12.000      23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.272   5.000   6.000      23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  359.0   459.0   562.0   587.8   666.0  1137.0      23 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   2.130   2.800   2.664   3.000   3.400      23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  22 NA's 
 567   23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
 6.7 11.8 13.3   20 NA's 
  10   38  127  392   23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2008 2009 2010 NA's 
  33  476   58   23 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.753e+07 9.753e+07 9.753e+07 1.445e+08 9.754e+07 1.050e+09       529 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   1    2    5   13 NA's 
  10  159  395    3   23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1200  0.1400  0.1500  0.1507  0.1600  0.1900      23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   11.00   15.00   16.54   24.00   28.00      23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
 567   23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.591  10.000  12.000      23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.303   5.000  11.000      23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  360.0   460.0   563.0   594.6   667.0  1565.0      23 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   2.000   2.000   2.256   2.800   3.400      23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  22 NA's 
 567   23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
 6.7 13.3 17.6   20 NA's 
  10  395   38  124   23 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2008 2009 2010 2011 NA's 
  71  435   58    3   23 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.751e+07 9.753e+07 9.753e+07 2.987e+08 1.539e+08 1.054e+09       376 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.500   4.111   8.000  12.000     310 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1300  0.1500  0.1306  0.1600  0.1800     310 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    9.00    9.00   11.92   19.00   27.00     310 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
 280  310 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   5.132   6.000  12.000     310 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.364   5.000  12.000     310 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.500   0.530   2.279   3.000  22.660     310 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  22   23 NA's 
 260   20  310 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    3.33    3.33   21.58   20.00  133.30     310 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2008    2010    2010    2010    2011    2013     310 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.751e+07 9.753e+07 9.753e+07 1.345e+08 1.539e+08 1.050e+09       506 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   8.000   5.667   8.000  12.000     452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.1400  0.1500  0.1411  0.1600  0.1900     452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    9.00   11.00   14.39   19.00   26.00     452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
 138  452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   6.000   7.000   8.391  12.000  12.000     452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   4.022   5.000  12.000     452 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.500   1.004   0.630  25.500     452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  22   23 NA's 
 117   21  452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   3.330   8.604   3.330 172.000     452 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
2009 2010 2011 2013 NA's 
   4   61   52   21  452 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
9.753e+07 9.888e+07 1.539e+08 2.073e+08 1.539e+08 1.053e+09       566 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000   8.000   7.361   9.000  10.000     554 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.05    0.08    0.15    0.13    0.17    0.19     554 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   17.00   19.00   18.72   26.00   28.00     554 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
  36  554 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1       6       7       8      12      12     554 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   3.500   3.833   6.000  10.000     554 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.7650  0.8292  1.0700  4.6600     554 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  22   23 NA's 
  24   12  554 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   5.000   8.193  13.400  33.300     554 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2009    2010    2011    2011    2013    2015     554 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97531696 116440647 153880920 139147287 153880984 167811755       580 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.center_id:
   1    8    9   10 NA's 
   1    2    2    6  579 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0750  0.0800  0.1073  0.1500  0.1700     579 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   9.00    9.00    9.00   15.27   21.00   27.00     579 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe:
  no NA's 
  11  579 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.500   9.000   7.818   9.000  12.000     579 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   2.000   3.454   4.000  11.000     579 


patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_row:
   a    b    d    h NA's 
   6    3    1    1  579 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.97    1.40    1.24    1.60    2.26     579 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.source_center:
  22   23 NA's 
   4    7  579 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   9.985  13.400  13.646  20.000  20.000     579 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.year_of_shipment:
2010 2011 2013 2015 NA's 
   3    1    6    1  579 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97529003  97529732  97529774 107941183  97543502 167780541       522 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    5    7 NA's 
 568   10    3    9 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0100  0.1400  0.1500  0.1545  0.1600  0.5000       9 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   11.00   15.00   16.59   24.00   28.00       9 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 581    9 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.577  10.000  12.000       9 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   3.284   5.000   9.000       9 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.200   0.940   1.010   3.462   1.070 200.000       9 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  22   23 NA's 
 571   10    9 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.90    6.70    6.70   11.84    6.70  400.00       9 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2008 2009 2010 2014 NA's 
  75  438   58   10    9 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00   76.67  218.34  402.34 1042.40       9 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
      dna       rna total rna      NA's 
      574         4         3         9 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
   d    r    t NA's 
 574    4    3    9 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0100  0.1400  0.1500  0.1503  0.1600  0.1900       9 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
 574   16 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
  no  yes NA's 
  40  534   16 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             581 
                                            NA's 
                                               9 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
 581    9 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
 adna preparation type allprep rna extraction              total rna 
                   574                      4                      3 
                  NA's 
                     9 

patient.samples.sample.portions.portion.analytes.analyte.rna.ratio_28s_18s:
   0    1  1.1  1.4 NA's 
   1    1    1    1  586 

patient.samples.sample.portions.portion.analytes.analyte.rna.rinvalue:
 7.5  7.6  8.6 NA's 
   2    1    1  586 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
       10       571         9 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2 NA's 
   8    2  580 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    4.00   17.00   15.04   21.00   31.00       5 

patient.samples.sample.portions.portion.is_ffpe:
  no NA's 
 585    5 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   5.000   5.456   8.000  12.000       5 

patient.samples.sample.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.253   1.000  12.000       5 

patient.samples.sample.portions.portion.portion_sequence:
   1 NA's 
   8  582 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
 563   27 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    5.00   20.02   17.50  100.00     507 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.638   0.000  30.000     543 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   2.957   5.000  20.000      27 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00    7.72    5.00   50.00     540 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.392   0.000  85.000      39 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   10.00   12.27   20.00   89.00      46 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0    75.0    85.0    81.4    90.0   100.0      41 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   85.00   90.00   88.18   95.00   99.00      27 

patient.samples.sample.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
    45    518     27 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
 575   15 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    4.00   14.03   10.00  100.00     496 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.403   1.000  20.000     533 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    1.00    3.24    5.00   20.00      15 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   5.648   3.750  50.000     536 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.383   0.000  58.000      26 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   10.00   13.44   20.00   90.00      39 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   75.00   85.00   80.05   90.00  100.00      28 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  60.00   80.00   90.00   87.44   95.00   99.00      15 

patient.samples.sample.portions.portion.slides.slide.section_location:
bottom    top   NA's 
   518     57     15 

patient.samples.sample.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   25.0   106.0   122.0   121.9   140.0   229.0      15 

patient.samples.sample.portions.portion.year_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2008    2009    2009    2009    2009    2014       5 


patient.samples.sample.portions.shipment_portion.2.center_id:
  20   28   29 NA's 
   4  117   55  414 

patient.samples.sample.portions.shipment_portion.2.is_ffpe:
  no NA's 
 176  414 

patient.samples.sample.portions.shipment_portion.2.plate_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2214    2222    2223    2223    2224    2225     414 

patient.samples.sample.portions.shipment_portion.2.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  21.00   31.00   31.00   30.69   31.00   41.00     414 


patient.samples.sample.portions.shipment_portion.2.shipment_portion_day_of_shipment:
   2   12 NA's 
 172    4  414 

patient.samples.sample.portions.shipment_portion.2.shipment_portion_month_of_shipment:
   3    4 NA's 
   4  172  414 

patient.samples.sample.portions.shipment_portion.2.shipment_portion_year_of_shipment:
2012 NA's 
 176  414 


patient.samples.sample.portions.shipment_portion.3.center_id:
  28   29 NA's 
   2   33  555 

patient.samples.sample.portions.shipment_portion.3.is_ffpe:
  no NA's 
  35  555 

patient.samples.sample.portions.shipment_portion.3.plate_id:
2222 2224 2225 NA's 
   2   27    6  555 

patient.samples.sample.portions.shipment_portion.3.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  22.00   32.00   32.00   32.26   32.00   42.00     555 


patient.samples.sample.portions.shipment_portion.3.shipment_portion_day_of_shipment:
   2 NA's 
  35  555 

patient.samples.sample.portions.shipment_portion.3.shipment_portion_month_of_shipment:
   4 NA's 
  35  555 

patient.samples.sample.portions.shipment_portion.3.shipment_portion_year_of_shipment:
2012 NA's 
  35  555 

patient.samples.sample.portions.shipment_portion.4.bcr_shipment_portion_uuid:
fec21c14-b283-4442-a720-bcd59a8bd52a                                 NA's 
                                   1                                  589 

patient.samples.sample.portions.shipment_portion.4.center_id:
  29 NA's 
   1  589 

patient.samples.sample.portions.shipment_portion.4.is_ffpe:
  no NA's 
   1  589 

patient.samples.sample.portions.shipment_portion.4.plate_id:
2224 NA's 
   1  589 

patient.samples.sample.portions.shipment_portion.4.portion_number:
  42 NA's 
   1  589 

patient.samples.sample.portions.shipment_portion.4.shipment_portion_bcr_aliquot_barcode:
tcga-13-1489-01a-42-2224-29                        NA's 
                          1                         589 

patient.samples.sample.portions.shipment_portion.4.shipment_portion_day_of_shipment:
   2 NA's 
   1  589 

patient.samples.sample.portions.shipment_portion.4.shipment_portion_month_of_shipment:
   4 NA's 
   1  589 

patient.samples.sample.portions.shipment_portion.4.shipment_portion_year_of_shipment:
2012 NA's 
   1  589 


patient.samples.sample.portions.shipment_portion.center_id:
  20   28   29 NA's 
 420    3    2  165 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
 425  165 


patient.samples.sample.portions.shipment_portion.portion_number:
   3   11   21   31 NA's 
  16   14  387    8  165 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   2 NA's 
   8  582 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
   2   12   14   30 NA's 
   5    3  407   10  165 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   2    3    4    7 NA's 
 407    3    5   10  165 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2011 2012 2014 NA's 
 407    8   10  165 

patient.samples.sample.sample_type:
  primary tumor recurrent tumor 
            589               1 

patient.samples.sample.sample_type_id:
  1   2 
589   1 

patient.samples.sample.shortest_dimension:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0500  0.3000  0.4000  0.4252  0.5000  1.5000      20 

patient.samples.sample.vial_number:
  a   b   c   d 
566  21   2   1 

}}
\keyword{datasets}
