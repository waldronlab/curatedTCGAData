\name{ OV }
\alias{ OV }
\docType{data}
\title{ Ovarian serous cystadenocarcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( OV )
ExperimentList class object of length 17: 
 [1] CNACGH_CGH_hg_244a-20160128: RaggedExperiment with 128946 rows and 472 columns 
 [2] CNACGH_CGH_hg_415k_g4124a-20160128: RaggedExperiment with 245847 rows and 674 columns 
 [3] CNASNP-20160128: RaggedExperiment with 907765 rows and 1145 columns 
 [4] CNVSNP-20160128: RaggedExperiment with 261680 rows and 1141 columns 
 [5] GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 573 columns 
 [6] GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 573 columns 
 [7] Methylation_methyl27-20160128: SummarizedExperiment with 27578 rows and 591 columns 
 [8] Methylation_methyl450-20160128: SummarizedExperiment with 485577 rows and 10 columns 
 [9] miRNAArray-20160128: SummarizedExperiment with 821 rows and 573 columns 
 [10] miRNASeqGene-20160128: SummarizedExperiment with 705 rows and 461 columns 
 [11] mRNAArray_huex-20160128: SummarizedExperiment with 18632 rows and 575 columns 
 [12] mRNAArray_TX_g4502a-20160128: SummarizedExperiment with 17814 rows and 577 columns 
 [13] mRNAArray_TX_ht_hg_u133a-20160128: SummarizedExperiment with 12042 rows and 524 columns 
 [14] Mutation-20160128: RaggedExperiment with 20219 rows and 316 columns 
 [15] RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 307 columns 
 [16] RNASeqGene-20160128: SummarizedExperiment with 19990 rows and 299 columns 
 [17] RPPAArray-20160128: SummarizedExperiment with 208 rows and 427 columns 

> rownames( OV )
CharacterList of length 17
[["CNACGH_CGH_hg_244a-20160128"]] character(0)
[["CNACGH_CGH_hg_415k_g4124a-20160128"]] character(0)
[["CNASNP-20160128"]] character(0)
[["CNVSNP-20160128"]] character(0)
[["GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["GISTIC_ThresholdedByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["Methylation_methyl27-20160128"]] cg00000292 cg00002426 ... cg27665659
[["Methylation_methyl450-20160128"]] cg00000029 cg00000108 ... rs9839873
[["miRNAArray-20160128"]] DarkCorner dmr_285 dmr_3 ... NegativeControl SCorner3
[["miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
...
<7 more elements>

> colnames( OV )
CharacterList of length 17
[["CNACGH_CGH_hg_244a-20160128"]] TCGA-04-1331-01A-01D-0431-02 ...
[["CNACGH_CGH_hg_415k_g4124a-20160128"]] TCGA-04-1353-01A-01D-1046-02 ...
[["CNASNP-20160128"]] TCGA-04-1331-01A-01D-0428-01 ...
[["CNVSNP-20160128"]] TCGA-04-1331-01A-01D-0428-01 ...
[["GISTIC_AllByGene-20160128"]] TCGA-04-1331-01A-01D-0428-01 ...
[["GISTIC_ThresholdedByGene-20160128"]] TCGA-04-1331-01A-01D-0428-01 ...
[["Methylation_methyl27-20160128"]] TCGA-04-1331-01A-01D-0432-05 ...
[["Methylation_methyl450-20160128"]] TCGA-13-A5FT-01A-11D-A409-05 ...
[["miRNAArray-20160128"]] TCGA-04-1331-01A-01T-0438-07 ...
[["miRNASeqGene-20160128"]] TCGA-04-1331-01A-01R-1569-13 ...
...
<7 more elements>

Sizes of each ExperimentList element:

                                assay size.Mb
1         CNACGH_CGH_hg_244a-20160128  3.6 Mb
2  CNACGH_CGH_hg_415k_g4124a-20160128  6.8 Mb
3                     CNASNP-20160128 24.6 Mb
4                     CNVSNP-20160128  7.3 Mb
5           GISTIC_AllByGene-20160128  4.9 Mb
6   GISTIC_ThresholdedByGene-20160128  4.9 Mb
7       Methylation_methyl27-20160128  4.9 Mb
8      Methylation_methyl450-20160128   75 Mb
9                 miRNAArray-20160128  0.1 Mb
10              miRNASeqGene-20160128  0.1 Mb
11            mRNAArray_huex-20160128  1.2 Mb
12       mRNAArray_TX_g4502a-20160128  1.2 Mb
13  mRNAArray_TX_ht_hg_u133a-20160128  0.8 Mb
14                  Mutation-20160128  7.6 Mb
15           RNASeq2GeneNorm-20160128  1.3 Mb
16                RNASeqGene-20160128  1.3 Mb
17                 RPPAArray-20160128  0.1 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
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
     16     266     842    1216    1931    5481     358 

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

Including an additional 2869 columns
}}
\keyword{datasets}
