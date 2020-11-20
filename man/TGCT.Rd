\name{ TGCT }
\alias{ TGCT }
\docType{data}
\title{ Testicular Germ Cell Tumors }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( TGCT )
ExperimentList class object of length 10: 
 [1] TGCT_CNASNP-20160128: RaggedExperiment with 137968 rows and 271 columns 
 [2] TGCT_CNVSNP-20160128: RaggedExperiment with 25479 rows and 271 columns 
 [3] TGCT_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 134 columns 
 [4] TGCT_GISTIC_Peaks-20160128: RangedSummarizedExperiment with 48 rows and 134 columns 
 [5] TGCT_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 134 columns 
 [6] TGCT_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 139 columns 
 [7] TGCT_Mutation-20160128: RaggedExperiment with 14672 rows and 138 columns 
 [8] TGCT_RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 139 columns 
 [9] TGCT_RPPAArray-20160128: SummarizedExperiment with 192 rows and 107 columns 
 [10] TGCT_Methylation-20160128: SummarizedExperiment with 485577 rows and 139 columns 

> rownames( TGCT )
CharacterList of length 10
[["TGCT_CNASNP-20160128"]] character(0)
[["TGCT_CNVSNP-20160128"]] character(0)
[["TGCT_GISTIC_AllByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["TGCT_GISTIC_Peaks-20160128"]] chr1:1-48649489 ... chr22:18613558-22141824
[["TGCT_GISTIC_ThresholdedByGene-20160128"]] ACAP3 ...
[["TGCT_miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["TGCT_Mutation-20160128"]] character(0)
[["TGCT_RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 ... ZZZ3 psiTPTE22 tAKR
[["TGCT_RPPAArray-20160128"]] 14-3-3_beta ... p90RSK_pT359_S363
[["TGCT_Methylation-20160128"]] cg00000029 cg00000108 ... rs966367 rs9839873

> colnames( TGCT )
CharacterList of length 10
[["TGCT_CNASNP-20160128"]] TCGA-2G-AAEW-01A-11D-A42X-01 ...
[["TGCT_CNVSNP-20160128"]] TCGA-2G-AAEW-01A-11D-A42X-01 ...
[["TGCT_GISTIC_AllByGene-20160128"]] TCGA-2G-AAEW-01A-11D-A42X-01 ...
[["TGCT_GISTIC_Peaks-20160128"]] TCGA-2G-AAEW-01A-11D-A42X-01 ...
[["TGCT_GISTIC_ThresholdedByGene-20160128"]] TCGA-2G-AAEW-01A-11D-A42X-01 ...
[["TGCT_miRNASeqGene-20160128"]] TCGA-2G-AAEW-01A-11R-A439-13 ...
[["TGCT_Mutation-20160128"]] TCGA-2G-AAEW-01A-11D-A42Y-10 ...
[["TGCT_RNASeq2GeneNorm-20160128"]] TCGA-2G-AAEW-01A-11R-A430-07 ...
[["TGCT_RPPAArray-20160128"]] TCGA-2G-AAEW-01A-21-A45P-20 ...
[["TGCT_Methylation-20160128"]] TCGA-2G-AAEW-01A-11D-A42Z-05 ...

Sizes of each ExperimentList element:

                                    assay size.Mb
1                    TGCT_CNASNP-20160128  3.8 Mb
2                    TGCT_CNVSNP-20160128  0.8 Mb
3          TGCT_GISTIC_AllByGene-20160128  4.9 Mb
4              TGCT_GISTIC_Peaks-20160128  0.1 Mb
5  TGCT_GISTIC_ThresholdedByGene-20160128  4.9 Mb
6              TGCT_miRNASeqGene-20160128  0.1 Mb
7                  TGCT_Mutation-20160128   10 Mb
8           TGCT_RNASeq2GeneNorm-20160128  1.3 Mb
9                 TGCT_RPPAArray-20160128    0 Mb
10              TGCT_Methylation-20160128   75 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   130 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 4.0000  4.0000  1.5493  0.0466      NA 


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  14.00   26.00   31.00   31.99   37.00   67.00 

vital_status:
  0   1 
130   4 

days_to_death:
  17  513  618 6972 NA's 
   1    1    1    1  130 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0   688.2  1265.5  2092.7  2826.2  7437.0       4 

tumor_tissue_site:
testes 
   134 


pathology_T_stage:
t1 t2 t3 tx 
76 51  6  1 

pathology_N_stage:
  n0   n1   n2   nx NA's 
  46   11    2   65   10 

pathology_M_stage:
  m0   m1  m1a  m1b NA's 
 115    2    1    1   15 

gender:
male 
 134 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1992    2006    2009    2008    2012    2013 

radiation_therapy:
  no  yes NA's 
 111   21    2 

karnofsky_performance_score:
  80   90  100 NA's 
   5   41   56   32 

race:
                    asian black or african american                     white 
                        4                         6                       119 
                     NA's 
                        5 

ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    12                    111                     11 

Including an additional 762 columns
}}
\seealso{\link{TGCT-v2.0.0}}
\keyword{datasets}
