\name{ UVM }
\alias{ UVM }
\docType{data}
\title{ Uveal Melanoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( UVM )
ExperimentList class object of length 11: 
 [1] UVM_CNASeq-20160128: RaggedExperiment with 6496 rows and 102 columns 
 [2] UVM_CNASNP-20160128: RaggedExperiment with 69487 rows and 160 columns 
 [3] UVM_CNVSNP-20160128: RaggedExperiment with 12973 rows and 160 columns 
 [4] UVM_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 80 columns 
 [5] UVM_GISTIC_Peaks-20160128: RangedSummarizedExperiment with 21 rows and 80 columns 
 [6] UVM_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 80 columns 
 [7] UVM_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 80 columns 
 [8] UVM_Mutation-20160128: RaggedExperiment with 2174 rows and 80 columns 
 [9] UVM_RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 80 columns 
 [10] UVM_RPPAArray-20160128: SummarizedExperiment with 192 rows and 12 columns 
 [11] UVM_Methylation-20160128: SummarizedExperiment with 485577 rows and 80 columns 

> rownames( UVM )
CharacterList of length 11
[["UVM_CNASeq-20160128"]] character(0)
[["UVM_CNASNP-20160128"]] character(0)
[["UVM_CNVSNP-20160128"]] character(0)
[["UVM_GISTIC_AllByGene-20160128"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["UVM_GISTIC_Peaks-20160128"]] chr1:19073360-24108626 ...
[["UVM_GISTIC_ThresholdedByGene-20160128"]] ACAP3 ...
[["UVM_miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["UVM_Mutation-20160128"]] character(0)
[["UVM_RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 ... ZZZ3 psiTPTE22 tAKR
[["UVM_RPPAArray-20160128"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK_pT359_S363
...
<1 more element>

> colnames( UVM )
CharacterList of length 11
[["UVM_CNASeq-20160128"]] TCGA-RZ-AB0B-01A-11D-A40D-26 ...
[["UVM_CNASNP-20160128"]] TCGA-RZ-AB0B-01A-11D-A39V-01 ...
[["UVM_CNVSNP-20160128"]] TCGA-RZ-AB0B-01A-11D-A39V-01 ...
[["UVM_GISTIC_AllByGene-20160128"]] TCGA-RZ-AB0B-01A-11D-A39V-01 ...
[["UVM_GISTIC_Peaks-20160128"]] TCGA-RZ-AB0B-01A-11D-A39V-01 ...
[["UVM_GISTIC_ThresholdedByGene-20160128"]] TCGA-RZ-AB0B-01A-11D-A39V-01 ...
[["UVM_miRNASeqGene-20160128"]] TCGA-RZ-AB0B-01A-11R-A40B-13 ...
[["UVM_Mutation-20160128"]] TCGA-RZ-AB0B-01A-11D-A39W-08 ...
[["UVM_RNASeq2GeneNorm-20160128"]] TCGA-RZ-AB0B-01A-11R-A405-07 ...
[["UVM_RPPAArray-20160128"]] TCGA-V3-A9ZX-01A-21-A41Z-20 ...
...
<1 more element>

Sizes of each ExperimentList element:

                                   assay size.Mb
1                    UVM_CNASeq-20160128  0.2 Mb
2                    UVM_CNASNP-20160128  1.9 Mb
3                    UVM_CNVSNP-20160128  0.4 Mb
4          UVM_GISTIC_AllByGene-20160128  4.9 Mb
5              UVM_GISTIC_Peaks-20160128    0 Mb
6  UVM_GISTIC_ThresholdedByGene-20160128  4.9 Mb
7              UVM_miRNASeqGene-20160128  0.1 Mb
8                  UVM_Mutation-20160128 12.3 Mb
9           UVM_RNASeq2GeneNorm-20160128  1.3 Mb
10                UVM_RPPAArray-20160128    0 Mb
11              UVM_Methylation-20160128   75 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   57 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  23.00   23.00    1.66    1.14    3.05 


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  22.00   51.00   61.50   61.65   74.25   86.00 

vital_status:
 0  1 
57 23 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   40.0   393.5   606.0   693.9  1029.0  1581.0      57 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0   495.0   821.0   861.9  1184.0  2600.0      23 

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

Including an additional 448 columns
}}
\seealso{\link{UVM-v2.0.1}}
\keyword{datasets}
