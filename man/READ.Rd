\name{ READ }
\alias{ READ }
\docType{data}
\title{ READ }

\details{
\preformatted{
> READ
A MultiAssayExperiment object of 12 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 12: 
 [1] RNASeqGene: ExpressionSet with 20502 rows and 72 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 72 columns 
 [3] miRNASeqGene: ExpressionSet with 705 rows and 76 columns 
 [4] CNASNP: RangedRaggedAssay with 156806 rows and 316 columns 
 [5] CNVSNP: RangedRaggedAssay with 35765 rows and 316 columns 
 [6] CNAseq: RangedRaggedAssay with 56380 rows and 70 columns 
 [7] Methylation: SummarizedExperiment with 485577 rows and 106 columns 
 [8] mRNAArray: ExpressionSet with 17814 rows and 72 columns 
 [9] RPPAArray: ExpressionSet with 208 rows and 131 columns 
 [10] Mutations: RangedRaggedAssay with 22075 rows and 69 columns 
 [11] gistica: SummarizedExperiment with 24776 rows and 165 columns 
 [12] gistict: SummarizedExperiment with 24776 rows and 165 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( READ )
CharacterList of length 12
[["RNASeqGene"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZYX ZZEF1 ZZZ3 psiTPTE22 tAKR
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 156800 156801 156802 156803 156804 156805 156806
[["CNVSNP"]] 1 2 3 4 5 6 7 8 ... 35759 35760 35761 35762 35763 35764 35765
[["CNAseq"]] 1 2 3 4 5 6 7 8 ... 56374 56375 56376 56377 56378 56379 56380
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["mRNAArray"]] ELMO2 CREB3L1 RPS11 PNMA1 MMP2 ... SLC39A6 SNRPD2 AQP7 CTSC
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] WNK3 IFRD1 PPP1R3A FAM71F1 ... LRRC14B ADAMTS16 MARVELD2
...
<2 more elements>

> colnames( READ )
CharacterList of length 12
[["RNASeqGene"]] TCGA-AF-2691-01A TCGA-AF-2692-01A ... TCGA-AG-A036-01A
[["RNASeq2GeneNorm"]] TCGA-AF-2691-01A TCGA-AF-2692-01A ... TCGA-AG-A036-01A
[["miRNASeqGene"]] TCGA-AF-2687-01A TCGA-AF-2690-01A ... TCGA-G5-6641-01A
[["CNASNP"]] TCGA-AF-2687-01A-02D-1732-01 ... TCGA-G5-6641-10A-01D-1825-01
[["CNVSNP"]] TCGA-AF-2687-01A-02D-1732-01 ... TCGA-G5-6641-10A-01D-1825-01
[["CNAseq"]] TCGA-AF-2691-01A-01D-1167-02 ... TCGA-AG-A032-10A-01D-A078-02
[["Methylation"]] TCGA-AF-2687-01A-02D-1734-05 ... TCGA-G5-6641-01A-11D-1828-05
[["mRNAArray"]] TCGA-AF-2689-11A TCGA-AF-2689-01A ... TCGA-AG-A032-01A
[["RPPAArray"]] TCGA-AF-2689-01A TCGA-AF-2690-01A ... TCGA-G5-6641-01A
[["Mutations"]] TCGA-AF-2689-01A-01W-0831-10 ... TCGA-AG-A036-01
...
<2 more elements>

Sizes of each ExperimentList element:

             assay size.Mb
1       RNASeqGene  1.1 Mb
2  RNASeq2GeneNorm  1.1 Mb
3     miRNASeqGene  0.1 Mb
4           CNASNP 12.1 Mb
5           CNVSNP  2.8 Mb
6           CNAseq  4.3 Mb
7      Methylation 67.4 Mb
8        mRNAArray    1 Mb
9        RPPAArray    0 Mb
10       Mutations    9 Mb
11         gistica  4.3 Mb
12         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   142 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  27.00   26.00    2.00    1.44    3.29 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  169 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  31.00   57.00   66.00   64.37   72.00   90.00 

vital_status: 
  0   1 
142  27 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   59.0   347.5   730.0   786.1  1193.0  1741.0     142 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   257.0   579.0   712.4  1035.0  3932.0       6 

tumor_tissue_site: 
rectum   NA's 
   166      3 




pathology_M_stage: 
  m0   m1  m1a   mx NA's 
 129   22    2   14    2 

gender: 
female   male 
    77     92 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1999    2007    2009    2008    2010    2012 

days_to_last_known_alive: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   31.0   292.2   863.0  1420.0  2214.0  3667.0     161 

radiation_therapy: 
  no  yes NA's 
 114   22   33 

histological_type: 
         rectal adenocarcinoma rectal mucinous adenocarcinoma 
                           150                             13 
                          NA's 
                             6 

residual_tumor: 
  r0   r1   r2   rx NA's 
 126    2   12    5   24 

number_of_lymph_nodes: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   2.692   3.000  31.000      10 

race: 
                    asian black or african american                     white 
                        1                         6                        83 
                     NA's 
                       79 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                     1                     84                     84 

}}
\keyword{datasets}
