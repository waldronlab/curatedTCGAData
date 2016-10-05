\name{ DLBC }
\alias{ DLBC }
\docType{data}
\title{ DLBC }

\details{
\preformatted{
> DLBC
A MultiAssayExperiment object of 9 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 9: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 15 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 47 columns 
 [3] CNASNP: RangedRaggedAssay with 44227 rows and 94 columns 
 [4] CNVSNP: RangedRaggedAssay with 9251 rows and 94 columns 
 [5] Methylation: SummarizedExperiment with 485577 rows and 48 columns 
 [6] RPPAArray: ExpressionSet with 192 rows and 33 columns 
 [7] Mutations: RangedRaggedAssay with 16918 rows and 48 columns 
 [8] gistica: SummarizedExperiment with 24776 rows and 48 columns 
 [9] gistict: SummarizedExperiment with 24776 rows and 48 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( DLBC )
CharacterList of length 9
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 8 ... 44719 44720 44721 44722 44723 44724 44725
[["CNVSNP"]] 1 2 3 4 5 6 7 8 9 ... 9335 9336 9337 9338 9339 9340 9341 9342 9343
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["Mutations"]] SGK1 TOP2B ITPKB BNC2 TET2 ... PAQR9 KRAS PPP1R3A DIP2B
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( DLBC )
CharacterList of length 9
[["RNASeq2GeneNorm"]] TCGA-FA-A7DS-01A TCGA-FA-A7Q1-01A ... TCGA-VB-A8QN-01A
[["miRNASeqGene"]] TCGA-FA-8693-01A TCGA-FA-A4BB-01A ... TCGA-VB-A8QN-01A
[["CNASNP"]] TCGA-FA-8693-01A-11D-2389-01 ... TCGA-VB-A8QN-10A-01D-A384-01
[["CNVSNP"]] TCGA-FA-8693-01A-11D-2389-01 ... TCGA-VB-A8QN-10A-01D-A384-01
[["Methylation"]] TCGA-FA-8693-01A-11D-2399-05 ... TCGA-VB-A8QN-01A-11D-A383-05
[["RPPAArray"]] TCGA-FA-8693-01A TCGA-FA-A4BB-01A ... TCGA-VB-A8QN-01A
[["Mutations"]] TCGA-FA-8693-01A-11D-2397-10 ... TCGA-VB-A8QN-01A-11D-A382-10
[["gistica"]] TCGA-FA-8693-01A-11D-2389-01 ... TCGA-VB-A8QN-01A-11D-A381-01
[["gistict"]] TCGA-FA-8693-01A-11D-2389-01 ... TCGA-VB-A8QN-01A-11D-A381-01

Sizes of each ExperimentList element:

            assay size.Mb
1 RNASeq2GeneNorm  1.1 Mb
2    miRNASeqGene  0.1 Mb
3          CNASNP  3.4 Mb
4          CNVSNP  0.7 Mb
5     Methylation 67.4 Mb
6       RPPAArray    0 Mb
7       Mutations 11.8 Mb
8         gistica  4.3 Mb
9         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   39 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  9.000   9.000   1.630   0.858      NA 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
   48 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  23.00   46.00   57.50   56.27   67.00   82.00 

vital_status: 
 0  1 
39  9 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     58     313     595    1505    1252    6425      39 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   441.5   832.0  1291.0  1581.0  5980.0       5 

gender: 
female   male 
    26     22 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1987    2009    2011    2009    2012    2013 

radiation_therapy: 
  no  yes NA's 
  40    7    1 

histological_type: 
diffuse large b-cell lymphoma (dlbcl) nos (any anatomic site nodal or extranodal) 
                                                                               41 
                                                         primary dlbcl of the cns 
                                                                                3 
                                               primary mediastinal (thymic) dlbcl 
                                                                                4 

race: 
                    asian black or african american                     white 
                       18                         1                        29 

ethnicity: 
    hispanic or latino not hispanic or latino 
                    12                     36 

}}
\keyword{datasets}
