\name{ SKCM }
\alias{ SKCM }
\docType{data}
\title{ SKCM }

\details{
\preformatted{
> SKCM
A MultiAssayExperiment object of 8 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 8: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 473 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 452 columns 
 [3] CNASNP: RangedRaggedAssay with 452114 rows and 938 columns 
 [4] CNVSNP: RangedRaggedAssay with 108084 rows and 937 columns 
 [5] CNAseq: RangedRaggedAssay with 31416 rows and 238 columns 
 [6] Methylation: SummarizedExperiment with 485577 rows and 475 columns 
 [7] RPPAArray: ExpressionSet with 181 rows and 206 columns 
 [8] Mutations: RangedRaggedAssay with 290322 rows and 345 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( SKCM )
CharacterList of length 8
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 452108 452109 452110 452111 452112 452113 452114
[["CNVSNP"]] 1 2 3 4 5 6 7 ... 108078 108079 108080 108081 108082 108083 108084
[["CNAseq"]] 1 2 3 4 5 6 7 8 ... 31410 31411 31412 31413 31414 31415 31416
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_epsilon 4E-BP1 4E-BP1_pS65 ... p90RSK p90RSK_pT359_S363
[["Mutations"]] EPHB6 BC101079 LYPD6B AK8 FAM123C ... CDKN2A CCDC90B RYR3 UBFD1

> colnames( SKCM )
CharacterList of length 8
[["RNASeq2GeneNorm"]] TCGA-3N-A9WB-06A TCGA-3N-A9WC-06A ... TCGA-Z2-AA3V-06A
[["miRNASeqGene"]] TCGA-3N-A9WC-06A TCGA-3N-A9WD-06A ... TCGA-Z2-AA3V-06A
[["CNASNP"]] TCGA-3N-A9WB-06A-11D-A38F-01 ... TCGA-Z2-AA3V-10A-01D-A399-01
[["CNVSNP"]] TCGA-3N-A9WB-06A-11D-A38F-01 ... TCGA-Z2-AA3V-10A-01D-A399-01
[["CNAseq"]] TCGA-BF-A1PU-01A-11D-A18Z-02 ... TCGA-HR-A2OH-10A-01D-A190-02
[["Methylation"]] TCGA-3N-A9WB-06A-11D-A38H-05 ... TCGA-Z2-AA3V-06A-11D-A398-05
[["RPPAArray"]] TCGA-BF-A1PU-01A TCGA-BF-A1PX-01A ... TCGA-IH-A3EA-01A
[["Mutations"]] TCGA-BF-A1PU-01A-11D-A19A-08 ... TCGA-RP-A695-06A-11D-A30X-08

Sizes of each ExperimentList element:

            assay  size.Mb
1 RNASeq2GeneNorm   1.2 Mb
2    miRNASeqGene   0.1 Mb
3          CNASNP  34.8 Mb
4          CNVSNP   8.5 Mb
5          CNAseq   2.5 Mb
6     Methylation  67.5 Mb
7       RPPAArray   0.1 Mb
8       Mutations 289.5 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   249 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
 221.00  219.00    2.99    2.40    3.92 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  470 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  15.00   48.00   58.00   58.24   71.00   90.00       8 

vital_status: 
  0   1 
249 221 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     79     518    1093    1789    2073   10870     249 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     -3     422    1010    1763    2359   11250     161 

days_to_submitted_specimen_dx: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     -2       0     344    1017    1372   10850      14 





melanoma_ulceration: 
  no  yes NA's 
 146  167  157 

melanoma_primary_known: 
 no yes 
 47 423 

Breslow_thickness: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.300   3.000   5.585   6.950  75.000     111 

gender: 
female   male 
   180    290 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1978    2003    2008    2006    2011    2013      11 

radiation_therapy: 
  no  yes NA's 
 420   49    1 

race: 
                    asian black or african american                     white 
                       12                         1                       447 
                     NA's 
                       10 

ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                    11                    446                     13 

}}
\keyword{datasets}
