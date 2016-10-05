\name{ CESC }
\alias{ CESC }
\docType{data}
\title{ CESC }

\details{
\preformatted{
> CESC
A MultiAssayExperiment object of 9 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 9: 
 [1] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 309 columns 
 [2] miRNASeqGene: ExpressionSet with 1046 rows and 312 columns 
 [3] CNASNP: RangedRaggedAssay with 267855 rows and 586 columns 
 [4] CNVSNP: RangedRaggedAssay with 59450 rows and 586 columns 
 [5] CNAseq: RangedRaggedAssay with 7834 rows and 100 columns 
 [6] Methylation: SummarizedExperiment with 485577 rows and 312 columns 
 [7] RPPAArray: ExpressionSet with 192 rows and 173 columns 
 [8] gistica: SummarizedExperiment with 24776 rows and 295 columns 
 [9] gistict: SummarizedExperiment with 24776 rows and 295 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( CESC )
CharacterList of length 9
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1 2 3 4 5 6 7 ... 267849 267850 267851 267852 267853 267854 267855
[["CNVSNP"]] 1 2 3 4 5 6 7 8 ... 59444 59445 59446 59447 59448 59449 59450
[["CNAseq"]] 1 2 3 4 5 6 7 8 9 ... 7826 7827 7828 7829 7830 7831 7832 7833 7834
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
[["gistict"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7

> colnames( CESC )
CharacterList of length 9
[["RNASeq2GeneNorm"]] TCGA-2W-A8YY-01A TCGA-4J-AA1J-01A ... TCGA-ZX-AA5X-01A
[["miRNASeqGene"]] TCGA-2W-A8YY-01A TCGA-4J-AA1J-01A ... TCGA-ZX-AA5X-01A
[["CNASNP"]] TCGA-2W-A8YY-01A-11D-A37M-01 ... TCGA-ZX-AA5X-10A-01D-A42Q-01
[["CNVSNP"]] TCGA-2W-A8YY-01A-11D-A37M-01 ... TCGA-ZX-AA5X-10A-01D-A42Q-01
[["CNAseq"]] TCGA-C5-A1BQ-01C-11D-A20X-26 ... TCGA-JX-A3Q8-10A-01D-A245-26
[["Methylation"]] TCGA-2W-A8YY-01A-11D-A37Q-05 ... TCGA-ZX-AA5X-01A-11D-A42P-05
[["RPPAArray"]] TCGA-2W-A8YY-01A TCGA-4J-AA1J-01A ... TCGA-ZJ-AB0I-01A
[["gistica"]] TCGA-2W-A8YY-01A-11D-A37M-01 ... TCGA-ZX-AA5X-01A-11D-A42N-01
[["gistict"]] TCGA-2W-A8YY-01A-11D-A37M-01 ... TCGA-ZX-AA5X-01A-11D-A42N-01

Sizes of each ExperimentList element:

            assay size.Mb
1 RNASeq2GeneNorm  1.2 Mb
2    miRNASeqGene  0.1 Mb
3          CNASNP 20.6 Mb
4          CNVSNP  4.7 Mb
5          CNAseq  0.6 Mb
6     Methylation 67.4 Mb
7       RPPAArray  0.1 Mb
8         gistica  4.3 Mb
9         gistict  4.3 Mb

--------------------------- 
Overall survival time-to-event summary (in years):
--------------------------- 

Call: survfit(formula = survival::Surv(object$days_to_death/365, object$vital_status) ~ 
    -1)

   235 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  72.00   71.00    1.70    1.39    2.29 


--------------------------- 
Available sample meta-data: 
--------------------------- 

Composite.Element.REF: 
value 
  307 

years_to_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0    38.0    46.0    48.2    56.0    88.0       2 

vital_status: 
  0   1 
236  71 

days_to_death: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   14.0   349.5   620.0   881.3  1070.0  4086.0     235 

days_to_last_followup: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     287     652    1024    1263    6408      58 

tumor_tissue_site: 
cervical 
     307 


pathology_N_stage: 
  n0   n1   nx NA's 
 135   60   67   45 

pathology_M_stage: 
  m0   m1   mx NA's 
 116   10  131   50 

gender: 
female 
   307 

date_of_initial_pathologic_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1994    2006    2010    2008    2012    2013       2 

radiation_therapy: 
  no  yes NA's 
  55  129  123 


number_pack_years_smoked: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.20    6.50   15.00   17.38   25.00   57.00     214 

number_of_lymph_nodes: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.044   1.000  16.000     148 


ethnicity: 
    hispanic or latino not hispanic or latino                   NA's 
                    24                    171                    112 

weight_kg_at_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  31.00   58.25   70.50   73.04   83.75  210.00      29 

tumor_status: 
tumor free with tumor       NA's 
       201         80         26 


tobacco_smoking_year_stopped: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1949    1995    2003    2000    2010    2013     264 

tobacco_smoking_pack_years_smoked: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.20    6.50   15.00   17.38   25.00   57.00     214 


agebegansmokinginyears: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   16.00   18.00   21.14   24.00   44.00     222 

radiation_therapy_status: 
   completed as planned treatment not completed                    NA's 
                     29                       3                     275 

pregnancies_count_total: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    2.00    3.00    3.61    5.00   15.00      40 

pregnancies_count_stillbirth: 
   0    1    3 NA's 
 106    5    1  195 

pregnancy_spontaneous_abortion_count: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.5442  1.0000  6.0000     160 

pregnancies_count_live_birth: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   1.250   2.000   2.844   4.000  11.000      45 

pregnancy_therapeutic_abortion_count: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.8525  1.0000 13.0000     185 

pregnancies_count_ectopic: 
   0    1    2 NA's 
 104   11    1  191 


lymph_node_location: 
lymph_node_location_positive_pathology_name|lymph_node_location_positive_pathology_text 
                                                                                    307 


menopause_status: 
                                               indeterminate (neither pre or postmenopausal) 
                                                                                           3 
                                              peri (6-12 months since last menstrual period) 
                                                                                          25 
           post (prior bilateral ovariectomy or >12 mo since lmp with no prior hysterectomy) 
                                                                                          84 
pre (<6 months since lmp and no prior bilateral ovariectomy and not on estrogen replacement) 
                                                                                         125 
                                                                                        NA's 
                                                                                          70 

lymphovascular_involvement: 
 absent present    NA's 
     72      80     155 

lymph_nodes_examined_he_count: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   1.044   1.000  16.000     148 

lymph_nodes_examined: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   13.00   21.00   22.28   29.00   63.00     127 

keratinization_squamous_cell: 
    keratinizing squamous cell carcinoma 
                                      55 
non-keratinizing squamous cell carcinoma 
                                     120 
                                    NA's 
                                     132 

initial_pathologic_dx_year: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1994    2006    2010    2008    2012    2013       2 

hysterectomy_type: 
hysterectomy_performed_type|hysterectomy_performed_text 
                                                    307 

history_hormonal_contraceptives_use: 
current user  former user   never used         NA's 
          15           54           90          148 

height_cm_at_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    132     157     160     161     165     183      43 

corpus_involvement: 
 absent present    NA's 
     99      19     189 

chemo_concurrent_type: 
carboplatin   cisplatin       other        NA's 
          7         104           2         194 

cervix_suv_results: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    8.70   12.00   13.22   16.92   28.80     290 

cause_of_death: 
cardiopulmonary arrest            lung cancer          renal failure 
                     1                      1                      1 
                  NA's 
                   304 

age_at_diagnosis: 
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  20.00   38.00   46.00   48.27   56.50   88.00 


}}
\keyword{datasets}
