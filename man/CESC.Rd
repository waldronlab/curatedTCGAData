\name{ CESC }
\alias{ CESC }
\docType{data}
\title{ Cervical squamous cell carcinoma and endocervical adenocarcinoma }
\description{
A document describing the TCGA cancer code
}

\details{
\preformatted{
> experiments( CESC )
ExperimentList class object of length 11: 
 [1] CESC_CNASeq-20160128: RaggedExperiment with 7834 rows and 100 columns 
 [2] CESC_CNASNP-20160128: RaggedExperiment with 267855 rows and 586 columns 
 [3] CESC_CNVSNP-20160128: RaggedExperiment with 59450 rows and 586 columns 
 [4] CESC_GISTIC_AllByGene-20160128: SummarizedExperiment with 24776 rows and 295 columns 
 [5] CESC_GISTIC_Peaks-20160128: RangedSummarizedExperiment with 60 rows and 295 columns 
 [6] CESC_GISTIC_ThresholdedByGene-20160128: SummarizedExperiment with 24776 rows and 295 columns 
 [7] CESC_miRNASeqGene-20160128: SummarizedExperiment with 1046 rows and 312 columns 
 [8] CESC_Mutation-20160128: RaggedExperiment with 46547 rows and 194 columns 
 [9] CESC_RNASeq2GeneNorm-20160128: SummarizedExperiment with 20501 rows and 309 columns 
 [10] CESC_RPPAArray-20160128: SummarizedExperiment with 192 rows and 173 columns 
 [11] CESC_Methylation-20160128: SummarizedExperiment with 485577 rows and 312 columns 

> rownames( CESC )
CharacterList of length 11
[["CESC_CNASeq-20160128"]] character(0)
[["CESC_CNASNP-20160128"]] character(0)
[["CESC_CNVSNP-20160128"]] character(0)
[["CESC_GISTIC_AllByGene-20160128"]] ACAP3 ... WASIR1|ENSG00000185203.7
[["CESC_GISTIC_Peaks-20160128"]] chr1:1-28840301 ... chr22:48668582-51304566
[["CESC_GISTIC_ThresholdedByGene-20160128"]] ACAP3 ...
[["CESC_miRNASeqGene-20160128"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99b
[["CESC_Mutation-20160128"]] character(0)
[["CESC_RNASeq2GeneNorm-20160128"]] A1BG A1CF A2BP1 ... ZZZ3 psiTPTE22 tAKR
[["CESC_RPPAArray-20160128"]] 14-3-3_beta ... p90RSK_pT359_S363
...
<1 more element>

> colnames( CESC )
CharacterList of length 11
[["CESC_CNASeq-20160128"]] TCGA-C5-A1BQ-01C-11D-A20X-26 ...
[["CESC_CNASNP-20160128"]] TCGA-2W-A8YY-01A-11D-A37M-01 ...
[["CESC_CNVSNP-20160128"]] TCGA-2W-A8YY-01A-11D-A37M-01 ...
[["CESC_GISTIC_AllByGene-20160128"]] TCGA-2W-A8YY-01A-11D-A37M-01 ...
[["CESC_GISTIC_Peaks-20160128"]] TCGA-2W-A8YY-01A-11D-A37M-01 ...
[["CESC_GISTIC_ThresholdedByGene-20160128"]] TCGA-2W-A8YY-01A-11D-A37M-01 ...
[["CESC_miRNASeqGene-20160128"]] TCGA-2W-A8YY-01A-11R-A37P-13 ...
[["CESC_Mutation-20160128"]] TCGA-BI-A0VR-01A-11D-A10S-08 ...
[["CESC_RNASeq2GeneNorm-20160128"]] TCGA-2W-A8YY-01A-11R-A37O-07 ...
[["CESC_RPPAArray-20160128"]] TCGA-2W-A8YY-01A-21-A40H-20 ...
...
<1 more element>

Sizes of each ExperimentList element:

                                    assay size.Mb
1                    CESC_CNASeq-20160128  0.3 Mb
2                    CESC_CNASNP-20160128  7.3 Mb
3                    CESC_CNVSNP-20160128  1.8 Mb
4          CESC_GISTIC_AllByGene-20160128  4.9 Mb
5              CESC_GISTIC_Peaks-20160128  0.1 Mb
6  CESC_GISTIC_ThresholdedByGene-20160128  4.9 Mb
7              CESC_miRNASeqGene-20160128  0.1 Mb
8                  CESC_Mutation-20160128 32.9 Mb
9           CESC_RNASeq2GeneNorm-20160128  1.3 Mb
10                CESC_RPPAArray-20160128    0 Mb
11              CESC_Methylation-20160128   75 Mb

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~ 
    -1)

   235 observations deleted due to missingness 
      n  events  median 0.95LCL 0.95UCL 
  72.00   72.00    1.70    1.39    2.29 


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   20.0    38.0    46.0    48.2    56.0    88.0       2 

vital_status:
  0   1 
235  72 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   14.0   349.5   620.0   881.3  1069.5  4086.0     235 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      0     374     688    1071    1362    6408      72 

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

tobacco_smoking_history:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.829   2.000   5.000      44 

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


Including an additional 1330 columns
}}
\keyword{datasets}
