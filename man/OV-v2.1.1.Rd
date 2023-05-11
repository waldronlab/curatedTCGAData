\name{OV-v2.1.1}
\alias{ OV-v2.1.1 }
\docType{data}
\title{ Ovarian serous cystadenocarcinoma }
\description{
A document describing the TCGA cancer code
Note. Only the \code{colData} has changed.
}

\details{
\preformatted{
> experiments( OV )
ExperimentList class object of length 0:

> rownames( OV )
CharacterList of length 0

> colnames( OV )
CharacterList of length 0

Sizes of each ExperimentList element:

[1] assay   size.Mb
<0 rows> (or 0-length row.names)

---------------------------
Overall survival time-to-event summary (in years):
---------------------------

Call: survfit(formula = survival::Surv(colDat$days_to_death/365, colDat$vital_status) ~
    -1)

   249 observations deleted due to missingness
       n events median 0.95LCL 0.95UCL
[1,] 343    343   2.94    2.76    3.17


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
  26.00   51.00   59.00   59.78   68.75   89.00      22

vital_status:
   0    1 NA's
 247  344    1

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
    8.0   567.5  1073.0  1147.4  1557.0  4624.0     249

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
     16     268     837    1214    1919    5481     359

tumor_tissue_site:
         omentum            ovary peritoneum ovary             NA's
               3              576                2               11

gender:
female   NA's
   581     11

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
   1992    2001    2004    2004    2007    2013      11

radiation_therapy:
  no  yes NA's
 557    5   30

karnofsky_performance_score:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
  40.00   60.00   80.00   75.83   80.00  100.00     508

histological_type:
serous cystadenocarcinoma                      NA's
                      581                        11

residual_tumor:
  r0   r1   r2   rx NA's
  16   31    5    3  537

ethnicity:
    hispanic or latino not hispanic or latino                   NA's
                    11                    338                    243

Including an additional 2850 columns
}}
\keyword{datasets}
