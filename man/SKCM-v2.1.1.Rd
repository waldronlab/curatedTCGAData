\name{SKCM-v2.1.1}
\alias{ SKCM-v2.1.1 }
\docType{data}
\title{ Skin Cutaneous Melanoma }
\description{
A document describing the TCGA cancer code
Note. Only the \code{colData} has changed.
}

\details{
\preformatted{
> experiments( SKCM )
ExperimentList class object of length 0:

> rownames( SKCM )
CharacterList of length 0

> colnames( SKCM )
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
[1,] 221    221   2.99     2.4     3.9


---------------------------
Available sample meta-data:
---------------------------

years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
  15.00   48.00   58.00   58.24   71.00   90.00       8

vital_status:
  0   1
247 223

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
     79     518    1093    1789    2073   10870     249

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
   -2.0   477.5  1146.0  1885.3  2658.8 11252.0     230

days_to_submitted_specimen_dx:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
     -2       0     344    1017    1372   10847      14





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

Including an additional 1517 columns
}}
\keyword{datasets}
