\name{ UCEC }
\alias{ UCEC }
\docType{data}
\title{ UCEC }

\details{
\preformatted{
> UCEC
A MultiAssayExperiment object of 11 listed
 experiments with user-defined names and respective classes. 
 Containing an ExperimentList class object of length 11: 
 [1] RNASeqGene: ExpressionSet with 20502 rows and 269 columns 
 [2] RNASeq2GeneNorm: ExpressionSet with 20501 rows and 380 columns 
 [3] miRNASeqGene: ExpressionSet with 1046 rows and 433 columns 
 [4] CNASNP: RangedRaggedAssay with 614636 rows and 1083 columns 
 [5] CNVSNP: RangedRaggedAssay with 126399 rows and 1078 columns 
 [6] CNAseq: RangedRaggedAssay with 36400 rows and 213 columns 
 [7] Methylation: SummarizedExperiment with 485577 rows and 466 columns 
 [8] mRNAArray: ExpressionSet with 17814 rows and 54 columns 
 [9] RPPAArray: ExpressionSet with 208 rows and 440 columns 
 [10] gistica: SummarizedExperiment with 24776 rows and 539 columns 
 [11] gistict: SummarizedExperiment with 24776 rows and 539 columns 
To access: 
 experiments() - to obtain the ExperimentList instance 
 pData() - for the primary/phenotype DataFrame 
 sampleMap() - for the sample availability DataFrame 
 metadata() - for the metadata object of ANY class 
See also: subsetByAssay(), subsetByRow(), subsetByColumn()

> rownames( UCEC )
CharacterList of length 11
[["RNASeqGene"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZYX ZZEF1 ZZZ3 psiTPTE22 tAKR
[["RNASeq2GeneNorm"]] A1BG A1CF A2BP1 A2LD1 A2ML1 ... ZZEF1 ZZZ3 psiTPTE22 tAKR
[["miRNASeqGene"]] hsa-let-7a-1 hsa-let-7a-2 ... hsa-mir-99a hsa-mir-99b
[["CNASNP"]] 1:61735-10246654 1:10249962-10249977 ... 24:28438116-59018259
[["CNVSNP"]] 1:3218610-14596162 1:14596892-16012024 ... 23:32651365-154905589
[["CNAseq"]] 1:10208-8431279 1:8431280-8682544 ... 24:2649474-28783838
[["Methylation"]] cg00000029 cg00000108 cg00000109 ... rs966367 rs9839873
[["mRNAArray"]] ELMO2 CREB3L1 RPS11 PNMA1 MMP2 ... SLC39A6 SNRPD2 AQP7 CTSC
[["RPPAArray"]] 14-3-3_beta 14-3-3_epsilon ... p90RSK p90RSK_pT359_S363
[["gistica"]] ACAP3 ACTRT2 ... WASIR1|ENSG00000185203.7
...
<1 more element>

> colnames( UCEC )
CharacterList of length 11
[["RNASeqGene"]] TCGA-A5-A0G1-01A-11R-A118-07 ... TCGA-EY-A212-01A-11R-A14M-07
[["RNASeq2GeneNorm"]] TCGA-A5-A0G1-01A-11R-A118-07 ...
[["miRNASeqGene"]] TCGA-2E-A9G8-01A-11R-A404-13 ...
[["CNASNP"]] TCGA-2E-A9G8-01A-11D-A402-01 ... TCGA-SL-A6JA-10A-01D-A31T-01
[["CNVSNP"]] TCGA-2E-A9G8-01A-11D-A402-01 ... TCGA-SL-A6JA-10A-01D-A31T-01
[["CNAseq"]] TCGA-A5-A0G5-01A-11D-A043-02 ... TCGA-FI-A2D4-10A-01D-A17E-02
[["Methylation"]] TCGA-2E-A9G8-01A-11D-A409-05 ... TCGA-SL-A6JA-01A-11D-A31V-05
[["mRNAArray"]] TCGA-A5-A0G2-01A-11R-A040-07 ... TCGA-BK-A0CC-01A-21R-A00V-07
[["RPPAArray"]] TCGA-4E-A92E-01A-21-A45J-20 ... TCGA-SL-A6JA-01A-21-A45J-20
[["gistica"]] TCGA-2E-A9G8-01A-11D-A402-01 ... TCGA-SL-A6JA-01A-11D-A31T-01
...
<1 more element>

Sizes of each ExperimentList element:

             assay size.Mb
1       RNASeqGene  1.2 Mb
2  RNASeq2GeneNorm  1.2 Mb
3     miRNASeqGene  0.1 Mb
4           CNASNP   47 Mb
5           CNVSNP 11.4 Mb
6           CNAseq  2.3 Mb
7      Methylation 67.5 Mb
8        mRNAArray    1 Mb
9        RPPAArray  0.1 Mb
10         gistica  4.4 Mb
11         gistict  4.4 Mb


---------------------------
Available sample meta-data:
---------------------------


years_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  31.00   57.00   64.00   63.94   71.00   90.00       4 

vital_status.x:
  0   1 
458  90 

days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   50.0   364.0   711.5   885.6  1072.0  3423.0     458 

days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -6.0   491.5   902.5  1129.3  1600.2  6859.0      46 

tumor_tissue_site:
   endometrial other  specify 
           547              1 

gender:
female 
   548 

date_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1995    2007    2009    2009    2010    2013       9 

days_to_last_known_alive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    145     404     729    1405    2096    4144     535 

radiation_therapy:
  no  yes NA's 
 295  228   25 

histological_type:
endometrioid endometrial adenocarcinoma           mixed serous and endometrioid 
                                    411                                      22 
      serous endometrial adenocarcinoma 
                                    115 

residual_tumor:
  r0   r1   r2   rx NA's 
 376   22   16   41   93 


ethnicity.x:
    hispanic or latino not hispanic or latino                   NA's 
                    15                    377                    156 

admin.bcr:
nationwide children's hospital 
                           548 

admin.day_of_dcc_upload:
  1 
548 

admin.disease_code:
ucec 
 548 


admin.month_of_dcc_upload:
 10 
548 

admin.patient_withdrawal.withdrawn:
false 
  548 

admin.project_code:
tcga 
 548 

admin.year_of_dcc_upload:
2015 
 548 

patient.additional_studies.additional_study.disease_code:
fppp NA's 
   4  544 

patient.additional_studies.additional_study.project_code:
tcga NA's 
   4  544 

patient.age_at_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  31.00   57.00   64.00   63.93   71.00   90.00       3 

patient.aln_pos_ihc:
   0    2 NA's 
 204    1  343 

patient.aln_pos_light_micro:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.4031  0.0000 29.0000     228 


patient.birth_control_pill_history_usage_category:
former user  never used        NA's 
          7          21         520 

patient.clinical_cqcf.anatomic_neoplasm_subdivision:
                         endometrium                         fundus uteri 
                                 272                                    6 
lower uterine segment/ isthmus uteri                           myometrium 
                                   3                                    1 
                                NA's 
                                 266 

patient.clinical_cqcf.consent_or_death_status:
consented  deceased 
      542         6 

patient.clinical_cqcf.country:
       canada        russia       ukraine united states          NA's 
           35             2             2           281           228 

patient.clinical_cqcf.days_to_consent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -731.0     9.0    20.0   116.3    42.0  5319.0      15 

patient.clinical_cqcf.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  189.0   287.8   416.5   590.5   823.5  1317.0     542 


patient.clinical_cqcf.history_of_neoadjuvant_treatment:
  no NA's 
 547    1 


patient.clinical_cqcf.normal_tissue_proximity:
distal (>2cm)          NA's 
           54           494 

patient.clinical_cqcf.tumor_type:
primary 
    548 

patient.colorectal_cancer:
  no  yes NA's 
  51    1  496 

patient.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    9.00   14.00   15.14   23.00   31.00 

patient.days_to_birth:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -32873  -26186  -23448  -23526  -20964  -11635       4 

patient.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   50.0   326.0   590.0   737.4   946.0  3251.0     503 

patient.days_to_initial_pathologic_diagnosis:
   0 NA's 
 542    6 

patient.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  -13.0   179.8   408.5   705.3  1004.8  5691.0      46 

patient.days_to_last_known_alive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  145.0   426.5   863.0  1557.8  2589.5  4144.0     537 

patient.diabetes:
  no  yes NA's 
  31   15  502 

patient.drugs.drug.10.bcr_drug_barcode:
tcga-bs-a0t9-d9201               NA's 
                 1                547 

patient.drugs.drug.10.bcr_drug_uuid:
67441c30-fdaf-49e6-aad8-e4d349c349ec                                 NA's 
                                   1                                  547 

patient.drugs.drug.10.day_of_form_completion:
  19 NA's 
   1  547 

patient.drugs.drug.10.days_to_drug_therapy_end:
1250 NA's 
   1  547 

patient.drugs.drug.10.days_to_drug_therapy_start:
1163 NA's 
   1  547 

patient.drugs.drug.10.drug_name:
gemzar   NA's 
     1    547 

patient.drugs.drug.10.month_of_form_completion:
  11 NA's 
   1  547 

patient.drugs.drug.10.number_cycles:
   9 NA's 
   1  547 

patient.drugs.drug.10.prescribed_dose:
1000 NA's 
   1  547 

patient.drugs.drug.10.prescribed_dose_units:
mg/m2  NA's 
    1   547 

patient.drugs.drug.10.regimen_indication:
progression        NA's 
          1         547 

patient.drugs.drug.10.regimen_number:
   4 NA's 
   1  547 

patient.drugs.drug.10.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              547 

patient.drugs.drug.10.therapy_ongoing:
  no NA's 
   1  547 

patient.drugs.drug.10.therapy_types.therapy_type:
chemotherapy         NA's 
           1          547 

patient.drugs.drug.10.total_dose:
9000 NA's 
   1  547 

patient.drugs.drug.10.total_dose_units:
mg/m2  NA's 
    1   547 

patient.drugs.drug.10.year_of_form_completion:
2010 NA's 
   1  547 

patient.drugs.drug.11.bcr_drug_barcode:
tcga-bs-a0t9-d9204               NA's 
                 1                547 

patient.drugs.drug.11.bcr_drug_uuid:
206c8c36-2556-4487-9710-60417cc2c5d3                                 NA's 
                                   1                                  547 

patient.drugs.drug.11.day_of_form_completion:
  19 NA's 
   1  547 

patient.drugs.drug.11.days_to_drug_therapy_end:
1379 NA's 
   1  547 

patient.drugs.drug.11.days_to_drug_therapy_start:
1309 NA's 
   1  547 

patient.drugs.drug.11.drug_name:
taxol  NA's 
    1   547 

patient.drugs.drug.11.month_of_form_completion:
  11 NA's 
   1  547 

patient.drugs.drug.11.number_cycles:
   8 NA's 
   1  547 

patient.drugs.drug.11.prescribed_dose:
  80 NA's 
   1  547 

patient.drugs.drug.11.prescribed_dose_units:
mg/m2  NA's 
    1   547 

patient.drugs.drug.11.regimen_indication:
progression        NA's 
          1         547 

patient.drugs.drug.11.regimen_number:
   5 NA's 
   1  547 

patient.drugs.drug.11.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              547 

patient.drugs.drug.11.therapy_ongoing:
  no NA's 
   1  547 

patient.drugs.drug.11.therapy_types.therapy_type:
chemotherapy         NA's 
           1          547 

patient.drugs.drug.11.total_dose:
 640 NA's 
   1  547 

patient.drugs.drug.11.total_dose_units:
mg/m2  NA's 
    1   547 

patient.drugs.drug.11.year_of_form_completion:
2010 NA's 
   1  547 



patient.drugs.drug.2.clinical_trail_drug_classification:
adjuvent     NA's 
       5      543 

patient.drugs.drug.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   17.00   16.13   23.00   31.00     379 

patient.drugs.drug.2.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   48.0   149.5   186.0   272.8   247.0  2566.0     404 

patient.drugs.drug.2.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   18.0    55.0    74.0   175.0   118.2  2419.0     388 


patient.drugs.drug.2.measure_of_response:
clinical progressive disease            complete response 
                           8                           44 
            partial response               stable disease 
                           1                            1 
                        NA's 
                         494 

patient.drugs.drug.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   6.373   9.000  12.000     379 

patient.drugs.drug.2.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.750   6.000   5.318   6.000  12.000     460 



patient.drugs.drug.2.regimen_indication:
               adjuvant other: specify in notes             progression 
                     81                       3                       3 
             recurrence                    NA's 
                      9                     452 

patient.drugs.drug.2.regimen_indication_notes:
                   mets to lung and brain 
                                        1 
        pt. request of less harsh therapy 
                                        1 
refractory neutropenia & thrompocytopenia 
                                        1 
                                     NA's 
                                      545 

patient.drugs.drug.2.regimen_number:
   1    2    3 NA's 
  81    9    2  456 

patient.drugs.drug.2.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
              85                3              460 

patient.drugs.drug.2.therapy_ongoing:
  no  yes NA's 
 162    6  380 

patient.drugs.drug.2.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                       160                          4 
targeted molecular therapy                       NA's 
                         1                        383 

patient.drugs.drug.2.therapy_types.therapy_type.2:
targeted molecular therapy                       NA's 
                         1                        547 

patient.drugs.drug.2.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     96    1372    2106    2570    2958   16560     481 

patient.drugs.drug.2.total_dose_units:
[iu]/mg      mg   mg/m2    NA's 
      1      61       4     482 

patient.drugs.drug.2.tx_on_clinical_trial:
  no  yes NA's 
  60    9  479 

patient.drugs.drug.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2015     379 



patient.drugs.drug.3.clinical_trail_drug_classification:
adjuvent     NA's 
       4      544 

patient.drugs.drug.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    10.0    18.5    16.6    23.0    31.0     488 

patient.drugs.drug.3.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   71.0   169.5   231.5   419.5   402.0  2566.0     500 

patient.drugs.drug.3.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   69.25  136.00  375.57  551.50 2419.00     490 


patient.drugs.drug.3.measure_of_response:
clinical progressive disease            complete response 
                           4                           13 
            partial response                         NA's 
                           1                          530 

patient.drugs.drug.3.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     4.0     6.0     6.3     9.0    12.0     488 

patient.drugs.drug.3.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.750   5.000   4.821   6.000   8.000     520 

patient.drugs.drug.3.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.8    40.0    80.0   190.1   175.0  1600.0     519 


patient.drugs.drug.3.regimen_indication:
               adjuvant other: specify in notes             progression 
                     14                       3                       7 
             recurrence                    NA's 
                      7                     517 

patient.drugs.drug.3.regimen_indication_notes:
mets to lung and brain             persistent         persistent lad 
                     1                      1                      1 
                  NA's 
                   545 

patient.drugs.drug.3.regimen_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   1.000   1.821   2.000   5.000     520 

patient.drugs.drug.3.route_of_administrations.route_of_administration:
intravenous (iv)             oral             NA's 
              28                1              519 

patient.drugs.drug.3.therapy_ongoing:
  no  yes NA's 
  49   10  489 

patient.drugs.drug.3.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                        53                          2 
targeted molecular therapy                       NA's 
                         2                        491 

patient.drugs.drug.3.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   12.0   402.5  1012.0  1447.4  1987.5  6400.0     528 

patient.drugs.drug.3.total_dose_units:
   mg mg/kg mg/m2  NA's 
   18     1     1   528 

patient.drugs.drug.3.tx_on_clinical_trial:
  no  yes NA's 
  21    8  519 

patient.drugs.drug.3.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2015     488 



patient.drugs.drug.4.clinical_trail_drug_classification:
adjuvent     NA's 
       2      546 

patient.drugs.drug.4.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   19.00   16.88   25.00   31.00     523 

patient.drugs.drug.4.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   67.0   150.5   428.0   636.5   841.0  2830.0     529 

patient.drugs.drug.4.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   18.0    87.0   373.0   523.0   580.5  2825.0     525 


patient.drugs.drug.4.measure_of_response:
clinical progressive disease                         NA's 
                           3                          545 

patient.drugs.drug.4.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    3.0     5.0     7.0     7.8    11.0    12.0     523 

patient.drugs.drug.4.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.533   7.000  29.000     533 

patient.drugs.drug.4.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   41.25   88.00  144.71  175.00  550.00     534 

patient.drugs.drug.4.prescribed_dose_units:
  auc    mg mg/m2  NA's 
    1     5     8   534 

patient.drugs.drug.4.regimen_indication:
               adjuvant other: specify in notes             progression 
                      5                       1                       2 
             recurrence                    NA's 
                      8                     532 

patient.drugs.drug.4.regimen_indication_notes:
mets to lung and brain                   NA's 
                     1                    547 

patient.drugs.drug.4.regimen_number:
   1    2    3 NA's 
   6    7    2  533 

patient.drugs.drug.4.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
              15              533 

patient.drugs.drug.4.therapy_ongoing:
  no  yes NA's 
  20    5  523 

patient.drugs.drug.4.therapy_types.therapy_type:
chemotherapy         NA's 
          24          524 

patient.drugs.drug.4.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     12     285     480    2103    1503   10300     537 

patient.drugs.drug.4.total_dose_units:
   mg mg/m2  NA's 
    8     3   537 

patient.drugs.drug.4.tx_on_clinical_trial:
  no  yes NA's 
   6    3  539 

patient.drugs.drug.4.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2012    2012    2015     523 



patient.drugs.drug.5.clinical_trail_drug_classification:
adjuvent     NA's 
       1      547 

patient.drugs.drug.5.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   13.25   18.00   17.08   22.00   29.00     536 

patient.drugs.drug.5.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  104.0   159.5   267.0   802.7  1062.5  2985.0     537 

patient.drugs.drug.5.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   18.0    54.0   218.0   708.1   988.5  2916.0     537 


patient.drugs.drug.5.measure_of_response:
clinical progressive disease                         NA's 
                           3                          545 

patient.drugs.drug.5.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   4.750   6.000   7.333  11.000  12.000     536 

patient.drugs.drug.5.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   3.000   5.000   4.714   6.000   8.000     541 

patient.drugs.drug.5.prescribed_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    5.0    60.0    80.0   148.6   142.5   550.0     541 

patient.drugs.drug.5.prescribed_dose_units:
  auc    mg mg/m2  NA's 
    1     3     3   541 

patient.drugs.drug.5.regimen_indication:
               adjuvant other: specify in notes             progression 
                      4                       1                       1 
             recurrence                    NA's 
                      2                     540 

patient.drugs.drug.5.regimen_indication_notes:
mets to lung and brain                   NA's 
                     1                    547 

patient.drugs.drug.5.regimen_number:
   1    2    3 NA's 
   3    3    1  541 

patient.drugs.drug.5.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               7              541 

patient.drugs.drug.5.therapy_ongoing:
  no  yes NA's 
  11    1  536 

patient.drugs.drug.5.therapy_types.therapy_type:
chemotherapy         NA's 
          12          536 

patient.drugs.drug.5.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    240     575    1285    1334    2051    2550     542 

patient.drugs.drug.5.total_dose_units:
   mg mg/m2  NA's 
    4     2   542 

patient.drugs.drug.5.tx_on_clinical_trial:
  no  yes NA's 
   3    1  544 

patient.drugs.drug.5.year_of_form_completion:
2010 2011 2012 2015 NA's 
   2    5    4    1  536 



patient.drugs.drug.6.clinical_trail_drug_classification:
adjuvant adjuvent     NA's 
       1        1      546 

patient.drugs.drug.6.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   15.25   20.00   20.17   25.50   29.00     542 

patient.drugs.drug.6.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    152     185    1379    1247    1431    3090     543 

patient.drugs.drug.6.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   13.0   106.2   730.5   995.8  1354.0  3041.0     542 

patient.drugs.drug.6.drug_name:
carboplatin  paclitaxel   topotecan        NA's 
          3           1           2         542 

patient.drugs.drug.6.measure_of_response:
clinical progressive disease                         NA's 
                           2                          546 

patient.drugs.drug.6.month_of_form_completion:
   5    7   11 NA's 
   2    2    2  542 

patient.drugs.drug.6.number_cycles:
   5    8 NA's 
   1    1  546 

patient.drugs.drug.6.prescribed_dose:
   6  167 NA's 
   1    1  546 

patient.drugs.drug.6.prescribed_dose_units:
 auc   mg NA's 
   1    1  546 

patient.drugs.drug.6.regimen_indication:
   adjuvant progression        NA's 
          1           1         546 

patient.drugs.drug.6.regimen_number:
   2    5 NA's 
   1    1  546 

patient.drugs.drug.6.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               2              546 

patient.drugs.drug.6.therapy_ongoing:
  no  yes NA's 
   5    1  542 

patient.drugs.drug.6.therapy_types.therapy_type:
chemotherapy         NA's 
           6          542 

patient.drugs.drug.6.total_dose:
1336 2730 NA's 
   1    1  546 

patient.drugs.drug.6.total_dose_units:
  mg NA's 
   2  546 

patient.drugs.drug.6.tx_on_clinical_trial:
  no  yes NA's 
   2    2  544 

patient.drugs.drug.6.year_of_form_completion:
2010 2011 2012 2015 NA's 
   1    1    2    2  542 

patient.drugs.drug.7.bcr_drug_barcode:
 tcga-bs-a0t9-d9199 tcga-d1-a16g-d18571                NA's 
                  1                   1                 546 

patient.drugs.drug.7.bcr_drug_uuid:
a18b297e-2a43-4df5-abc9-8a21110da904 fb52775e-0e9b-45ca-b849-fba324fb71e9 
                                   1                                    1 
                                NA's 
                                 546 

patient.drugs.drug.7.day_of_form_completion:
  19   26 NA's 
   1    1  546 

patient.drugs.drug.7.days_to_drug_therapy_end:
 756 1243 NA's 
   1    1  546 

patient.drugs.drug.7.days_to_drug_therapy_start:
 651 1072 NA's 
   1    1  546 

patient.drugs.drug.7.drug_name:
carboplatin gemcitabine        NA's 
          1           1         546 

patient.drugs.drug.7.month_of_form_completion:
  11 NA's 
   2  546 

patient.drugs.drug.7.number_cycles:
   4    6 NA's 
   1    1  546 

patient.drugs.drug.7.prescribed_dose:
   5 2000 NA's 
   1    1  546 

patient.drugs.drug.7.prescribed_dose_units:
  auc mg/m2  NA's 
    1     1   546 

patient.drugs.drug.7.regimen_indication:
progression  recurrence        NA's 
          1           1         546 

patient.drugs.drug.7.regimen_number:
   3 NA's 
   2  546 

patient.drugs.drug.7.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               2              546 

patient.drugs.drug.7.therapy_ongoing:
  no NA's 
   2  546 

patient.drugs.drug.7.therapy_types.therapy_type:
chemotherapy         NA's 
           2          546 

patient.drugs.drug.7.total_dose:
2770 8000 NA's 
   1    1  546 

patient.drugs.drug.7.total_dose_units:
   mg mg/m2  NA's 
    1     1   546 

patient.drugs.drug.7.year_of_form_completion:
2010 2011 NA's 
   1    1  546 

patient.drugs.drug.8.bcr_drug_barcode:
tcga-bs-a0t9-d9200               NA's 
                 1                547 

patient.drugs.drug.8.bcr_drug_uuid:
48905b00-83fe-4cf2-9a40-d8feef537236                                 NA's 
                                   1                                  547 

patient.drugs.drug.8.day_of_form_completion:
  19 NA's 
   1  547 

patient.drugs.drug.8.days_to_drug_therapy_end:
1243 NA's 
   1  547 

patient.drugs.drug.8.days_to_drug_therapy_start:
1156 NA's 
   1  547 

patient.drugs.drug.8.drug_name:
cisplatin      NA's 
        1       547 

patient.drugs.drug.8.month_of_form_completion:
  11 NA's 
   1  547 

patient.drugs.drug.8.number_cycles:
   6 NA's 
   1  547 

patient.drugs.drug.8.prescribed_dose:
  50 NA's 
   1  547 

patient.drugs.drug.8.prescribed_dose_units:
mg/m2  NA's 
    1   547 

patient.drugs.drug.8.regimen_indication:
progression        NA's 
          1         547 

patient.drugs.drug.8.regimen_number:
   4 NA's 
   1  547 

patient.drugs.drug.8.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              547 

patient.drugs.drug.8.therapy_ongoing:
  no NA's 
   1  547 

patient.drugs.drug.8.therapy_types.therapy_type:
chemotherapy         NA's 
           1          547 

patient.drugs.drug.8.total_dose:
 300 NA's 
   1  547 

patient.drugs.drug.8.total_dose_units:
mg/m2  NA's 
    1   547 

patient.drugs.drug.8.year_of_form_completion:
2010 NA's 
   1  547 

patient.drugs.drug.9.bcr_drug_barcode:
tcga-bs-a0t9-d9198               NA's 
                 1                547 

patient.drugs.drug.9.bcr_drug_uuid:
54c38de3-026d-4cec-9c6a-d9e4c807ea68                                 NA's 
                                   1                                  547 

patient.drugs.drug.9.day_of_form_completion:
  19 NA's 
   1  547 

patient.drugs.drug.9.days_to_drug_therapy_end:
1243 NA's 
   1  547 

patient.drugs.drug.9.days_to_drug_therapy_start:
1072 NA's 
   1  547 

patient.drugs.drug.9.drug_name:
cisplatin      NA's 
        1       547 

patient.drugs.drug.9.month_of_form_completion:
  11 NA's 
   1  547 

patient.drugs.drug.9.number_cycles:
   9 NA's 
   1  547 

patient.drugs.drug.9.prescribed_dose:
  50 NA's 
   1  547 

patient.drugs.drug.9.prescribed_dose_units:
mg/m2  NA's 
    1   547 

patient.drugs.drug.9.regimen_indication:
progression        NA's 
          1         547 

patient.drugs.drug.9.regimen_number:
   3 NA's 
   1  547 

patient.drugs.drug.9.route_of_administrations.route_of_administration:
intravenous (iv)             NA's 
               1              547 

patient.drugs.drug.9.therapy_ongoing:
  no NA's 
   1  547 

patient.drugs.drug.9.therapy_types.therapy_type:
chemotherapy         NA's 
           1          547 

patient.drugs.drug.9.total_dose:
 200 NA's 
   1  547 

patient.drugs.drug.9.total_dose_units:
mg/m2  NA's 
    1   547 

patient.drugs.drug.9.year_of_form_completion:
2010 NA's 
   1  547 



patient.drugs.drug.clinical_trail_drug_classification:
radiation alone            NA's 
              1             547 

patient.drugs.drug.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   17.00   15.69   23.00   31.00     347 

patient.drugs.drug.days_to_drug_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   49.0   151.0   189.0   287.6   250.5  3411.0     381 

patient.drugs.drug.days_to_drug_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  25.00   59.75   80.00  193.97  121.50 2419.00     360 


patient.drugs.drug.measure_of_response:
clinical progressive disease            complete response 
                           7                           53 
            partial response               stable disease 
                           2                            3 
                        NA's 
                         483 

patient.drugs.drug.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   6.154   9.000  12.000     347 

patient.drugs.drug.number_cycles:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   4.654   6.000  12.000     447 



patient.drugs.drug.regimen_indication:
               adjuvant other: specify in notes             progression 
                    102                       2                       2 
             recurrence                    NA's 
                     15                     427 

patient.drugs.drug.regimen_indication_notes:
mets to lung and brain             persistent                   NA's 
                     1                      1                    546 

patient.drugs.drug.regimen_number:
   1    2    3    5 NA's 
 105   10    1    1  431 

patient.drugs.drug.route_of_administrations.route_of_administration:
intramuscular (im)   intravenous (iv)               oral               NA's 
                 1                 89                 13                445 

patient.drugs.drug.therapy_ongoing:
  no  yes NA's 
 189    6  353 

patient.drugs.drug.therapy_types.therapy_type:
              chemotherapy            hormone therapy 
                       178                         15 
            other, specify targeted molecular therapy 
                         1                          1 
                      NA's 
                       353 

patient.drugs.drug.therapy_types.therapy_type_notes:
investigational new drug                     NA's 
                       1                      547 

patient.drugs.drug.total_dose:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    120     945    1698    2782    2276   53440     474 

patient.drugs.drug.total_dose_units:
[iu]/mg      mg  mg/day   mg/m2    NA's 
      1      71       1       2     473 

patient.drugs.drug.tx_on_clinical_trial:
  no  yes NA's 
  70    5  473 

patient.drugs.drug.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2005    2011    2011    2012    2013    2015     347 

patient.ethnicity:
    hispanic or latino not hispanic or latino                   NA's 
                    15                    377                    156 

patient.follow_ups.follow_up.2.additional_pharmaceutical_therapy:
 yes NA's 
   2  546 

patient.follow_ups.follow_up.2.additional_radiation_therapy:
  no NA's 
   2  546 

patient.follow_ups.follow_up.2.additional_surgery_locoregional_procedure:
 yes NA's 
   2  546 

patient.follow_ups.follow_up.2.additional_treatment_completion_success_outcome:
  complete response progressive disease      stable disease                NA's 
                153                  22                   5                 368 



patient.follow_ups.follow_up.2.birth_control_pill_history_usage_category:
former user  never used        NA's 
         19          51         478 

patient.follow_ups.follow_up.2.colorectal_cancer:
  no NA's 
 102  446 

patient.follow_ups.follow_up.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     6.0    19.0    16.9    27.0    31.0     341 

patient.follow_ups.follow_up.2.days_to_additional_surgery_metastatic_procedure:
 150  704 NA's 
   1    1  546 

patient.follow_ups.follow_up.2.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  326.0   684.0   963.5  1179.4  1179.5  3423.0     526 

patient.follow_ups.follow_up.2.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  327.0   973.5  1497.0  1652.4  2194.5  6859.0     365 

patient.follow_ups.follow_up.2.days_to_new_tumor_event_after_initial_treatment:
 529  705 NA's 
   1    1  546 

patient.follow_ups.follow_up.2.diabetes:
  no  yes NA's 
  67   25  456 

patient.follow_ups.follow_up.2.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             2                             30 
                          NA's 
                           516 

patient.follow_ups.follow_up.2.horm_ther:
                         no, i have never taken menopausal hormone therapy. 
                                                                         70 
yes, i have taken menopausal hormone therapy in the past, but no longer do. 
                                                                         16 
                                                                       NA's 
                                                                        462 

patient.follow_ups.follow_up.2.hypertension:
  no  yes NA's 
  43   60  445 

patient.follow_ups.follow_up.2.lost_follow_up:
  no  yes NA's 
 139   14  395 

patient.follow_ups.follow_up.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   6.266  10.000  12.000     341 

patient.follow_ups.follow_up.2.new_neoplasm_event_occurrence_anatomic_site:
          lung other, specify           NA's 
             1              1            546 

patient.follow_ups.follow_up.2.new_neoplasm_event_type:
locoregional disease           metastatic                 NA's 
                   1                    1                  546 

patient.follow_ups.follow_up.2.new_neoplasm_occurrence_anatomic_site_text:
diffuse carcinomatosis                   NA's 
                     1                    547 

patient.follow_ups.follow_up.2.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  52    2  494 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
 yes NA's 
   1  547 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
   1    1  546 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 413  743  767 NA's 
   1    1    1  545 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
          lung other, specify           NA's 
             1              2            545 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      2                       1                     545 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
abdomen- multiple sites of metastatic disease 
                                            1 
                                   pelvic lns 
                                            1 
                                         NA's 
                                          546 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no NA's 
   3  545 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.additional_radiation_therapy:
 yes NA's 
   1  547 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.days_to_new_tumor_event_after_initial_treatment:
 575  743 NA's 
   1    1  546 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.new_neoplasm_event_occurrence_anatomic_site:
          bone other, specify           NA's 
             1              1            546 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      1                       1                     546 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.new_neoplasm_occurrence_anatomic_site_text:
vaginal apex         NA's 
           1          547 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.3.new_tumor_event_additional_surgery_procedure:
  no NA's 
   2  546 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   8    9  531 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  11    4  533 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
 517  810 1178 2111 NA's 
   1    1    1    1  544 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   70.0   613.0   804.0   941.2  1105.0  2277.0     527 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
         brain          liver           lung other, specify           NA's 
             1              1              4             14            528 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                      5                      12                       4 
                   NA's 
                    527 


patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  12    5  531 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.recurrence_second_surgery_neoplasm_surgical_procedure_name:
          excisional biopsy other method, specify below 
                          1                           4 
         surgical resection                        NA's 
                          3                         540 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.recurrence_second_surgery_neoplasm_surgical_procedure_name_other:
                                                                            ct guided fine needle aspiration 
                                                                                                           1 
                                                                                               thoracentesis 
                                                                                                           1 
total pelvis exenteration, mobilation of the splenic fexure, and sigmoid colostomy, ileal conduit diversion. 
                                                                                                           1 
                                                                                              vaginal biopsy 
                                                                                                           1 
                                                                                                        NA's 
                                                                                                         544 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r0 NA's 
   1  547 

patient.follow_ups.follow_up.2.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 116   23  409 

patient.follow_ups.follow_up.2.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       152         20        376 

patient.follow_ups.follow_up.2.postoperative_rx_tx:
  no  yes NA's 
  85   61  402 


patient.follow_ups.follow_up.2.primary_therapy_outcome_success:
complete remission/response         progressive disease 
                        120                           3 
             stable disease                        NA's 
                          4                         421 

patient.follow_ups.follow_up.2.prior_tamoxifen_administered_usage_category:
current user  former user   never used         NA's 
           1            1           77          469 

patient.follow_ups.follow_up.2.radiation_therapy:
  no  yes NA's 
 117   83  348 

patient.follow_ups.follow_up.2.recurrence_second_surgery_neoplasm_surgical_procedure_name:
surgical resection               NA's 
                 2                546 

patient.follow_ups.follow_up.2.targeted_molecular_therapy:
  no  yes NA's 
  46    7  495 

patient.follow_ups.follow_up.2.vital_status:
alive  dead  NA's 
  182    22   344 

patient.follow_ups.follow_up.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2011    2012    2014    2013    2015    2015     341 

patient.follow_ups.follow_up.3.additional_treatment_completion_success_outcome:
  complete response progressive disease                NA's 
                  8                   1                 539 



patient.follow_ups.follow_up.3.birth_control_pill_history_usage_category:
former user  never used        NA's 
          2           3         543 

patient.follow_ups.follow_up.3.colorectal_cancer:
  no  yes NA's 
   4    1  543 

patient.follow_ups.follow_up.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      7      14      17      17      20      27     538 

patient.follow_ups.follow_up.3.days_to_death:
 875 1858 NA's 
   1    1  546 

patient.follow_ups.follow_up.3.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1153    1621    1792    2171    2801    3762     540 

patient.follow_ups.follow_up.3.diabetes:
  no NA's 
   4  544 

patient.follow_ups.follow_up.3.horm_ther:
no, i have never taken menopausal hormone therapy. 
                                                 4 
                                              NA's 
                                               544 

patient.follow_ups.follow_up.3.hypertension:
  no  yes NA's 
   2    2  544 

patient.follow_ups.follow_up.3.lost_follow_up:
  no NA's 
  10  538 

patient.follow_ups.follow_up.3.month_of_form_completion:
   2    7    8   11 NA's 
   1    3    2    4  538 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
 961 NA's 
   1  547 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
 158  961 NA's 
   1    1  546 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             1            547 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
new primary tumor              NA's 
                1               547 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
rectum   NA's 
     1    547 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   1  547 

patient.follow_ups.follow_up.3.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
   7    2  539 

patient.follow_ups.follow_up.3.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
         8          2        538 

patient.follow_ups.follow_up.3.postoperative_rx_tx:
  no  yes NA's 
   6    3  539 

patient.follow_ups.follow_up.3.pregnancies:
   2   4+ NA's 
   2    2  544 

patient.follow_ups.follow_up.3.primary_therapy_outcome_success:
complete remission/response                        NA's 
                          5                         543 

patient.follow_ups.follow_up.3.prior_tamoxifen_administered_usage_category:
never used       NA's 
         5        543 

patient.follow_ups.follow_up.3.radiation_therapy:
  no  yes NA's 
   4    6  538 

patient.follow_ups.follow_up.3.vital_status:
alive  dead  NA's 
    8     2   538 

patient.follow_ups.follow_up.3.year_of_form_completion:
2014 2015 NA's 
   9    1  538 

patient.follow_ups.follow_up.additional_pharmaceutical_therapy:
  no  yes NA's 
  21   36  491 

patient.follow_ups.follow_up.additional_radiation_therapy:
  no  yes NA's 
  32   25  491 

patient.follow_ups.follow_up.additional_surgery_locoregional_procedure:
  no  yes NA's 
  42   22  484 

patient.follow_ups.follow_up.additional_treatment_completion_success_outcome:
  complete response    partial response progressive disease      stable disease 
                322                  11                  48                   9 
               NA's 
                158 



patient.follow_ups.follow_up.birth_control_pill_history_usage_category:
current user  former user   never used         NA's 
           1           63          138          346 

patient.follow_ups.follow_up.colorectal_cancer:
  no  yes NA's 
 450    3   95 

patient.follow_ups.follow_up.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   17.00   16.04   23.00   31.00      13 

patient.follow_ups.follow_up.days_to_additional_surgery_metastatic_procedure:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   12.0   344.0   585.0   576.9   792.0  1374.0     527 

patient.follow_ups.follow_up.days_to_death:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   50.0   326.0   616.0   818.8  1045.0  3423.0     483 

patient.follow_ups.follow_up.days_to_last_followup:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   -6.0   428.0   681.0   936.2  1215.0  5835.0      79 

patient.follow_ups.follow_up.days_to_last_known_alive:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    394     729    1634    1867    3083    4144     539 

patient.follow_ups.follow_up.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   12.0   302.5   437.5   529.9   687.8  1940.0     482 

patient.follow_ups.follow_up.diabetes:
  no  yes NA's 
 302  118  128 

patient.follow_ups.follow_up.followup_case_report_form_submission_reason:
    additional new tumor event scheduled follow-up submission 
                             3                            257 
                          NA's 
                           288 

patient.follow_ups.follow_up.horm_ther:
                         no, i have never taken menopausal hormone therapy. 
                                                                        273 
                     yes, i am currently taking menopausal hormone therapy. 
                                                                          5 
yes, i have taken menopausal hormone therapy in the past, but no longer do. 
                                                                         40 
                                                                       NA's 
                                                                        230 

patient.follow_ups.follow_up.hypertension:
  no  yes NA's 
 183  271   94 

patient.follow_ups.follow_up.lost_follow_up:
  no  yes NA's 
 132   25  391 

patient.follow_ups.follow_up.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.587  10.000  12.000      13 


patient.follow_ups.follow_up.new_neoplasm_event_type:
locoregional disease           metastatic    new primary tumor 
                  12                   27                    4 
                NA's 
                 505 


patient.follow_ups.follow_up.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 224   44  280 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
 yes NA's 
   2  546 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no  yes NA's 
   1    1  546 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_additional_surgery_procedure:
 271  606 NA's 
   1    1  546 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 257  606 1232 NA's 
   1    1    1  545 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             3            545 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                      1                       1                       1 
                   NA's 
                    545 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
            omentum        right breast thoracic, mediastin                NA's 
                  1                   1                   1                 545 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
 yes NA's 
   2  546 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.recurrence_second_surgery_neoplasm_surgical_procedure_name:
          incisional biopsy other method, specify below 
                          1                           1 
                       NA's 
                        546 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.recurrence_second_surgery_neoplasm_surgical_procedure_name_other:
tumor debulking            NA's 
              1             547 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.2.residual_disease_post_new_tumor_event_margin_status:
  r2 NA's 
   1  547 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
  no  yes NA's 
   5   14  529 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
  11    5  532 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
 306  517  606 1122 NA's 
   1    1    1    1  544 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  131.0   367.0   452.0   631.3   573.0  2706.0     524 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence       new primary tumor 
                      7                      15                       1 
                   NA's 
                    525 


patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
  14    4  530 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.recurrence_second_surgery_neoplasm_surgical_procedure_name:
          excisional biopsy other method, specify below 
                          2                           3 
         surgical resection                        NA's 
                          1                         542 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.recurrence_second_surgery_neoplasm_surgical_procedure_name_other:
 6 cycles chemotherapy        tumor debulking whole pelvic radiation 
                     1                      1                      1 
                  NA's 
                   545 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r0   r2 NA's 
   2    1  545 

patient.follow_ups.follow_up.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
 115   24  409 

patient.follow_ups.follow_up.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       402         77         69 

patient.follow_ups.follow_up.postoperative_rx_tx:
  no  yes NA's 
  80   58  410 


patient.follow_ups.follow_up.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                        421                          10 
        progressive disease              stable disease 
                         20                           7 
                       NA's 
                         90 

patient.follow_ups.follow_up.prior_tamoxifen_administered_usage_category:
current user  former user   never used         NA's 
           1            7          351          189 

patient.follow_ups.follow_up.radiation_therapy:
  no  yes NA's 
 275  239   34 

patient.follow_ups.follow_up.recurrence_second_surgery_neoplasm_surgical_procedure_name:
          excisional biopsy other method, specify below 
                          4                          10 
         surgical resection                        NA's 
                         13                         521 


patient.follow_ups.follow_up.residual_disease_post_new_tumor_event_margin_status:
  r0   r2   rx NA's 
   9    5    4  530 

patient.follow_ups.follow_up.targeted_molecular_therapy:
  no  yes NA's 
 244  134  170 

patient.follow_ups.follow_up.vital_status:
alive  dead  NA's 
  468    65    15 

patient.follow_ups.follow_up.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2012    2015      13 

patient.gender:
female 
   548 

patient.height:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   66.0   157.0   161.0   161.2   166.0   183.0      29 

patient.histological_type:
endometrioid endometrial adenocarcinoma           mixed serous and endometrioid 
                                    411                                      22 
      serous endometrial adenocarcinoma 
                                    115 

patient.history_of_neoadjuvant_treatment:
 no yes 
546   2 

patient.horm_ther:
                         no, i have never taken menopausal hormone therapy. 
                                                                         28 
yes, i have taken menopausal hormone therapy in the past, but no longer do. 
                                                                          1 
                                                                       NA's 
                                                                        519 

patient.hypertension:
  no  yes NA's 
  15   31  502 

patient.icd_10:
c54.0 c54.1 c54.3 c54.9 
    2   539     3     4 


patient.icd_o_3_site:
c54.0 c54.1 c54.3 c54.9 
    2   539     3     4 

patient.informed_consent_verified:
yes 
548 



patient.menopause_status:
                                               indeterminate (neither pre or postmenopausal) 
                                                                                          17 
                                              peri (6-12 months since last menstrual period) 
                                                                                          17 
           post (prior bilateral ovariectomy or >12 mo since lmp with no prior hysterectomy) 
                                                                                         448 
pre (<6 months since lmp and no prior bilateral ovariectomy and not on estrogen replacement) 
                                                                                          35 
                                                                                        NA's 
                                                                                          31 

patient.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    3.00    5.00    6.06    9.00   12.00 

patient.neoplasm_histologic_grade:
        g1         g2         g3 high grade 
        99        122        316         11 

patient.new_tumor_events.new_tumor_event.2.additional_pharmaceutical_therapy:
 yes NA's 
   2  546 

patient.new_tumor_events.new_tumor_event.2.additional_radiation_therapy:
  no NA's 
   2  546 

patient.new_tumor_events.new_tumor_event.2.days_to_new_tumor_event_after_initial_treatment:
 321  661 NA's 
   1    1  546 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_occurrence_anatomic_site:
other, specify           NA's 
             2            546 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_event_type:
distant metastasis               NA's 
                 2                546 

patient.new_tumor_events.new_tumor_event.2.new_neoplasm_occurrence_anatomic_site_text:
abdominal carcinomatosis                     NA's 
                       2                      546 

patient.new_tumor_events.new_tumor_event.2.new_tumor_event_additional_surgery_procedure:
  no NA's 
   2  546 

patient.new_tumor_events.new_tumor_event.additional_pharmaceutical_therapy:
 yes NA's 
   5  543 

patient.new_tumor_events.new_tumor_event.additional_radiation_therapy:
  no  yes NA's 
   3    1  544 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_additional_surgery_procedure:
 396  517 NA's 
   1    1  546 

patient.new_tumor_events.new_tumor_event.days_to_new_tumor_event_after_initial_treatment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  321.0   335.0   407.0   448.2   517.0   661.0     543 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_occurrence_anatomic_site:
         liver other, specify           NA's 
             2              2            544 

patient.new_tumor_events.new_tumor_event.new_neoplasm_event_type:
     distant metastasis locoregional recurrence                    NA's 
                      2                       3                     543 

patient.new_tumor_events.new_tumor_event.new_neoplasm_occurrence_anatomic_site_text:
                 abdominal wall retroperitoneal ln; vulvar mass 
                              1                               1 
                           NA's 
                            546 

patient.new_tumor_events.new_tumor_event.new_tumor_event_additional_surgery_procedure:
  no  yes NA's 
   3    2  543 

patient.new_tumor_events.new_tumor_event.recurrence_second_surgery_neoplasm_surgical_procedure_name:
other method, specify below          surgical resection 
                          1                           1 
                       NA's 
                        546 

patient.new_tumor_events.new_tumor_event.recurrence_second_surgery_neoplasm_surgical_procedure_name_other:
excisional biopsy              NA's 
                1               547 

patient.new_tumor_events.new_tumor_event.residual_disease_post_new_tumor_event_margin_status:
  r0 NA's 
   1  547 

patient.new_tumor_events.new_tumor_event_after_initial_treatment:
  no  yes NA's 
  51    5  492 


patient.pct_tumor_invasion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   13.00   40.00   41.86   60.50  280.00      73 

patient.peritoneal_wash:
negative positive     NA's 
     353       58      137 

patient.person_neoplasm_cancer_status:
tumor free with tumor       NA's 
       431         79         38 

patient.pln_pos_ihc:
   0    1    2    4 NA's 
 236    2    2    1  307 

patient.pln_pos_light_micro:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.4822  0.0000 15.0000     154 

patient.postoperative_rx_tx:
  no  yes NA's 
  27   23  498 


patient.primary_therapy_outcome_success:
complete remission/response  partial remission/response 
                         34                           2 
        progressive disease              stable disease 
                          3                           2 
                       NA's 
                        507 

patient.prior_dx:
 no yes 
501  47 

patient.prior_tamoxifen_administered_usage_category:
never used       NA's 
        45        503 


patient.radiation_therapy:
  no  yes NA's 
  19   31  498 




patient.radiations.radiation.2.course_number:
   1    2 NA's 
  26    7  515 

patient.radiations.radiation.2.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0    10.0    17.0    16.6    23.0    31.0     495 

patient.radiations.radiation.2.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   64.0   136.2   216.0   360.8   262.2  2752.0     496 

patient.radiations.radiation.2.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   50.0   118.0   182.0   339.8   234.5  2718.0     497 

patient.radiations.radiation.2.measure_of_response:
               complete response                 partial response 
                              15                                1 
radiographic progressive disease                   stable disease 
                               1                                1 
                            NA's 
                             530 

patient.radiations.radiation.2.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   5.000   6.302   9.000  12.000     495 

patient.radiations.radiation.2.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00   22.00   15.09   25.00   33.00     504 

patient.radiations.radiation.2.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
     10    1500    4500    3165    4500    6100     502 

patient.radiations.radiation.2.radiation_treatment_ongoing:
  no NA's 
  53  495 


patient.radiations.radiation.2.radiation_type_notes:
gamma knife        NA's 
          1         547 

patient.radiations.radiation.2.regimen_indication:
               adjuvant other: specify in notes              palliative 
                     24                       1                       4 
             recurrence                    NA's 
                      4                     515 

patient.radiations.radiation.2.regimen_indication_notes:
mets to brain          NA's 
            1           547 

patient.radiations.radiation.2.units:
 cgy   gy NA's 
  43    3  502 

patient.radiations.radiation.2.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2015     495 

patient.radiations.radiation.3.anatomic_treatment_site:
 distant recurrence        distant site primary tumor field       regional site 
                  3                   1                   1                   4 
               NA's 
                539 



patient.radiations.radiation.3.course_number:
   1    2 NA's 
   6    2  540 

patient.radiations.radiation.3.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   17.00   23.00   21.44   25.00   31.00     539 

patient.radiations.radiation.3.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  106.0   159.0   246.0   377.1   534.0  1079.0     539 

patient.radiations.radiation.3.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   74.0   134.0   244.0   359.2   517.0  1079.0     539 

patient.radiations.radiation.3.month_of_form_completion:
   2    5    9   11 NA's 
   1    4    2    2  539 

patient.radiations.radiation.3.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00   12.50   12.50   20.75   25.00     540 

patient.radiations.radiation.3.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    540    3000    3960    4249    5500    8400     539 

patient.radiations.radiation.3.radiation_treatment_ongoing:
  no NA's 
   9  539 

patient.radiations.radiation.3.radiation_type:
               external           external beam other: specify in notes 
                      1                       7                       1 
                   NA's 
                    539 

patient.radiations.radiation.3.radiation_type_notes:
radiosurgery         NA's 
           1          547 

patient.radiations.radiation.3.regimen_indication:
   adjuvant progression  recurrence        NA's 
          5           1           2         540 

patient.radiations.radiation.3.units:
 cgy NA's 
   9  539 

patient.radiations.radiation.3.year_of_form_completion:
2010 2011 2012 2013 NA's 
   3    4    1    1  539 

patient.radiations.radiation.4.anatomic_treatment_site:
 distant recurrence    local recurrence primary tumor field       regional site 
                  1                   1                   1                   1 
               NA's 
                544 

patient.radiations.radiation.4.bcr_radiation_barcode:
tcga-a5-a0g3-r16783 tcga-d1-a163-r18490 tcga-ey-a1go-r19930 tcga-ey-a1h0-r12185 
                  1                   1                   1                   1 
               NA's 
                544 

patient.radiations.radiation.4.bcr_radiation_uuid:
0988307d-cdfd-4d8f-943d-64e7ba0d10d8 5593a6b9-35ce-410e-b6ae-8d3f47683289 
                                   1                                    1 
85817f64-a637-47a4-8fb7-57ac5ec22984 9b613400-f3a1-4eac-9bde-c5a37df2f359 
                                   1                                    1 
                                NA's 
                                 544 

patient.radiations.radiation.4.course_number:
   1    2    3 NA's 
   2    1    1  544 

patient.radiations.radiation.4.day_of_form_completion:
   4   17   25   26 NA's 
   1    1    1    1  544 

patient.radiations.radiation.4.days_to_radiation_therapy_end:
 151  176  327 1163 NA's 
   1    1    1    1  544 

patient.radiations.radiation.4.days_to_radiation_therapy_start:
 123  174  327 1163 NA's 
   1    1    1    1  544 

patient.radiations.radiation.4.month_of_form_completion:
   1    5    9   11 NA's 
   1    1    1    1  544 

patient.radiations.radiation.4.numfractions:
   1    3 NA's 
   1    2  545 

patient.radiations.radiation.4.radiation_dosage:
 540 1000 1500 6900 NA's 
   1    1    1    1  544 

patient.radiations.radiation.4.radiation_treatment_ongoing:
  no NA's 
   4  544 

patient.radiations.radiation.4.radiation_type:
          external beam other: specify in notes                    NA's 
                      2                       2                     544 

patient.radiations.radiation.4.radiation_type_notes:
        radiosurgery vaginal cylinder hdr                 NA's 
                   1                    1                  546 

patient.radiations.radiation.4.regimen_indication:
   adjuvant progression  recurrence        NA's 
          2           1           1         544 

patient.radiations.radiation.4.units:
 cgy NA's 
   4  544 

patient.radiations.radiation.4.year_of_form_completion:
2011 2012 NA's 
   3    1  544 




patient.radiations.radiation.course_number:
   1    2    3 NA's 
 150    9    1  388 

patient.radiations.radiation.day_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    9.00   17.00   15.82   23.00   31.00     310 

patient.radiations.radiation.days_to_radiation_therapy_end:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   48.0   102.0   132.0   208.3   203.0  2802.0     329 

patient.radiations.radiation.days_to_radiation_therapy_start:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   29.0    80.0   111.0   186.9   177.5  2782.0     329 

patient.radiations.radiation.measure_of_response:
               complete response                 partial response 
                              48                                1 
radiographic progressive disease                   stable disease 
                               5                                2 
                            NA's 
                             492 

patient.radiations.radiation.month_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.538   9.000  12.000     310 

patient.radiations.radiation.numfractions:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    3.00   15.84   25.00  850.00     359 

patient.radiations.radiation.radiation_dosage:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.5  2000.0  2100.0  2855.0  4500.0  7500.0     347 

patient.radiations.radiation.radiation_treatment_ongoing:
  no NA's 
 237  311 




patient.radiations.radiation.regimen_indication_notes:
left breast cancer      mets to brain               NA's 
                 1                  1                546 

patient.radiations.radiation.units:
 cgy   gy NA's 
 183   15  350 

patient.radiations.radiation.year_of_form_completion:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2012    2013    2015     310 

patient.residual_tumor:
  r0   r1   r2   rx NA's 
 376   22   16   41   93 


patient.stage_event.system_version:
1988 1995 2009 NA's 
 163   27  285   73 

patient.surgical_approach:
minimally invasive               open               NA's 
               203                321                 24 

patient.tissue_prospective_collection_indicator:
  no  yes NA's 
 433  110    5 

patient.tissue_retrospective_collection_indicator:
 no yes 
110 438 


patient.total_aor.lnp:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.4038  0.0000 29.0000     179 

patient.total_aor_lnr:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   3.000   4.729   7.000  38.000      31 

patient.total_pelv_lnp:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0000  0.0000  0.0000  0.5158  0.0000 15.0000     104 

patient.total_pelv_lnr:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    5.00   13.50   15.12   22.00   74.00      18 

patient.tumor_tissue_site:
   endometrial other  specify 
           547              1 

patient.vital_status:
alive  dead 
  503    45 

patient.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  44.00   67.00   84.00   87.59  102.75  209.00      22 

patient.year_of_form_completion:
2010 2011 2012 2013 2014 
  83  321   86   35   23 

patient.year_of_initial_pathologic_diagnosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1995    2007    2009    2009    2010    2013       8 


patient.microsatellite_instability_test_results.microsatellite_instability_test_result.mononucleotide_and_dinucleotide_marker_panel_analysis_status:
indeterminate         msi-h         msi-l           mss 
           10           170            44           324 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason:
 case matched expression control case matched methylation control 
                              24                               12 
                 ffpe validation                             NA's 
                               4                              508 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.2:
case matched methylation control              germline comparison 
                              23                                3 
                            NA's 
                             522 

patient.bcr_canonical_check.bcr_patient_canonical_reasons.bcr_canonical_reason.3:
germline comparison                NA's 
                 13                 535 

patient.bcr_canonical_check.bcr_patient_canonical_status:
       canonical canonical - plus 
             508               40 

patient.biospecimen_cqcf.consent_or_death_status:
consented  deceased 
      542         6 

patient.biospecimen_cqcf.country:
       canada        russia       ukraine united states          NA's 
           35             2             2           281           228 

patient.biospecimen_cqcf.days_to_pathology_review:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0   174.2   535.5   794.2  1061.2  5548.0      10 

patient.biospecimen_cqcf.days_to_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-690.00   14.00   28.00   31.79   42.00  805.00       9 

patient.biospecimen_cqcf.digital_image_submitted:
  no  yes NA's 
 401   92   55 

patient.biospecimen_cqcf.ethnicity:
not hispanic or latino                   NA's 
                    42                    506 



patient.biospecimen_cqcf.history_of_neoadjuvant_treatment:
  no NA's 
 547    1 



patient.biospecimen_cqcf.normal_controls.normal_control.2.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 -27.00    8.00   31.00   34.78   50.00  209.00     499 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_conc:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
  0.0570   0.1485   0.1724 261.9182 435.3605 962.1800      541 


patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_qty:
  12   20 24.8   29 NA's 
   1    4    1    1  541 

patient.biospecimen_cqcf.normal_controls.normal_control.2.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  23.00   28.50  116.00   96.86  135.50  211.00     541 



patient.biospecimen_cqcf.normal_controls.normal_control.2.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           26           522 

patient.biospecimen_cqcf.normal_controls.normal_control.2.vial_number:
   1    2 NA's 
  45   12  491 

patient.biospecimen_cqcf.normal_controls.normal_control.3.bcr_sample_uuid:
5733ecf2-9865-4aa9-8476-245f3a2c90bb e0910926-2cd1-4163-9da4-ff6b2a825200 
                                   1                                    1 
                                NA's 
                                 546 

patient.biospecimen_cqcf.normal_controls.normal_control.3.days_to_normal_sample_procurement:
   4   21 NA's 
   1    1  546 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_conc:
0.534  NA's 
    1   547 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_qm:
nano-drop      NA's 
        1       547 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_qty:
16.02  NA's 
    1   547 

patient.biospecimen_cqcf.normal_controls.normal_control.3.extracted_dna.ncedna_dna_vol:
  30 NA's 
   1  547 

patient.biospecimen_cqcf.normal_controls.normal_control.3.method_of_normal_sample_procurement:
                   blood draw blood draw (preferred method) 
                            1                             1 
                         NA's 
                          546 

patient.biospecimen_cqcf.normal_controls.normal_control.3.vial_number:
   1    3 NA's 
   1    1  546 


patient.biospecimen_cqcf.normal_controls.normal_control.days_to_normal_sample_procurement:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
-287.00   10.25   25.50   75.82   44.00 5319.00     122 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_conc:
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
   0.037    0.250    0.360  162.656  244.700 1628.600      323 


patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_qty:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  15.00   17.00   20.00   19.41   20.00   25.40     323 

patient.biospecimen_cqcf.normal_controls.normal_control.extracted_dna.ncedna_dna_vol:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.00   50.00   80.00   79.12   88.40  488.00     323 



patient.biospecimen_cqcf.normal_controls.normal_control.normal_tissue.normal_tissue_proximity:
distal (>2cm)          NA's 
           42           506 

patient.biospecimen_cqcf.normal_controls.normal_control.vial_number:
   1    2 NA's 
 529    6   13 



patient.biospecimen_cqcf.path_confirm_diagnosis_matching:
  no  yes NA's 
   2  542    4 

patient.biospecimen_cqcf.path_confirm_report_attached:
 no yes 
 24 524 

patient.biospecimen_cqcf.path_confirm_tumor_necrosis_metrics:
yes 
548 

patient.biospecimen_cqcf.path_confirm_tumor_nuclei_metrics:
 no yes 
  2 546 

patient.biospecimen_cqcf.race:
black or african american                      NA's 
                       48                       500 

patient.biospecimen_cqcf.reason_path_confirm_diagnosis_not_matching:
  pathology analysis at tss determined a specific histological subtype different from original pathology report (see note at right) 
                                                                                                                                  1 
pathology review of frozen section for tcga determined histological subtype different from the pathology report (see note at right) 
                                                                                                                                  1 
                                                                                                                               NA's 
                                                                                                                                546 

patient.biospecimen_cqcf.sample_prescreened:
yes 
548 

patient.biospecimen_cqcf.site_of_disease:
corpus uteri 
         548 


patient.biospecimen_cqcf.submitted_for_lce:
  no  yes NA's 
  48    2  498 

patient.biospecimen_cqcf.top_slide_submitted:
  no  yes NA's 
  87  406   55 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_necrosis_percent:
   0    5   10   20 NA's 
   1    1    2    1  543 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_nuclei_percent:
  70   75   90   95 NA's 
   1    1    2    1  543 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.tumor_weight:
 100  200 1800 NA's 
   1    5    1  541 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.2.vial_number:
   1    2 NA's 
   4    3  541 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.bcr_sample_uuid:
40876428-d5f2-4a7a-a707-9642b1a3bce4 a4e7601a-9828-453a-8616-17f19de6f997 
                                   1                                    1 
f806d29d-1d45-4a4e-8be2-cd2bad95789b                                 NA's 
                                   1                                  545 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_necrosis_percent:
   0   10 NA's 
   2    1  545 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_nuclei_percent:
  70   90 NA's 
   2    1  545 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.tumor_weight:
 100  200 NA's 
   1    2  545 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.3.vial_number:
   1 NA's 
   3  545 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.4.bcr_sample_uuid:
04089146-9eda-4bd0-b23d-01f7150e0e21                                 NA's 
                                   1                                  547 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.4.tumor_necrosis_percent:
   0 NA's 
   1  547 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.4.tumor_nuclei_percent:
  70 NA's 
   1  547 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.4.tumor_weight:
 200 NA's 
   1  547 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.4.vial_number:
   1 NA's 
   1  547 


patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_necrosis_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   2.000   6.366  10.000  50.000       4 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_nuclei_percent:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   40.0    70.0    80.0    81.3    90.0   100.0       3 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.tumor_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    9.0   150.0   200.0   239.4   230.0  2350.0       7 

patient.biospecimen_cqcf.tumor_samples.tumor_sample.vial_number:
   1    2 NA's 
 545    1    2 


patient.days_to_index:
   0 NA's 
 539    9 



patient.samples.sample.2.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   24.0   204.5   566.0   823.5  1076.5  5556.0       9 

patient.samples.sample.2.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   50.0    87.5   110.0   123.3   150.0   240.0     524 

patient.samples.sample.2.is_ffpe:
 no yes 
544   4 

patient.samples.sample.2.oct_embedded:
false  true 
  545     3 

patient.samples.sample.2.pathology_report_file_name:
tcga-bk-a0ca.883d4832-75fb-495c-b93f-904fca1ab18c.pdf 
                                                    1 
tcga-bk-a0cc.6a2aac78-98b9-44d4-af8d-9a132579cc2d.pdf 
                                                    1 
tcga-bk-a139.5f8c376b-31f8-4566-8ebf-21e7f4e6499d.pdf 
                                                    1 
tcga-bk-a26l.979388a8-cc61-4c03-badc-bef9ec50c40a.pdf 
                                                    1 
                                                 NA's 
                                                  544 

patient.samples.sample.2.pathology_report_uuid:
5f8c376b-31f8-4566-8ebf-21e7f4e6499d 6a2aac78-98b9-44d4-af8d-9a132579cc2d 
                                   1                                    1 
883d4832-75fb-495c-b93f-904fca1ab18c 979388a8-cc61-4c03-badc-bef9ec50c40a 
                                   1                                    1 
                                NA's 
                                 544 

patient.samples.sample.2.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.750   1.812   1.855   1.859   1.913   2.000     534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
   4  544 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
   4  544 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97500288  99028598  99036585 103627507 100476252 153878574       463 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7    8    9   13 NA's 
  10   25   46    4  463 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1500  0.5000  0.5000  0.4445  0.5000  0.5000     463 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   18.00   18.00   20.06   29.00   30.00     463 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no  yes NA's 
  81    4  463 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   6.000   8.000   7.035   8.000  11.000     463 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      1       4       5       6       8      12     463 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.95   40.00   40.00   33.07   40.00   40.00     463 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
  85  463 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   80.00   80.00   68.13   80.00   80.00     463 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 2011 2012 2013 NA's 
  69    9    1    6  463 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
150756963 150757025 167814149 160991313 167814197 167814230       543 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7   36 NA's 
   2    3  543 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
0.15 0.16 0.18 NA's 
   2    2    1  543 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   2   26 NA's 
   2    3  543 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no  yes NA's 
   2    3  543 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   8   10 NA's 
   3    2  543 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   1    4    6    8 NA's 
   1    1    2    1  543 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_id:
a32y a46x NA's 
   2    3  543 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_row:
   a    c    d    h NA's 
   1    1    1    2  543 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
0.75  0.8 1.95 2.34 NA's 
   1    2    1    1  543 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
   5  543 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
   5   13 NA's 
   3    2  543 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2013 2015 NA's 
   2    3  543 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   4  544 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   4  544 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97500630 100474274 108432736 113394584 118967797 167802612         5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    8    9   13 NA's 
   4   46  483   10    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1500  0.5000  0.5000  0.4913  0.5000  0.5000       5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   11.00   13.65   21.00   30.00       5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no  yes NA's 
 539    4    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.221   9.000  11.000       5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.00    9.00    8.86   11.00   12.00       5 



patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.95   40.00   40.00   39.04   40.00   40.00       5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
 543    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   20   80 NA's 
   4   10  529    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2011    2012    2014       5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.39   11.55   15.19   22.13   24.05   81.26     534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                    rna 
                   521                      8                     14 
                  NA's 
                     5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w    x NA's 
  14  521    8    5 



patient.samples.sample.2.portions.portion.analytes.analyte.2.concentration:
0.15 0.16 0.17 0.18 NA's 
   5    4    1    4  534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
 529   19 

patient.samples.sample.2.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
 529   19 

patient.samples.sample.2.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             543 
                                            NA's 
                                               5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no  yes NA's 
 539    4    5 

patient.samples.sample.2.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
highpure mirna (allprep dna) ffpe rna             mirvana (allprep dna) rna 
                                    4                                    10 
                              repli-g                             repli-g x 
                                  498                                     8 
                                 NA's 
                                   28 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.250   1.200   0.950   1.375   1.700     534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.200   3.700   7.450   6.093   7.675   8.100     534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
     14     534 

patient.samples.sample.2.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2    3 NA's 
  21    1    1  525 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99034893  99034952 100471062  99992349 100471099 100471130       479 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   8    9 NA's 
  23   46  479 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
  69  479 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
  18   29 NA's 
  46   23  479 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  69  479 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   6    8 NA's 
  23   46  479 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   4.000   6.000   6.304   8.000  12.000     479 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
a030 a064 NA's 
  23   46  479 


patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
  40 NA's 
  69  479 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  23 NA's 
  69  479 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  80 NA's 
  69  479 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
  69  479 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99036757  99053082  99053642 100223637  99067066 132270829       422 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8    9 NA's 
  46   80  422 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
 126  422 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   18.00   15.42   18.00   29.00     422 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 126  422 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   8.000   8.000   8.698  11.000  11.000     422 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   6.000   8.000   7.627  10.000  12.000     422 



patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
 126  422 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
 126  422 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
 126  422 

patient.samples.sample.2.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 2012 NA's 
 116    8    2  422 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                   NA's 
                    10                    116                    422 

patient.samples.sample.2.portions.portion.analytes.analyte.3.analyte_type_id:
   w    x NA's 
  10  116  422 



patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
 126  422 

patient.samples.sample.2.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
 126  422 

patient.samples.sample.2.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             126 
                                            NA's 
                                             422 

patient.samples.sample.2.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 126  422 

patient.samples.sample.2.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
       10        93       445 

patient.samples.sample.2.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2 NA's 
   9    1  538 

patient.samples.sample.2.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.910   1.880   1.950   1.963   2.000  13.840     194 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
   4  544 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
   4  544 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99027984 100476846 108432087 111823113 118964560 167780532         1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    2    5    9 NA's 
 219   91    6  231    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0600  0.0800  0.1400  0.1246  0.1600  0.1800       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   15.00   18.00   20.15   28.00   31.00       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no  yes NA's 
 543    4    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.753   8.000  12.000       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   8.000  10.000   9.077  11.000  12.000       1 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.070   1.950   2.031   3.200   3.600       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
 547    1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67    6.67   13.00   20.74   40.00   40.00       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014       1 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.additional_studies.additional_study.disease_code:
fppp NA's 
   4  544 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.additional_studies.additional_study.project_code:
tcga NA's 
   4  544 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97500299  99016628 100476272 104732749 108476531 132160027       167 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000   9.000   7.997   9.000   9.000     167 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.07000 0.08000 0.08000 0.08997 0.09000 0.18000     167 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   12.00   15.00   15.64   18.00   30.00     167 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no  yes NA's 
 377    4  167 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    5.00    5.69    8.00   12.00     167 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000  10.000   9.178  11.000  12.000     167 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.700   2.080   2.477   3.200   4.810     167 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
 381  167 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   20.00   26.00   29.62   40.00   53.40     167 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 2012 2013 NA's 
 144  232    1    4  167 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97500681  99028554  99050811 106203463 100492036 167809431       441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   2    5    8    9 NA's 
  23   10    3   71  441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0750  0.0800  0.1012  0.1450  0.1700     441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00   18.00   18.00   20.21   26.00   31.00     441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no  yes NA's 
 104    3  441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000   8.000   7.486   8.000  12.000     441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.000   6.364   9.000  12.000     441 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.070   1.600   1.829   2.080   6.430     441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
 107  441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   13.17   13.34   18.93   20.00   91.80     441 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2010    2011    2011    2015     441 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99029397 106944079 113416987 126853834 153880671 153880955       541 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   5    9 NA's 
   1    6  541 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.07 0.08 0.09 0.16 NA's 
   2    2    2    1  541 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  12   16   19 NA's 
   2    3    2  541 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   7  541 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   7    9   10 NA's 
   2    3    2  541 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   1    2    5 NA's 
   1    5    1  541 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a100 a17g a17h a325 NA's 
   2    1    1    3  541 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row:
   a    b    c    g NA's 
   3    1    1    2  541 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.400   1.700   2.080   3.369   2.990  10.720     541 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   7  541 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
  20   26   52   67 NA's 
   3    2    1    1  541 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2011 2013 NA's 
   2    2    3  541 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   4  544 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   4  544 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 99014637 100475259 108431214 111650442 118966674 167780539 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1   2   7   9 
316 227   1   4 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0700  0.1500  0.1600  0.1577  0.1700  0.1800 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   15.00   18.00   19.59   28.00   31.00 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
 no yes 
544   4 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   5.748   8.000  12.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  2.000   8.000  10.000   9.069  11.000  12.000 



patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.800   1.070   1.140   1.476   2.080   2.340 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 23 
548 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  6.670   6.700   6.700   9.374  13.000  20.000 

patient.samples.sample.2.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2013 2014 
 144  289   63   37   15 

patient.samples.sample.2.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   4.46   13.71   21.32   30.36   34.54  264.43 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type:
dna rna 
547   1 

patient.samples.sample.2.portions.portion.analytes.analyte.analyte_type_id:
  d   r 
547   1 



patient.samples.sample.2.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1200  0.1500  0.1600  0.1582  0.1700  0.1800 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
 547    1 

patient.samples.sample.2.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
 547    1 

patient.samples.sample.2.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             548 

patient.samples.sample.2.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no yes 
544   4 

patient.samples.sample.2.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
            adna preparation type                  allprep ffpe dna 
                              281                                 4 
        mirvana (allprep dna) rna pre-extracted dna received by tss 
                                1                               262 

patient.samples.sample.2.portions.portion.analytes.analyte.rna.ratio_28s_18s:
1.39 NA's 
   1  547 

patient.samples.sample.2.portions.portion.analytes.analyte.rna.rinvalue:
 8.1 NA's 
   1  547 

patient.samples.sample.2.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
      194       354 

patient.samples.sample.2.portions.portion.analytes.analyte.subportion_sequence:
   1    2    3 NA's 
  22    1    1  524 



patient.samples.sample.2.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    9.00   14.00   15.59   21.00   31.00 

patient.samples.sample.2.portions.portion.is_ffpe:
 no yes 
544   4 

patient.samples.sample.2.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   4.000   7.000   6.763  10.000  12.000 

patient.samples.sample.2.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   1.000   1.533   1.000  22.000 

patient.samples.sample.2.portions.portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    1.00    1.06    1.00    8.00      29 

patient.samples.sample.2.portions.portion.slides.slide.2.bcr_slide_barcode:
tcga-bk-a0ca-01b-02-bs2 tcga-bk-a0cc-01b-04-bs4 tcga-bk-a139-01c-08-bs8 
                      1                       1                       1 
tcga-bk-a26l-01c-04-bs4                    NA's 
                      1                     544 

patient.samples.sample.2.portions.portion.slides.slide.2.bcr_slide_uuid:
5dc72acb-fc02-4eba-8909-5d75449e0ead a52ed78b-2752-43d5-a185-c329b9b38b74 
                                   1                                    1 
a82d9022-25bc-41fd-a8d0-4f4b196198d0 eb427800-6af1-48b8-8321-5a6a7d2e6154 
                                   1                                    1 
                                NA's 
                                 544 

patient.samples.sample.2.portions.portion.slides.slide.2.image_file_name:
tcga-bk-a0ca-01b-02-bs2.a82d9022-25bc-41fd-a8d0-4f4b196198d0.svs 
                                                               1 
tcga-bk-a0cc-01b-04-bs4.5dc72acb-fc02-4eba-8909-5d75449e0ead.svs 
                                                               1 
tcga-bk-a139-01c-08-bs8.a52ed78b-2752-43d5-a185-c329b9b38b74.svs 
                                                               1 
tcga-bk-a26l-01c-04-bs4.eb427800-6af1-48b8-8321-5a6a7d2e6154.svs 
                                                               1 
                                                            NA's 
                                                             544 

patient.samples.sample.2.portions.portion.slides.slide.2.is_derived_from_ffpe:
 yes NA's 
   4  544 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   1    2    4 NA's 
   1    1    2  544 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   0   40 NA's 
   3    1  544 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_necrosis:
   0    1    2 NA's 
   2    1    1  544 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   0    2   20 NA's 
   2    1    1  544 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_normal_cells:
   0 NA's 
   4  544 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_stromal_cells:
   0   30 NA's 
   3    1  544 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_cells:
  70   98   99  100 NA's 
   1    1    1    1  544 

patient.samples.sample.2.portions.portion.slides.slide.2.percent_tumor_nuclei:
  60   70   75   80 NA's 
   1    1    1    1  544 

patient.samples.sample.2.portions.portion.slides.slide.2.section_location:
bottom   NA's 
     4    544 




patient.samples.sample.2.portions.portion.slides.slide.is_derived_from_ffpe:
  no  yes NA's 
  24    4  520 

patient.samples.sample.2.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    4.00   15.46   32.50   60.00     524 

patient.samples.sample.2.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   7.917  20.000  40.000     524 

patient.samples.sample.2.portions.portion.slides.slide.percent_necrosis:
   0    1    2    3 NA's 
  20    2    1    1  524 

patient.samples.sample.2.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   7.583  12.500  40.000     524 

patient.samples.sample.2.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   90.00  100.00   80.57  100.00  100.00     520 

patient.samples.sample.2.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    0.0     0.0     0.0     7.5     0.0   100.0     524 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    0.00   12.75    0.00  100.00     520 

patient.samples.sample.2.portions.portion.slides.slide.percent_tumor_nuclei:
   0   60   75   80 NA's 
  20    2    1    1  524 

patient.samples.sample.2.portions.portion.slides.slide.section_location:
 top NA's 
  28  520 

patient.samples.sample.2.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    2.00   89.56  200.00 2000.00     280 

patient.samples.sample.2.portions.portion.year_of_creation:
2010 2011 2012 2013 2014 
 234  230   39   36    9 

patient.samples.sample.2.sample_type:
blood derived normal        primary tumor  solid tissue normal 
                 520                    4                   24 

patient.samples.sample.2.sample_type_id:
  1  10  11 
  4 520  24 

patient.samples.sample.2.vial_number:
  a   b   c 
527  18   3 



patient.samples.sample.3.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  45.00   81.25  139.50  479.20  488.50 2391.00     528 

patient.samples.sample.3.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   40.0   100.0   140.0   176.5   210.0   430.0     531 

patient.samples.sample.3.is_ffpe:
  no NA's 
  20  528 

patient.samples.sample.3.oct_embedded:
false  true  NA's 
   16     4   528 

patient.samples.sample.3.pathology_report_file_name:
tcga-bk-a139.fe6c557c-6f28-4f5c-b48c-e2a3bb7ef14f.pdf 
                                                    1 
                                                 NA's 
                                                  547 

patient.samples.sample.3.pathology_report_uuid:
fe6c557c-6f28-4f5c-b48c-e2a3bb7ef14f                                 NA's 
                                   1                                  547 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.disease_code:
fppp NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.additional_studies.additional_study.project_code:
tcga NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-bk-a0ca-10a-02d-a275-01 tcga-bk-a0cc-10a-02d-a275-01 
                           1                            1 
                        NA's 
                         546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
0b06d0b5-b390-4eb0-be4d-114cc824d911 8b8230f4-b85a-4808-92c5-29f2b73fe475 
                                   1                                    1 
                                NA's 
                                 546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
145675731 145675733      NA's 
        1         1       546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.center_id:
   1 NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.concentration:
0.14 0.16 NA's 
   1    1  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  14 NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   3 NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_column:
   7    8 NA's 
   1    1  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_id:
a275 NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.plate_row:
   a    g NA's 
   1    1  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.quantity:
0.94 1.07 NA's 
   1    1  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.volume:
 6.7 NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2013 NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-bk-a0ca-10a-02d-a46w-08                         NA's 
                           1                          547 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
916abe51-cfd2-4d8b-9369-6fc3a30c867b                                 NA's 
                                   1                                  547 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
145674310      NA's 
        1       547 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.3.center_id:
   8 NA's 
   1  547 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.3.concentration:
0.08 NA's 
   1  547 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   3 NA's 
   1  547 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  547 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   8 NA's 
   1  547 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_column:
   9 NA's 
   1  547 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_id:
a46w NA's 
   1  547 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.3.plate_row:
   h NA's 
   1  547 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.3.quantity:
 1.6 NA's 
   1  547 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
   1  547 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.3.volume:
  20 NA's 
   1  547 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2015 NA's 
   1  547 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.disease_code:
fppp NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.additional_studies.additional_study.project_code:
tcga NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-bk-a0ca-10a-02d-a272-09 tcga-bk-a0cc-10a-02d-a272-09 
                           1                            1 
                        NA's 
                         546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
53c177e3-b385-4c03-9e4f-32b359f47496 c405ed93-f656-4974-b3e4-b65d3d2df8ab 
                                   1                                    1 
                                NA's 
                                 546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              2             546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
132158803 132160640      NA's 
        1         1       546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.center_id:
   9 NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.concentration:
0.07 0.08 NA's 
   1    1  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.day_of_shipment:
  13 NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.month_of_shipment:
   3 NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_column:
   7    8 NA's 
   1    1  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_id:
a272 NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.plate_row:
   a    g NA's 
   1    1  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.quantity:
 1.4  1.6 NA's 
   1    1  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.volume:
  20 NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.aliquots.aliquot.year_of_shipment:
2013 NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.amount:
    5 63.88  NA's 
    1     1   546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.analyte_type:
 dna NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.analyte_type_id:
   d NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.bcr_analyte_barcode:
tcga-bk-a0ca-10a-02d tcga-bk-a0cc-10a-02d                 NA's 
                   1                    1                  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.bcr_analyte_uuid:
3117a597-31e0-4e13-8c54-017ef8087637 d65682ec-e448-4b63-ac97-f95bf364eda4 
                                   1                                    1 
                                NA's 
                                 546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.concentration:
0.14 0.16 NA's 
   1    1  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               2 
                                            NA's 
                                             546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    2                   546 

patient.samples.sample.3.portions.portion.2.analytes.analyte.spectrophotometer_method:
picogreen      NA's 
        2       546 

patient.samples.sample.3.portions.portion.2.bcr_portion_barcode:
tcga-bk-a0ca-10a-02 tcga-bk-a0cc-10a-02                NA's 
                  1                   1                 546 

patient.samples.sample.3.portions.portion.2.bcr_portion_uuid:
6a68784f-0190-4640-becf-72e7181adef1 cf4f52aa-6ec2-459f-be01-869950e4e7e0 
                                   1                                    1 
                                NA's 
                                 546 

patient.samples.sample.3.portions.portion.2.day_of_creation:
  27 NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.is_ffpe:
  no NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.month_of_creation:
   2 NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.portion_number:
   2 NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.portion_sequence:
   2 NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.weight:
   2 NA's 
   2  546 

patient.samples.sample.3.portions.portion.2.year_of_creation:
2013 NA's 
   2  546 

patient.samples.sample.3.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.740   1.800   1.845   1.852   1.870   2.040     534 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99019532 106934024 118957353 121089363 138896429 145632921       533 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7    8   13 NA's 
  13    1    1  533 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.150   0.160   0.160   0.184   0.170   0.500     533 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   18.00   25.00   22.27   29.50   30.00     533 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  15  533 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     3.0     5.0     4.8     5.5    10.0     533 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   5.000   6.000   5.733   7.000   8.000     533 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.950   2.080   2.210   5.138   3.735  40.000     533 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
  15  533 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
  13   20 26.7   80 NA's 
  10    1    3    1  533 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
2010 2011 2012 2013 NA's 
   2    4    4    5  533 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99034892 112147514 118956083 120220495 125632876 145631803       528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   7    9   13 NA's 
   1    6   13  528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.150   0.160   0.165   0.263   0.500   0.500     528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   4.00   17.25   24.00   21.35   26.00   30.00     528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  20  528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    1.0     3.0     5.0     5.7     9.0    11.0     528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    5.00    6.50    7.00    7.25   12.00     528 



patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   3.00    3.20    3.40   14.31   40.00   40.00     528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
  20  528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  20 26.7   80 NA's 
  13    1    6  528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2013 NA's 
   2    9    4    5  528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   7.82    8.50   11.67   23.76   30.41   73.88     534 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                  rna                 NA's 
                   6                   14                  528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.analyte_type_id:
   r    w NA's 
  14    6  528 



patient.samples.sample.3.portions.portion.analytes.analyte.2.concentration:
0.15 0.16 0.17 0.18 NA's 
   3    7    3    1  534 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   6  542 

patient.samples.sample.3.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
   6  542 

patient.samples.sample.3.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              20 
                                            NA's 
                                             528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
  20  528 

patient.samples.sample.3.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna                   repli-g                      NA's 
                       14                         5                       529 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.100   1.200   1.300   1.326   1.385   1.800     534 

patient.samples.sample.3.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  7.300   7.500   7.650   7.793   8.050   8.700     534 

patient.samples.sample.3.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec    NA's 
     14     534 

patient.samples.sample.3.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2    3 NA's 
  15    1    1  531 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-bk-a0cc-10a-01x-a030-08                         NA's 
                           1                          547 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.bcr_aliquot_uuid:
ae435dbd-5f54-4c0b-bb02-4a2fd71c092c                                 NA's 
                                   1                                  547 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
99036855     NA's 
       1      547 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   8 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
  29 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   6 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   6 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
a030 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_row:
   h NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
  40 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  23 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  80 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
   1  547 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99015692 113404052 132270282 122848371 132270603 132271494       533 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   9 NA's 
  15  533 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
  15  533 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   8.00    8.00   11.00   12.87   14.50   29.00     533 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  15  533 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.000   7.000   6.733   8.000  11.000     533 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   5    6   12 NA's 
   1    8    6  533 



patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
  15  533 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
  15  533 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
  15  533 

patient.samples.sample.3.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2013 NA's 
   2    4    4    5  533 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                   NA's 
                    14                      1                    533 

patient.samples.sample.3.portions.portion.analytes.analyte.3.analyte_type_id:
   w    x NA's 
  14    1  533 



patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
  15  533 

patient.samples.sample.3.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
  15  533 

patient.samples.sample.3.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              15 
                                            NA's 
                                             533 

patient.samples.sample.3.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
  15  533 

patient.samples.sample.3.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
     14     534 

patient.samples.sample.3.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2    3 NA's 
  12    1    1  534 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_barcode:
tcga-bk-a0cb-11a-33x-a10d-09                         NA's 
                           1                          547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.bcr_aliquot_uuid:
ace6c3c1-e405-4535-8ac0-b708d2196f69                                 NA's 
                                   1                                  547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
99053207     NA's 
       1      547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   9 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   8 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
  11 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   6 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id:
a10d NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_row:
   h NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  23 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2010 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.analyte_type:
repli-g x (qiagen) dna                   NA's 
                     1                    547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.analyte_type_id:
   x NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.bcr_analyte_barcode:
tcga-bk-a0cb-11a-33x                 NA's 
                   1                  547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.bcr_analyte_uuid:
63abc097-b457-4b86-bfee-adb0a41807d6                                 NA's 
                                   1                                  547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
 yes NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g x      NA's 
        1       547 

patient.samples.sample.3.portions.portion.analytes.analyte.4.subportion_sequence:
   3 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.810   1.900   1.950   1.938   1.980   2.070     539 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99065228 110161790 113422438 119271662 125632856 145632762       528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   1    2    5    9 NA's 
   5    2    3   10  528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.1200  0.1205  0.1625  0.1800     528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0    14.0    21.5    19.4    26.0    30.0     528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  20  528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    5.00    5.25    6.00   10.00     528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00    6.00    9.00    8.95   12.00   12.00     528 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.185   2.210   2.240   2.900   3.600     528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
  20  528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.67   11.43   13.17   23.59   40.00   40.00     528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 2012 2013 NA's 
   2    9    4    5  528 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99016076 110160403 113421243 118449228 122257239 145633146       528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   5    8    9   26 NA's 
   9    1    9    1  528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.1150  0.1185  0.1525  0.1700     528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0    14.0    21.5    19.4    26.0    30.0     528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  20  528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    5.00    5.25    6.00   10.00     528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00    6.00    9.00    8.95   12.00   12.00     528 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.070   1.950   2.505   2.619   3.200   4.010     528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
  20  528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.00   13.00   20.02   25.70   40.00   40.00     528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2010 2011 2012 2013 NA's 
   2    9    4    5  528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.additional_studies.additional_study.disease_code:
fppp NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.additional_studies.additional_study.project_code:
tcga NA's 
   1  547 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             547 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99015500 100113838 106956706 112955877 118105926 145633060       540 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   2    5    9 NA's 
   1    5    2  540 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0800  0.1425  0.1600  0.1462  0.1700  0.1800     540 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   11.75   14.00   16.12   19.75   31.00     540 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   8  540 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    5.00    5.25    6.50   11.00     540 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   5    6    7   12 NA's 
   1    3    1    3  540 


patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_row:
   d    f    g    h NA's 
   1    2    1    4  540 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.800   2.080   3.240   3.257   4.338   4.810     540 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
   8  540 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  13   20   26 26.7 NA's 
   2    1    1    4  540 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2010 2011 2013 NA's 
   2    4    2  540 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.disease_code:
fppp NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.project_code:
tcga NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-bk-a0cb-11a-33d-a10q-05 tcga-bk-a26l-10a-01d-a275-01 
                           1                            1 
                        NA's 
                         546 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
14429c6a-938f-4e24-bfe4-025ff12a1857 8c623eeb-c4d4-4561-a8c3-1beceeb9fd63 
                                   1                                    1 
                                NA's 
                                 546 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             547 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
100476623 145675724      NA's 
        1         1       546 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   1    5 NA's 
   1    1  546 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.15 0.17 NA's 
   1    1  546 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  14   20 NA's 
   1    1  546 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   2  546 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   3   12 NA's 
   1    1  546 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   7    9 NA's 
   1    1  546 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a10q a275 NA's 
   1    1  546 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row:
   f    h NA's 
   1    1  546 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
1.14 4.01 NA's 
   1    1  546 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   2  546 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
 6.7 26.7 NA's 
   1    1  546 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2013 NA's 
   1    1  546 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-bk-a26l-10a-01d-a46w-08                         NA's 
                           1                          547 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
00654b37-a4b5-4be7-a467-7f46cb9c86ae                                 NA's 
                                   1                                  547 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
145674311      NA's 
        1       547 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   8 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.09 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   3 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   8 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
  10 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
a46w NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   f NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
 1.8 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
  20 NA's 
   1  547 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2015 NA's 
   1  547 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99031052 110160842 113421587 119279440 125643208 145671546       528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1    2 NA's 
  15    5  528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.14    0.15    0.16    0.16    0.17    0.18     528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    4.0    14.0    18.0    18.7    25.0    30.0     528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
  20  528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    3.00    5.00    5.25    6.00   10.00     528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00    6.00    9.00    8.95   12.00   12.00     528 



patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.940   1.055   1.100   1.330   1.343   2.340     528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
  20  528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.67  6.7   13 NA's 
   3   12    5  528 

patient.samples.sample.3.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2013 NA's 
   2    9    4    5  528 

patient.samples.sample.3.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.12   12.12   18.82   27.69   34.16   98.46     528 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
  20  528 

patient.samples.sample.3.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
  20  528 



patient.samples.sample.3.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.14    0.15    0.16    0.16    0.17    0.18     528 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
  20  528 

patient.samples.sample.3.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
  20  528 

patient.samples.sample.3.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                              20 
                                            NA's 
                                             528 

patient.samples.sample.3.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
  20  528 

patient.samples.sample.3.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                   20                   528 

patient.samples.sample.3.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec      NA's 
       11         9       528 

patient.samples.sample.3.portions.portion.analytes.analyte.subportion_sequence:
   1    2    3 NA's 
  15    1    1  531 



patient.samples.sample.3.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    7.50   14.50   12.75   16.00   31.00     528 

patient.samples.sample.3.portions.portion.is_ffpe:
  no NA's 
  20  528 

patient.samples.sample.3.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00    3.75    6.00    6.40    9.00   12.00     528 

patient.samples.sample.3.portions.portion.portion_number:
   1   11   12   33 NA's 
   3   15    1    1  528 

patient.samples.sample.3.portions.portion.portion_sequence:
   1    3 NA's 
  18    1  529 

patient.samples.sample.3.portions.portion.slides.slide.2.bcr_slide_barcode:
tcga-bk-a0cb-11a-03-ts3                    NA's 
                      1                     547 

patient.samples.sample.3.portions.portion.slides.slide.2.bcr_slide_uuid:
91a84b56-6c1b-489f-8dc0-484f5e49be06                                 NA's 
                                   1                                  547 

patient.samples.sample.3.portions.portion.slides.slide.2.image_file_name:
tcga-bk-a0cb-11a-03-ts3.91a84b56-6c1b-489f-8dc0-484f5e49be06.svs 
                                                               1 
                                                            NA's 
                                                             547 

patient.samples.sample.3.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
   1  547 

patient.samples.sample.3.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   5 NA's 
   1  547 

patient.samples.sample.3.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   0 NA's 
   1  547 

patient.samples.sample.3.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   0 NA's 
   1  547 

patient.samples.sample.3.portions.portion.slides.slide.2.percent_normal_cells:
 100 NA's 
   1  547 

patient.samples.sample.3.portions.portion.slides.slide.2.percent_tumor_cells:
   0 NA's 
   1  547 

patient.samples.sample.3.portions.portion.slides.slide.2.section_location:
 top NA's 
   1  547 

patient.samples.sample.3.portions.portion.slides.slide.3.bcr_slide_barcode:
tcga-bk-a0cb-11a-04-ts4                    NA's 
                      1                     547 

patient.samples.sample.3.portions.portion.slides.slide.3.bcr_slide_uuid:
31b8cd48-1eaf-4c06-a64c-1703f9dfd57f                                 NA's 
                                   1                                  547 

patient.samples.sample.3.portions.portion.slides.slide.3.image_file_name:
tcga-bk-a0cb-11a-04-ts4.31b8cd48-1eaf-4c06-a64c-1703f9dfd57f.svs 
                                                               1 
                                                            NA's 
                                                             547 

patient.samples.sample.3.portions.portion.slides.slide.3.is_derived_from_ffpe:
  no NA's 
   1  547 

patient.samples.sample.3.portions.portion.slides.slide.3.percent_lymphocyte_infiltration:
   5 NA's 
   1  547 

patient.samples.sample.3.portions.portion.slides.slide.3.percent_monocyte_infiltration:
   0 NA's 
   1  547 

patient.samples.sample.3.portions.portion.slides.slide.3.percent_neutrophil_infiltration:
   0 NA's 
   1  547 

patient.samples.sample.3.portions.portion.slides.slide.3.percent_normal_cells:
 100 NA's 
   1  547 

patient.samples.sample.3.portions.portion.slides.slide.3.percent_tumor_cells:
   0 NA's 
   1  547 

patient.samples.sample.3.portions.portion.slides.slide.3.section_location:
 top NA's 
   1  547 




patient.samples.sample.3.portions.portion.slides.slide.is_derived_from_ffpe:
  no NA's 
  17  531 

patient.samples.sample.3.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   8.294   8.000  40.000     531 

patient.samples.sample.3.portions.portion.slides.slide.percent_monocyte_infiltration:
   0   20 NA's 
  15    2  531 

patient.samples.sample.3.portions.portion.slides.slide.percent_necrosis:
   0   20 NA's 
  15    1  532 

patient.samples.sample.3.portions.portion.slides.slide.percent_neutrophil_infiltration:
   0   10   20   30 NA's 
  14    1    1    1  531 

patient.samples.sample.3.portions.portion.slides.slide.percent_normal_cells:
   5   40  100 NA's 
   1    1   15  531 

patient.samples.sample.3.portions.portion.slides.slide.percent_stromal_cells:
   0   95 NA's 
  15    1  532 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_cells:
   0   40 NA's 
  16    1  531 

patient.samples.sample.3.portions.portion.slides.slide.percent_tumor_nuclei:
   0   65 NA's 
  15    1  532 

patient.samples.sample.3.portions.portion.slides.slide.section_location:
 top NA's 
  17  531 

patient.samples.sample.3.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      2      30      30      48      30     240     529 

patient.samples.sample.3.portions.portion.year_of_creation:
2010 2011 2012 NA's 
   4   10    6  528 

patient.samples.sample.3.sample_type:
blood derived normal      recurrent tumor  solid tissue normal 
                   3                    1                   16 
                NA's 
                 528 

patient.samples.sample.3.sample_type_id:
   2   10   11 NA's 
   1    3   16  528 

patient.samples.sample.3.vial_number:
   a NA's 
  20  528 

patient.samples.sample.4.bcr_sample_barcode:
tcga-bk-a139-10a             NA's 
               1              547 

patient.samples.sample.4.bcr_sample_uuid:
dc183cca-cf96-4df3-87db-bfcff58dc273                                 NA's 
                                   1                                  547 

patient.samples.sample.4.days_to_collection:
 175 NA's 
   1  547 

patient.samples.sample.4.is_ffpe:
  no NA's 
   1  547 

patient.samples.sample.4.oct_embedded:
false  NA's 
    1   547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_barcode:
tcga-bk-a139-10a-01w-a12v-09                         NA's 
                           1                          547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.bcr_aliquot_uuid:
d8d929ae-bff5-42b2-8b47-a0b80a0aef0a                                 NA's 
                                   1                                  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
108432685      NA's 
        1       547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
   9 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
 0.5 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
  23 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   2 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
  11 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_id:
a12v NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_row:
   g NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
  40 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
  23 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  80 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2011 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type:
repli-g (qiagen) dna                 NA's 
                   1                  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.analyte_type_id:
   w NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.bcr_analyte_barcode:
tcga-bk-a139-10a-01w                 NA's 
                   1                  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.bcr_analyte_uuid:
71c07865-4cec-415e-92fc-44ef9d0304d2                                 NA's 
                                   1                                  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.normal_tumor_genotype_match:
 yes NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.dna.pcr_amplification_successful:
 yes NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
  no NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
repli-g    NA's 
      1     547 

patient.samples.sample.4.portions.portion.analytes.analyte.a260_a280_ratio:
   2 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_barcode:
tcga-bk-a139-10a-01d-a121-02                         NA's 
                           1                          547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.bcr_aliquot_uuid:
255cdc14-da01-436a-a7ac-10a0e5adca6b                                 NA's 
                                   1                                  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
100474132      NA's 
        1       547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
   2 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
0.15 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
  18 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   1 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
  11 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_id:
a121 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_row:
   g NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
1.95 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
  23 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
  13 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2011 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_barcode:
tcga-bk-a139-10a-01d-a122-09                         NA's 
                           1                          547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.bcr_aliquot_uuid:
2ae69cba-be7f-46c3-b610-e27a48daef9d                                 NA's 
                                   1                                  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
99016084     NA's 
       1      547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   9 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
0.07 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
  18 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   1 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
  11 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_id:
a122 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_row:
   g NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
 2.8 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
  40 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
2011 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.additional_studies.additional_study.disease_code:
fppp NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.additional_studies.additional_study.project_code:
tcga NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_barcode:
tcga-bk-a139-10a-01d-a272-09                         NA's 
                           1                          547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_aliquot_uuid:
31b62653-e962-4865-97c5-a67f908c4f69                                 NA's 
                                   1                                  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
132159102      NA's 
        1       547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   9 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
0.07 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
  13 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   3 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   7 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_id:
a272 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_row:
   h NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
 1.4 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
  20 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
2013 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.disease_code:
fppp NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.additional_studies.additional_study.project_code:
tcga NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_barcode:
tcga-bk-a139-10a-01d-a275-01                         NA's 
                           1                          547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_aliquot_uuid:
023b8011-6da4-4ce2-adb9-124bcb8db69c                                 NA's 
                                   1                                  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
145675748      NA's 
        1       547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   1 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
0.15 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
  14 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   3 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   7 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_id:
a275 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_row:
   h NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
1.01 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
 6.7 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2013 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_barcode:
tcga-bk-a139-10a-01d-a46w-08                         NA's 
                           1                          547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_aliquot_uuid:
4f24fd49-dd13-407e-8f4f-a7488d7cc78a                                 NA's 
                                   1                                  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
145674313      NA's 
        1       547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   8 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
0.07 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   3 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   8 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
  10 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_id:
a46w NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_row:
   c NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
 1.4 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
  20 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2015 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_barcode:
tcga-bk-a139-10a-01d-a120-01                         NA's 
                           1                          547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.bcr_aliquot_uuid:
4da5fca9-08af-4d60-83fe-7e077775fd72                                 NA's 
                                   1                                  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
100475860      NA's 
        1       547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
   1 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.15 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
  18 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   1 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
  11 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a120 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.plate_row:
   g NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
   1 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
  23 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.volume:
6.67 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2011 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.amount:
17.4 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type:
 dna NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.analyte_type_id:
   d NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_barcode:
tcga-bk-a139-10a-01d                 NA's 
                   1                  547 

patient.samples.sample.4.portions.portion.analytes.analyte.bcr_analyte_uuid:
aa78120b-d2fc-43ad-88ba-a66d62168aac                                 NA's 
                                   1                                  547 

patient.samples.sample.4.portions.portion.analytes.analyte.concentration:
0.15 NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
 yes NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
 yes NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                               1 
                                            NA's 
                                             547 

patient.samples.sample.4.portions.portion.analytes.analyte.is_derived_from_ffpe:
  no NA's 
   1  547 

patient.samples.sample.4.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type                  NA's 
                    1                   547 

patient.samples.sample.4.portions.portion.analytes.analyte.spectrophotometer_method:
uv spec    NA's 
      1     547 

patient.samples.sample.4.portions.portion.bcr_portion_barcode:
tcga-bk-a139-10a-01                NA's 
                  1                 547 

patient.samples.sample.4.portions.portion.bcr_portion_uuid:
70bafda3-4861-4f2b-b3af-fa8f2e934153                                 NA's 
                                   1                                  547 

patient.samples.sample.4.portions.portion.day_of_creation:
  16 NA's 
   1  547 

patient.samples.sample.4.portions.portion.is_ffpe:
  no NA's 
   1  547 

patient.samples.sample.4.portions.portion.month_of_creation:
  12 NA's 
   1  547 

patient.samples.sample.4.portions.portion.portion_number:
   1 NA's 
   1  547 

patient.samples.sample.4.portions.portion.portion_sequence:
   1 NA's 
   1  547 

patient.samples.sample.4.portions.portion.weight:
 200 NA's 
   1  547 

patient.samples.sample.4.portions.portion.year_of_creation:
2010 NA's 
   1  547 

patient.samples.sample.4.sample_type:
blood derived normal                 NA's 
                   1                  547 

patient.samples.sample.4.sample_type_id:
  10 NA's 
   1  547 

patient.samples.sample.4.vial_number:
   a NA's 
   1  547 



patient.samples.sample.days_to_collection:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   24.0   196.0   560.0   816.2  1070.0  5556.0       9 




patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.4:
33c67f98-a8c3-4c3a-b630-d12014c645ec 631a44b6-43e0-4481-af1f-3eb484784672 
                                   1                                    1 
860ccc5d-05a1-4630-8ae0-620825609f83 970a790e-649c-4d4d-98db-7df6a7045a14 
                                   1                                    1 
                                NA's 
                                 544 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.5:
5d10994a-40e6-4327-a9c0-6fea2ac6b771 75c61081-9ab3-4237-b2ce-efcba1f22343 
                                   1                                    1 
c87132c9-f354-4ead-a89c-df85d3890338                                 NA's 
                                   1                                  545 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.6:
32d62843-11db-4653-990e-80a3eeb59612 af10cfc6-3016-4523-9a01-5ef8a06b3cdd 
                                   1                                    1 
d526a895-b369-47b9-8903-8731bcd0a1bb                                 NA's 
                                   1                                  545 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.7:
1f385796-7aec-43ce-9f6a-564204ba9be6 267346bc-b6c5-4613-9b19-460672319141 
                                   1                                    1 
                                NA's 
                                 546 

patient.samples.sample.diagnostic_slides.ffpe_slide_uuid.8:
8b5c9140-cde8-47f6-b847-f5cfb92399cd                                 NA's 
                                   1                                  547 

patient.samples.sample.initial_weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    1.0   110.0   175.0   235.6   280.0  1540.0 

patient.samples.sample.is_ffpe:
 no 
548 

patient.samples.sample.oct_embedded:
false  true 
  304   244 



patient.samples.sample.portions.portion.analytes.analyte.2.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.350   1.800   1.840   2.158   1.880 180.000 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99019239 100475438 108431976 111321284 116188610 167788983         1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.center_id:
   7   13 NA's 
 395  152    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1600  0.1607  0.1700  0.1800       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   15.00   18.00   19.58   28.00   31.00       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
 547    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.757   8.000  12.000       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   4.097   6.000  10.000       1 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.93    2.21    3.20    3.22    4.27    4.81       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.source_center:
  23 NA's 
 547    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.volume:
6.67   13   20 26.7 NA's 
  24  167  128  228    1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.2.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014       1 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.additional_studies.additional_study.disease_code:
fppp NA's 
   3  545 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.additional_studies.additional_study.project_code:
tcga NA's 
   3  545 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3             545 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
100471174 113422303 113422324 125290443 145632072 167812967       514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.center_id:
   7 NA's 
  34  514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.1300  0.1500  0.1500  0.1547  0.1600  0.1800     514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  2.000   5.000   5.000   6.912   5.000  29.000     514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
  34  514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  3.000   7.000   7.000   7.588   9.000  12.000     514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.750   8.000   6.912   9.000  11.000     514 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.060   1.853   1.950   2.074   2.178   4.270     514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.source_center:
  23 NA's 
  34  514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.volume:
 6.6   13 26.7 NA's 
   2   30    2  514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.3.year_of_shipment:
2010 2011 2013 2014 NA's 
   2   21    9    2  514 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.additional_studies.additional_study.disease_code:
fppp NA's 
   4  544 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.additional_studies.additional_study.project_code:
tcga NA's 
   4  544 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              4             544 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
113422292 132179204 132179220 137234165 142268855 167812991       542 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.center_id:
   7   13 NA's 
   3    3  542 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.concentration:
0.15 0.16 0.18 NA's 
   4    1    1  542 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.day_of_shipment:
   5    9   13   14 NA's 
   1    1    3    1  542 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
   6  542 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.month_of_shipment:
   3    7   12 NA's 
   4    1    1  542 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_column:
   1    7   10 NA's 
   1    4    1  542 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.plate_id:
a16w a277 a27d a466 NA's 
   1    1    3    1  542 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.990   1.982   2.540   2.437   3.000   3.600     542 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.source_center:
  23 NA's 
   6  542 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.volume:
 6.6   13   20 NA's 
   1    2    3  542 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.4.year_of_shipment:
2011 2013 2014 NA's 
   1    4    1  542 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.additional_studies.additional_study.disease_code:
fppp NA's 
   1  547 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.additional_studies.additional_study.project_code:
tcga NA's 
   1  547 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             547 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
132179224 167812941 167814148 160686948 167814196 167814233       543 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.center_id:
   7   13   36 NA's 
   1    1    3  543 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.concentration:
0.15 0.16 0.18 NA's 
   3    1    1  543 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.day_of_shipment:
   9   13   26 NA's 
   1    1    3  543 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
   5  543 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.month_of_shipment:
   3    8   12 NA's 
   1    3    1  543 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.plate_column:
   3    5    7 NA's 
   1    2    2  543 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.plate_id:
a27d a466 a46x NA's 
   1    1    3  543 


patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.quantity:
0.75  0.9 1.06    3 NA's 
   2    1    1    1  543 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.source_center:
  23 NA's 
   5  543 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.volume:
   5  6.6   20 NA's 
   3    1    1  543 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.5.year_of_shipment:
2013 2014 2015 NA's 
   1    1    3  543 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 99012901 100474385 108431940 111239284 118956032 167812023 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.center_id:
  7  13 
153 395 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1607  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   15.00   18.00   19.59   28.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.is_derived_from_ffpe:
 no 
548 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   5.752   8.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.097   6.000  10.000 



patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.950   3.200   3.400   3.357   3.600   4.540 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.source_center:
 23 
548 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.volume:
  13   20 26.7 
  36  395  117 

patient.samples.sample.portions.portion.analytes.analyte.2.aliquots.aliquot.year_of_shipment:
2010 2011 2012 2013 2014 
 145  292   63   33   15 

patient.samples.sample.portions.portion.analytes.analyte.2.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.59   53.38   74.17   71.71   90.47  148.61 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type:
rna 
548 

patient.samples.sample.portions.portion.analytes.analyte.2.analyte_type_id:
  r 
548 



patient.samples.sample.portions.portion.analytes.analyte.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1607  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.2.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             548 

patient.samples.sample.portions.portion.analytes.analyte.2.is_derived_from_ffpe:
 no 
548 

patient.samples.sample.portions.portion.analytes.analyte.2.protocols.protocol.experimental_protocol_type:
mirvana (allprep dna) rna 
                      548 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.ratio_28s_18s:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   1.200   1.485   1.461   1.700   4.100 

patient.samples.sample.portions.portion.analytes.analyte.2.rna.rinvalue:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  7.000   7.500   8.100   8.139   8.700  10.000 

patient.samples.sample.portions.portion.analytes.analyte.2.spectrophotometer_method:
uv spec 
    548 

patient.samples.sample.portions.portion.analytes.analyte.2.subportion_sequence:
   1    2    3 NA's 
 491   52    2    3 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97500617  99036047  99036578 100819793  99067577 153878512       476 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.center_id:
   8    9 NA's 
  26   46  476 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.concentration:
 0.5 NA's 
  72  476 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.day_of_shipment:
  15   18   29 NA's 
   2   46   24  476 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  72  476 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.month_of_shipment:
   6    7    8 NA's 
  24    2   46  476 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   4.431   6.250  12.000     476 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.plate_id:
a028 a062 a315 NA's 
  24   46    2  476 


patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.quantity:
   5   40 NA's 
   2   70  476 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.source_center:
  23 NA's 
  72  476 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.volume:
  10   80 NA's 
   2   70  476 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.2.year_of_shipment:
2010 2013 NA's 
  70    2  476 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97500643 100474266 108432724 113165586 118967300 167804457         5 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.center_id:
   8    9 NA's 
  46  497    5 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.concentration:
 0.5 NA's 
 543    5 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    8.00   11.00   13.62   21.00   29.00       5 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 543    5 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   6.000   6.247   9.000  11.000       5 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   4.355   5.000  12.000       5 



patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.quantity:
  40 NA's 
 543    5 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.source_center:
  23 NA's 
 543    5 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.volume:
  80 NA's 
 543    5 

patient.samples.sample.portions.portion.analytes.analyte.3.aliquots.aliquot.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2011    2011    2011    2014       5 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type:
  repli-g (qiagen) dna repli-g x (qiagen) dna                   NA's 
                   535                      8                      5 

patient.samples.sample.portions.portion.analytes.analyte.3.analyte_type_id:
   w    x NA's 
 535    8    5 



patient.samples.sample.portions.portion.analytes.analyte.3.dna.normal_tumor_genotype_match:
 yes NA's 
 543    5 

patient.samples.sample.portions.portion.analytes.analyte.3.dna.pcr_amplification_successful:
 yes NA's 
 543    5 

patient.samples.sample.portions.portion.analytes.analyte.3.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             543 
                                            NA's 
                                               5 

patient.samples.sample.portions.portion.analytes.analyte.3.is_derived_from_ffpe:
  no NA's 
 543    5 

patient.samples.sample.portions.portion.analytes.analyte.3.protocols.protocol.experimental_protocol_type:
  repli-g repli-g x      NA's 
      511         8        29 

patient.samples.sample.portions.portion.analytes.analyte.3.subportion_sequence:
   1    2    3 NA's 
 486   52    2    8 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99034900  99035291 100471050  99978708 100471104 100471139       478 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.center_id:
   8    9 NA's 
  24   46  478 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.concentration:
 0.5 NA's 
  70  478 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.day_of_shipment:
  18   29 NA's 
  46   24  478 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.is_derived_from_ffpe:
  no NA's 
  70  478 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.month_of_shipment:
   6    8 NA's 
  24   46  478 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   3.000   4.471   6.750  12.000     478 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.plate_id:
a030 a064 NA's 
  24   46  478 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.quantity:
  40 NA's 
  70  478 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.source_center:
  23 NA's 
  70  478 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.volume:
  80 NA's 
  70  478 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.2.year_of_shipment:
2010 NA's 
  70  478 



patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99052561  99053091  99053631  99080424  99053699 100471057       431 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.center_id:
   8    9 NA's 
  46   71  431 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.concentration:
 0.5 NA's 
 117  431 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.day_of_shipment:
   8   18   29 NA's 
  47   46   24  431 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
 117  431 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.month_of_shipment:
   6    8   11 NA's 
  24   46   47  431 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    3.00    4.06    5.00   12.00     431 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.plate_id:
a029 a063 a10d NA's 
  24   46   47  431 


patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.quantity:
  40 NA's 
 117  431 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.source_center:
  23 NA's 
 117  431 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.volume:
  80 NA's 
 117  431 

patient.samples.sample.portions.portion.analytes.analyte.4.aliquots.aliquot.year_of_shipment:
2010 NA's 
 117  431 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type:
repli-g x (qiagen) dna                   NA's 
                   117                    431 

patient.samples.sample.portions.portion.analytes.analyte.4.analyte_type_id:
   x NA's 
 117  431 



patient.samples.sample.portions.portion.analytes.analyte.4.dna.normal_tumor_genotype_match:
 yes NA's 
 117  431 

patient.samples.sample.portions.portion.analytes.analyte.4.dna.pcr_amplification_successful:
 yes NA's 
 117  431 

patient.samples.sample.portions.portion.analytes.analyte.4.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             117 
                                            NA's 
                                             431 

patient.samples.sample.portions.portion.analytes.analyte.4.is_derived_from_ffpe:
  no NA's 
 117  431 

patient.samples.sample.portions.portion.analytes.analyte.4.protocols.protocol.experimental_protocol_type:
repli-g x      NA's 
       93       455 

patient.samples.sample.portions.portion.analytes.analyte.4.subportion_sequence:
   1    2    3 NA's 
 108    8    1  431 

patient.samples.sample.portions.portion.analytes.analyte.a260_a280_ratio:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.350   1.897   1.950   1.949   2.000   2.850     188 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
 99014631 100474959 108431160 111118934 118963662 167822340 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.center_id:
  1   2   5   9 
248 228  71   1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0800  0.1500  0.1600  0.1581  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   15.00   18.00   19.61   28.00   31.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.is_derived_from_ffpe:
 no 
548 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   5.000   5.752   8.000  12.000 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   4.000   4.133   5.000  12.000 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.870   1.070   1.845   1.881   2.210   4.540 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.source_center:
 23 
548 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   6.67    6.70   13.00   11.91   13.00   40.00 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.2.year_of_shipment:
2010 2011 2012 2013 2014 
 145  292   63   33   15 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99027978 100476830 108432090 111502046 116194753 167780530         1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.center_id:
   1    2    5    9 NA's 
 286   47   20  194    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.1500  0.1302  0.1600  0.1800       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   15.00   18.00   19.58   28.00   31.00       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.is_derived_from_ffpe:
  no NA's 
 547    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.757   8.000  12.000       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   4.132   5.000  12.000       1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.070   1.200   1.914   3.200   3.600       1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.source_center:
  23 NA's 
 547    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.volume:
6.67  6.7   13   40 NA's 
 278    8   67  194    1 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.3.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014       1 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99014951 100474620 108476460 111561787 113421787 167780460         2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.center_id:
   1    2    5    9 NA's 
  12   73  232  229    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.1500  0.1248  0.1600  0.1800       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00   15.00   18.00   20.21   28.00   31.00       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.is_derived_from_ffpe:
  no NA's 
 546    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   5.000   5.758   8.000  12.000       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   4.000   4.137   5.000  12.000       2 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.950   2.210   2.713   3.600   4.810       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.source_center:
  23 NA's 
 546    2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   13.00   26.00   24.94   40.00   53.40       2 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.4.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2010    2011    2011    2011    2014       2 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97500645  99019568  99067569 103621007 108474667 113421639       176 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.center_id:
   5    8    9 NA's 
 225   70   77  176 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.1500  0.1285  0.1600  0.1800     176 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.00   12.00   16.00   15.83   18.00   29.00     176 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.is_derived_from_ffpe:
  no NA's 
 372  176 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    2.00    5.00    5.61    8.00   12.00     176 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.376   6.000  12.000     176 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   2.080   3.740   3.126   4.270   4.810     176 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.source_center:
  23 NA's 
 372  176 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   20.00   26.70   23.87   26.70   26.70     176 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.5.year_of_shipment:
2010 2011 NA's 
 145  227  176 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.additional_studies.additional_study.disease_code:
fppp NA's 
   3  545 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.additional_studies.additional_study.project_code:
tcga NA's 
   3  545 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              3             545 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 97500622  99029439  99067536 103646220 100476682 160356063       417 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.center_id:
   2    5    9 NA's 
  24   48   59  417 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.1400  0.1215  0.1600  0.1700     417 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   18.00   20.00   21.28   20.00   31.00     417 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.is_derived_from_ffpe:
  no NA's 
 131  417 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   8.000   8.000   9.298  12.000  12.000     417 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   2.000   4.000   4.534   6.000  12.000     417 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.930   1.070   2.080   2.577   4.010  10.720     417 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.source_center:
  23 NA's 
 131  417 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   13.34   13.34   20.36   26.70  129.00     417 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.6.year_of_shipment:
2010 2011 2012 2013 NA's 
 117    3    2    9  417 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.additional_studies.additional_study.disease_code:
fppp NA's 
   4  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.additional_studies.additional_study.project_code:
tcga NA's 
   4  544 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              4             544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.biospecimen_barcode_bottom:
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
 99054847 105041336 132267439 131367604 145675746 167811797       531 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.center_id:
   1    5    9 NA's 
   3    6    8  531 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
 0.0700  0.0800  0.1300  0.1153  0.1500  0.1700     531 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.day_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  13.00   16.00   20.00   21.53   26.00   31.00     531 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.is_derived_from_ffpe:
  no NA's 
  17  531 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.month_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   3.000   7.000   5.588   9.000  12.000     531 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.plate_column:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  1.000   1.000   3.000   3.706   7.000   8.000     531 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.quantity:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.870   1.400   3.200   3.014   4.010   6.800     531 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.source_center:
  23 NA's 
  17  531 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.volume:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   6.70   20.00   26.70   26.29   40.00   40.00     531 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.7.year_of_shipment:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2010    2011    2012    2012    2013    2015     531 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.additional_studies.additional_study.disease_code:
fppp NA's 
   4  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.additional_studies.additional_study.project_code:
tcga NA's 
   4  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_barcode:
tcga-bk-a0ca-01a-21d-a27b-05 tcga-bk-a0cc-01a-21d-a275-01 
                           1                            1 
tcga-bk-a139-01a-11d-a27b-05 tcga-bk-a26l-01a-11d-a27b-05 
                           1                            1 
                        NA's 
                         544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_aliquot_uuid:
02af7906-342c-4e78-bfa8-f65a65652563 16265fc6-60ae-4873-b71e-feb25d3c94b6 
                                   1                                    1 
bb8680b9-5753-4ee4-8260-e9373fd4aaa7 f4d7d989-8926-4fbd-acc4-ab39a6d82030 
                                   1                                    1 
                                NA's 
                                 544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              4             544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.biospecimen_barcode_bottom:
132159752 132160009 132160024 145675723      NA's 
        1         1         1         1       544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.center_id:
   1    5 NA's 
   1    3  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.concentration:
0.13 0.14 0.16 0.17 NA's 
   1    1    1    1  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.day_of_shipment:
  13   14 NA's 
   3    1  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.is_derived_from_ffpe:
  no NA's 
   4  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.month_of_shipment:
   3 NA's 
   4  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_column:
   1    3 NA's 
   3    1  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_id:
a275 a27b NA's 
   1    3  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.plate_row:
   a    f    g    h NA's 
   1    1    1    1  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.quantity:
0.87 0.94 1.07 1.14 NA's 
   1    1    1    1  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.source_center:
  23 NA's 
   4  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.volume:
 6.7 NA's 
   4  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.8.year_of_shipment:
2013 NA's 
   4  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.additional_studies.additional_study.disease_code:
fppp NA's 
   1  547 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.additional_studies.additional_study.project_code:
tcga NA's 
   1  547 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_barcode:
tcga-bk-a0ca-01a-21d-a46w-08 tcga-bk-a0cc-01a-21d-a27b-05 
                           1                            1 
tcga-bk-a139-01a-11d-a46w-08 tcga-bk-a26l-01a-11d-a46w-08 
                           1                            1 
                        NA's 
                         544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_aliquot_uuid:
57821c5a-ed19-4d9b-98f7-dd9e1fb39732 6dccfe1c-ac6e-470b-ae4c-0ee22f53a451 
                                   1                                    1 
88a9c649-5b32-4523-ab0a-ffabd934434d 8a403346-57d2-48e3-81c7-25ca892f33a9 
                                   1                                    1 
                                NA's 
                                 544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.bcr_biospecimen_canonical_reasons.bcr_canonical_reason:
ffpe validation            NA's 
              1             547 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.biospecimen_barcode_bottom:
132159796 145674263 145674264 145674287      NA's 
        1         1         1         1       544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.center_id:
   5    8 NA's 
   1    3  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.concentration:
0.07 0.08 0.14 NA's 
   1    2    1  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.day_of_shipment:
   3   13 NA's 
   3    1  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.is_derived_from_ffpe:
  no NA's 
   4  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.month_of_shipment:
   3    8 NA's 
   1    3  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_column:
   3    9   10 NA's 
   1    1    2  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_id:
a27b a46w NA's 
   1    3  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.plate_row:
   a    d    f NA's 
   2    1    1  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.quantity:
0.94  1.4  1.6 NA's 
   1    1    2  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.source_center:
  23 NA's 
   4  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.volume:
 6.7   20 NA's 
   1    3  544 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.9.year_of_shipment:
2013 2015 NA's 
   1    3  544 



patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.biospecimen_barcode_bottom:
167780543      NA's 
        1       547 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.center_id:
  1  23 
  1 547 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.concentration:
0.15 NA's 
   1  547 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.day_of_shipment:
  14 NA's 
   1  547 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.is_derived_from_ffpe:
  no NA's 
   1  547 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.month_of_shipment:
   5 NA's 
   1  547 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_column:
   1 NA's 
   1  547 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_id:
a402 yyyy 
   1  547 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.plate_row:
   a NA's 
   1  547 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.quantity:
1.01 NA's 
   1  547 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.source_center:
 23 
548 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.volume:
 6.7 NA's 
   1  547 

patient.samples.sample.portions.portion.analytes.analyte.aliquots.aliquot.year_of_shipment:
2014 NA's 
   1  547 

patient.samples.sample.portions.portion.analytes.analyte.amount:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  10.77   54.38   71.76   72.54   91.52  145.04       2 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type:
dna 
548 

patient.samples.sample.portions.portion.analytes.analyte.analyte_type_id:
  d 
548 



patient.samples.sample.portions.portion.analytes.analyte.concentration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.1300  0.1500  0.1600  0.1582  0.1700  0.1800 

patient.samples.sample.portions.portion.analytes.analyte.dna.normal_tumor_genotype_match:
yes 
548 

patient.samples.sample.portions.portion.analytes.analyte.dna.pcr_amplification_successful:
yes 
548 

patient.samples.sample.portions.portion.analytes.analyte.gel_image_file:
https://sharedoc.nchri.org/tcga-bcr/default.aspx 
                                             548 

patient.samples.sample.portions.portion.analytes.analyte.is_derived_from_ffpe:
 no 
548 

patient.samples.sample.portions.portion.analytes.analyte.protocols.protocol.experimental_protocol_type:
adna preparation type 
                  548 

patient.samples.sample.portions.portion.analytes.analyte.spectrophotometer_method:
picogreen   uv spec 
      186       362 

patient.samples.sample.portions.portion.analytes.analyte.subportion_sequence:
   1    2    3 NA's 
 491   52    2    3 



patient.samples.sample.portions.portion.day_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    7.00   14.00   14.49   22.00   31.00 

patient.samples.sample.portions.portion.is_ffpe:
 no 
548 

patient.samples.sample.portions.portion.month_of_creation:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   3.000   6.000   6.557  10.000  12.000 

patient.samples.sample.portions.portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
     11      11      11      13      11      51 

patient.samples.sample.portions.portion.portion_sequence:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   1.00    1.00    1.00    1.19    1.00    5.00      10 




patient.samples.sample.portions.portion.slides.slide.2.is_derived_from_ffpe:
  no NA's 
 194  354 

patient.samples.sample.portions.portion.slides.slide.2.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    3.00   10.96   20.00   90.00     355 

patient.samples.sample.portions.portion.slides.slide.2.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   3.114   1.000  30.000     355 

patient.samples.sample.portions.portion.slides.slide.2.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   3.000   5.461  10.000  30.000     355 

patient.samples.sample.portions.portion.slides.slide.2.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   6.016  10.000  57.000     355 

patient.samples.sample.portions.portion.slides.slide.2.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   4.668   5.000  59.000     355 

patient.samples.sample.portions.portion.slides.slide.2.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   5.000   9.435  15.000  67.000     355 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   70.00   80.00   80.46   95.00  100.00     355 

patient.samples.sample.portions.portion.slides.slide.2.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00   70.00   80.00   79.31   90.00  100.00     355 

patient.samples.sample.portions.portion.slides.slide.2.section_location:
bottom    top   NA's 
     8    186    354 




patient.samples.sample.portions.portion.slides.slide.is_derived_from_ffpe:
 no 
548 

patient.samples.sample.portions.portion.slides.slide.percent_lymphocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    4.00   12.95   20.00   90.00       1 

patient.samples.sample.portions.portion.slides.slide.percent_monocyte_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   5.049   6.000  40.000       1 

patient.samples.sample.portions.portion.slides.slide.percent_necrosis:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   5.000   5.605  10.000  60.000       1 

patient.samples.sample.portions.portion.slides.slide.percent_neutrophil_infiltration:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   1.000   6.899  10.000  57.000       1 

patient.samples.sample.portions.portion.slides.slide.percent_normal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  0.000   0.000   0.000   3.576   0.000  70.000       1 

patient.samples.sample.portions.portion.slides.slide.percent_stromal_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   0.00    0.00    7.00   10.86   20.00   92.00       1 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_cells:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   5.00   70.00   80.00   80.04   90.00  100.00       1 

patient.samples.sample.portions.portion.slides.slide.percent_tumor_nuclei:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
   2.00   70.00   80.00   79.52   90.00  100.00       1 

patient.samples.sample.portions.portion.slides.slide.section_location:
bottom    top 
   188    360 

patient.samples.sample.portions.portion.weight:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  10.00   30.00   30.00   28.78   30.00   40.00 

patient.samples.sample.portions.portion.year_of_creation:
2010 2011 2012 2013 2014 
 213  251   45   34    5 


patient.samples.sample.portions.shipment_portion.center_id:
  20 NA's 
 440  108 

patient.samples.sample.portions.shipment_portion.is_ffpe:
  no NA's 
 440  108 


patient.samples.sample.portions.shipment_portion.portion_number:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  11.00   21.00   21.00   20.28   21.00   41.00     108 

patient.samples.sample.portions.shipment_portion.portion_sequence:
   1    2    3    4 NA's 
  47  377    5    4  115 


patient.samples.sample.portions.shipment_portion.shipment_portion_day_of_shipment:
  11   14   30   31 NA's 
 116   88   36  200  108 

patient.samples.sample.portions.shipment_portion.shipment_portion_month_of_shipment:
   5    7   10   11 NA's 
 200   36  116   88  108 

patient.samples.sample.portions.shipment_portion.shipment_portion_year_of_shipment:
2010 2011 2012 2014 NA's 
   1  315   88   36  108 

patient.samples.sample.sample_type:
primary tumor 
          548 

patient.samples.sample.sample_type_id:
  1 
548 

patient.samples.sample.vial_number:
  a   b 
542   6 

patient_type:
Tumor  NA's 
  373   175 

age:
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  31.00   56.00   63.00   63.13   70.00   90.00     175 


histology:
Endometrioid        Mixed       Serous         NA's 
         307           13           53          175 

tumor_grade:
Grade 1 Grade 2 Grade 3    NA's 
     89     106     178     175 


vital_status.y:
DECEASED   LIVING  Unknown     NA's 
      39      332        2      175 


disease_status_at_lfu:
  NoDisease     Unknown WithDisease        NA's 
        299          29          45         175 

recurred_progressed:
     No Unknown     Yes    NA's 
    279      22      72     175 



ethnicity.y:
    HISPANIC OR LATINO NOT HISPANIC OR LATINO                Unknown 
                     8                    257                    108 
                  NA's 
                   175 



msi_status_7_marker_call:
Indeterminant         MSI-H         MSI-L           MSS          NA's 
            3           127            20           223           175 

cna_cluster_k4:
   1    2    3    4 NA's 
  86  129   55   93  185 

mrna_expression_cluster:
   1    2    3 NA's 
 126  111   96  215 

}}
\keyword{datasets}
