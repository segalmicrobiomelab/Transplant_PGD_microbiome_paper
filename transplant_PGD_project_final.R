## Segal Lab
## Programmer: Kendrew Wong
## Project: Transplant PGD

#clear all objects
rm(list=ls(all.names=T))
gc()

##open up the R script with all of the function created for the project
source("C:/Users/kendr/OneDrive/Desktop/research_transplant/transplant_final_R_files/KW_functions_v1j.R")

#packages needed for the script
pkg_list <- c("devtools", "tidyverse", "readr", "readtext", "vegan", "ade4", "biomformat", "cachem", 
              "utf8", "backports", "colorspace", "rhdf5", "DelayedArray","Biobase", "Biostrings", "magick",
              "phyloseq", "ape", "phangorn", "ggpubr", "decontam", "ggplot2", "reshape2",
              "tidyr", "matrixStats", "DESeq2", "edgeR", "limma", "Glimma", "RColorBrewer",
              "pheatmap", "ggrepel", "scales", "data.table", "fBasics", "forcats", "maptools", 
              "lubridate", "boot", "table1", "stringr", "papaja", "flextable",
              "Gmisc", "glue", "htmlTable", "grid", "magrittr", "rmarkdown", "plotly",
              "microbiome", "DT", "webshot", "lubridate", "png", "RCurl", "cowplot", "janitor",
              "optmatch", "MatchIt", "decontam", "qdap", "stringr","openxlsx", "chisq.posthoc.test", 
              "FSA", "cobalt", "ggplotify", "grid", "gridExtra", "combinat", "mixOmics", "gplots", "plyr", 
              "readxl", "DESeq2", "mia", "microbiomeMarker", "jpeg", 
              "mia", "miaViz", "corrplot", "ggcorrplot", "cowplot", "gridGraphics",
              "ade4", "ggthemes", "Hmisc", "rdist", "rstatix", "ggpubr", "pdftools", "convertGraph")

#installing all packages needed  
install_packages(pkg_list)

#loading packages
for (i in pkg_list){
  eval(bquote(library(.(i))))
}

#install qiime2R from github directly
devtools::install_github("jbisanz/qiime2R")
library(qiime2R)
devtools::install_github("vmikk/metagMisc")
library(metagMisc)
devtools::install_github("Sebastien-Le/YesSiR")
library(YesSiR)
devtools::install_github("thomasp85/patchwork")
library(patchwork)
devtools::install_github("david-barnett/microViz")
library(microViz)
devtools::install_github("zdk123/pulsar")
library(pulsar)
devtools::install_github("zdk123/SpiecEasi")
library(SpiecEasi)

###############Setting up meta file###############
setwd("C:/Users/kendr/OneDrive/Desktop/research_transplant/transplant_raw/") #set the directory to the raw file directory 

#mapping key data
transplant_meta<-read.delim("msq.master.map.transplant.txt") ##reading mapping data (missing key demographics variables)

#remove re-transplant subject TX_0057_0021
transplant_meta<-transplant_meta[transplant_meta$SubjID != "TX_0057_0021",] #10 observations were removed as a result

#edit mapping file date which is entered incorrectly
transplant_meta$Date.collection[transplant_meta$SampleID=="TX.0052.Supraglottic.3"] <- "1.12.2022" #from 1.12.2012
transplant_meta$Date.collection[transplant_meta$SampleID=="TX.0029.BAL.RML.DL.5"] <- "3.31.2021" #from 3.30.2021
transplant_meta$Date.collection[transplant_meta$SampleID=="TX.0037.Supraglottic.1"] <- "6.11.2021" #from 6.10.2021
transplant_meta$Date.collection[transplant_meta$SampleID=="U.CF.0002.BAL.RLL.DL.1.219.276"] <- "7.5.2019" #from 7.8.2019

###keeping only identifier variables from the old mapping files
transplant_mapping_1<-transplant_meta %>% dplyr::select(c("SampleID", "SampleID.unique", "BarcodeSequence", "LinkerPrimerSequence", 
                                                          "Primer.Plate", "Host", "Amp.Well.Plate", 
                                                          "MSQ", "LANE", "PI", "SubjID", "Date.collection", "Description", "Protocol"))

#setting up dataset for timeline graph- part 1
#only taking out the sample ID, Subject ID and date of collection to make the timeline graph which display all of the samples taken
transplant_sample_timeline <- dplyr::select(transplant_meta,c(SampleID, SubjID,Date.collection)) 
#dropping rows with na date of collection (only the MOC, blank and DFW should be dropped)
transplant_sample_timeline<-transplant_sample_timeline[!(transplant_sample_timeline$Date.collection=="n.a"),]
#change Date.collection to a date variable
transplant_sample_timeline$Date.collection<- mdy(transplant_sample_timeline$Date.collection)
#keeping only subject ID and date of collection for samples which were sequenced
transplant_sample_timeline <- dplyr::select (transplant_sample_timeline,c(SubjID,Date.collection))
#keeping only one observation for each sample collected (this would effectively removed if multiple samples were collected on the same date)
transplant_sample_timeline<-unique(transplant_sample_timeline)

#transplant_sample_meta contains all samples and their unique ID
transplant_sample_meta<-transplant_meta %>% dplyr::select(c("SampleID", "SampleID.unique", "BarcodeSequence", "LinkerPrimerSequence", 
                                                            "Primer.Plate", "Host", "Amp.Well.Plate", 
                                                            "MSQ", "LANE", "PI", "SubjID", "Date.collection", "Description", "Protocol"))

#transplant_sample_meta: excluded samples without subject ID
transplant_sample_meta<-transplant_sample_meta %>% dplyr::filter_at(vars(matches("SubjID")), all_vars(str_detect(., "TX|UNIV|CF")))
transplant_sample_meta$transplant_id <- transplant_sample_meta %>% dplyr::group_indices(SubjID) 
transplant_sample_meta$Date.collection <- str_replace_all(transplant_sample_meta$Date.collection,"\\.","/")
## sort by new ID "transplant_ID" and date of collection
transplant_sample_meta <- transplant_sample_meta %>% dplyr::arrange(transplant_id, mdy(Date.collection)) 

transplant_baseline_meta<- transplant_sample_meta %>%
  dplyr::group_by(transplant_id) %>%
  dplyr::mutate(my_ranks = order(transplant_id, mdy(Date.collection)))

#creating a key dataset with old identifier with the new transplant_id so the other redcap raw data can use the same new transplant_id
transplant_key_id <-  dplyr::select (transplant_sample_meta,c(SubjID,transplant_id))
transplant_key_id <- unique(transplant_key_id) ####TX_0031 does not exist in the mapping file (sample did not get sequenced for some reason)
transplant_key_id %>% as.data.frame(row.names = 1:nrow(.)) #resetting rowname 

###############clinical information- red cap###############
transplant_meta2<-read.csv("TXProject2020_DATA_2022-12-23_1234.csv") ##reading raw red cap data
transplant_meta2<-subset(transplant_meta2, redcap_data_access_group %in% c("consented_patients")) ##keeping only consented patients
transplant_meta2 <- transplant_meta2 %>% dplyr::rename(SubjID = ntm_id_01)
####TX_0016 did not have CMV antibody reported in the export even though in red cap. manually correcting
transplant_meta2$cmv_antibody[transplant_meta2$SubjID == "TX_0016" & transplant_meta2$redcap_event_name== "baseline_tx_data_arm_1"] <- 1

####remove pt who was re-transplanted TX_0057_0021
transplant_meta2<-transplant_meta2[transplant_meta$SubjID != "TX_0057_0021",]
transplant_meta2$SubjID[transplant_meta2$SubjID == 'UNIV_CF_0002_TX'] <- "U_CF_0002"
transplant_meta2$SubjID <- gsub('_TX', '', transplant_meta2$SubjID)
transplant_meta2$SubjID <- toupper(transplant_meta2$SubjID)
transplant_meta2 <- merge(transplant_meta2,transplant_key_id,by=c("SubjID")) #merging in the new transplant_id
#generate a variable to indicate whether the sample is biopsy
transplant_meta2 <- transplant_meta2 %>% mutate(biopsy= case_when((is.na(acute_cellular_rejection)~ 0), TRUE ~ 1))
#transplant_mapping_2 contains information regarding the sample whether it is biopsy or not
transplant_mapping_2<-transplant_meta2
transplant_mapping_2$Date.collection<-mdy(transplant_meta2$date_2)

##############################PGD sample determination############################## 
#transplant_date_PGD has the transplant date for everyone
transplant_date_PGD<-subset(transplant_meta2,select=c("SubjID", "ts_date_1"))
transplant_date_PGD$transplant_date <-mdy(transplant_date_PGD$ts_date_1) 
transplant_date_PGD<-transplant_date_PGD[!is.na(transplant_date_PGD$transplant_date),]
#transplant_collect_date_PGD has the collections dates for all samples which were sequenced
transplant_collect_date_PGD<-subset(transplant_sample_meta, select=c("SubjID","Date.collection"))
transplant_collect_date_PGD<- unique(transplant_collect_date_PGD)
transplant_collect_date_PGD$Date.collection<-mdy(transplant_collect_date_PGD$Date.collection)

transplant_PGD_sample_selection1<-merge(transplant_collect_date_PGD,transplant_date_PGD, by="SubjID" )
transplant_PGD_sample_selection1$obs_day<-transplant_PGD_sample_selection1$Date.collection-transplant_PGD_sample_selection1$transplant_date

transplant_PGD_sample_selection2<-transplant_PGD_sample_selection1[transplant_PGD_sample_selection1$obs_day<=3,] #remove observation with more than 3 days from time of transplant
transplant_PGD_sample<-unique(subset(transplant_PGD_sample_selection2, select=c("SubjID")))
transplant_PGD_sample$PGD_sample<-"yes"
#transplant_PGD_sample contains all subject with bronch within first 3 days (obs_day <= 3)
transplant_final_PGD_samplelist<-pull(unique(transplant_PGD_sample),SubjID) #transplant_final_PGD_samplelist is a vector of SubjID which should be included in the final analysis for PGD (subject with bronch within 3 days after transplant)

#setting up dataset for timeline graph- part 2
transplant_meta_timeline_var <- transplant_meta2 %>%dplyr::select(c("SubjID", "date_of_death", "ts_date_1", "date_2", "acute_cellular_rejection"))

transplant_meta_timeline_var$Date.collection <-mdy(transplant_meta_timeline_var$date_2)
transplant_meta_timeline_var$transplant_date <-mdy(transplant_meta_timeline_var$ts_date_1) 
transplant_meta_timeline_var$death_date <-mdy(transplant_meta_timeline_var$date_of_death)
                                              
transplant_meta_timeline_var_collected<-transplant_meta_timeline_var %>%dplyr::select(c("SubjID", "Date.collection", "acute_cellular_rejection"))                                              
transplant_meta_timeline_var_collected<-transplant_meta_timeline_var_collected[!is.na(transplant_meta_timeline_var_collected$Date.collection),]         
transplant_meta_timeline_var_collected$acute_cellular_rejection <-as.numeric(transplant_meta_timeline_var_collected$acute_cellular_rejection) #change variable from factor to numeric
transplant_meta_timeline_var_collected<-transplant_meta_timeline_var_collected %>% dplyr::mutate(biopsy = case_when((!is.na(acute_cellular_rejection)) ~ 1, TRUE~ 0 ))
#changing acute_cellular_rejection 5 to 0 because 5 indicates A0
transplant_meta_timeline_var_collected$acute_cellular_rejection <- replace(transplant_meta_timeline_var_collected$acute_cellular_rejection, transplant_meta_timeline_var_collected$acute_cellular_rejection==5, 0) 

transplant_meta_timeline_var_base<-transplant_meta_timeline_var %>%dplyr::select(c("SubjID","transplant_date", "death_date"))
transplant_meta_timeline_var_base<-transplant_meta_timeline_var_base[!is.na(transplant_meta_timeline_var_base$transplant_date),]         

#######merging back the above variables which are obtained from the red cap back to transplant_sample_timeline which has samples which are sequenced  
transplant_sample_timeline2 <- merge(transplant_sample_timeline, transplant_meta_timeline_var_collected, by=c("SubjID", "Date.collection"), all.x=T) ###keeping based on the sequenced data
transplant_sample_timeline_final <- merge(transplant_sample_timeline2, transplant_meta_timeline_var_base, by=c("SubjID")) ###keeping based on the sequenced data

transplant_meta_timeline_var_ACR <- transplant_sample_timeline_final %>% dplyr::mutate(ACR_2more = case_when((acute_cellular_rejection >= 2 & !is.na(acute_cellular_rejection)) ~ 1, 
                                                                                                             (acute_cellular_rejection < 2 & !is.na(acute_cellular_rejection)) ~ 0))
#####deleting observation more than 13 months (405 days)
transplant_meta_timeline_var_ACR$obs_day<-transplant_meta_timeline_var_ACR$Date.collection-transplant_meta_timeline_var_ACR$transplant_date
transplant_meta_timeline_var_ACR<- transplant_meta_timeline_var_ACR[transplant_meta_timeline_var_ACR$obs_day <=405,]
transplant_meta_timeline_var_ACR<-merge(transplant_meta_timeline_var_ACR, transplant_key_id, by="SubjID")
transplant_meta_timeline_var_ACR <- transplant_meta_timeline_var_ACR %>% dplyr::filter(!is.na(acute_cellular_rejection)) #nonmissing acute cellular rejection
####generating acute cellular rejection
transplant_baseline_meta_highest_ACR<- transplant_meta_timeline_var_ACR %>%
  dplyr::group_by(transplant_id) %>%
  dplyr::summarize(transplant_highest_ACR = max(acute_cellular_rejection))

transplant_meta_timeline_var_ACR <- transplant_meta_timeline_var_ACR %>% dplyr::select(c("SubjID","ACR_2more"))
transplant_meta_timeline_var_ACR <- transplant_meta_timeline_var_ACR %>% dplyr::group_by(SubjID) %>% dplyr::summarise (ACR_2more_ever=max(ACR_2more))                                                                                                                    
transplant_meta_timeline_var_ACR <- transplant_meta_timeline_var_ACR %>% dplyr::mutate(ACR_2more_ever = case_when(ACR_2more_ever==1 ~ "ACR grades 2-4", TRUE~ "ACR grades 0-1" ))
transplant_meta_timeline_var_ACR$transplant_ACR_2more_ever <- as.factor(transplant_meta_timeline_var_ACR$ACR_2more_ever)
transplant_sample_timeline_final <- merge(transplant_sample_timeline_final, transplant_meta_timeline_var_ACR, by=c("SubjID")) ###keeping based on the sequenced data
transplant_sample_timeline_final$death_day<-transplant_sample_timeline_final$death_date-transplant_sample_timeline_final$transplant_date
transplant_sample_timeline_final$obs_day<-transplant_sample_timeline_final$Date.collection-transplant_sample_timeline_final$transplant_date
transplant_sample_timeline_final$acute_cellular_rejection[is.na(transplant_sample_timeline_final$acute_cellular_rejection)] <- "none"
transplant_sample_timeline_final$acute_cellular_rejection<-as.factor(transplant_sample_timeline_final$acute_cellular_rejection)
########### determine biopsy timing 
#remove all the non-biopsy sample 
transplant_sample_timeline_final_biopsysample<-transplant_sample_timeline_final[transplant_sample_timeline_final$acute_cellular_rejection!="none",]
####get a variable that say whether the biopsy result for the sample is ACR2+ or not
transplant_sample_timeline_final_biopsysample <- transplant_sample_timeline_final_biopsysample %>% dplyr::left_join(transplant_sample_timeline_final_biopsysample %>%
                                                                                          dplyr::mutate(biopsy_ACR2more=case_when((acute_cellular_rejection=="2" | acute_cellular_rejection=="3" | acute_cellular_rejection=="4")~1, TRUE~0)))
transplant_sample_biopsyresult <- transplant_sample_timeline_final_biopsysample %>% dplyr::select(c("SubjID","Date.collection","biopsy_ACR2more"))

label(transplant_sample_biopsyresult$biopsy_ACR2more) <- "sample biopsy ACR grade"
transplant_sample_biopsyresult$biopsy_ACR2more <- factor(transplant_sample_biopsyresult$biopsy_ACR2more, 
                                                  levels=c(0,1),
                                                  labels=c("biopsy ACR A0 or A1", 
                                                           "biopsy ACR A2 or above"))
transplant_sample_timeline_final_biopsysample <- transplant_sample_timeline_final_biopsysample %>% dplyr::left_join(transplant_sample_timeline_final_biopsysample %>% 
                                                                                                                      dplyr::group_by(SubjID) %>% 
                                                                                                                      dplyr::arrange(obs_day) %>%
                                                                                                                      dplyr::mutate(biopsy_order_T = row_number())) 
transplant_sample_timeline_final_biopsysample2 <- dplyr::select(transplant_sample_timeline_final_biopsysample,c("SubjID","biopsy_order_T", "obs_day"))
transplant_sample_timeline_final_biopsysample3 <- transplant_sample_timeline_final_biopsysample2 %>% spread(biopsy_order_T,obs_day)
transplant_sample_timeline_final_biopsysample3 <- transplant_sample_timeline_final_biopsysample3 %>% dplyr::rename(biopsy_1=2,biopsy_2=3,
                                                                                                            biopsy_3=4,biopsy_4=5,
                                                                                                            biopsy_5=6,biopsy_6=7)

transplant_sample_timeline_final_biopsysample3$biopsy_1 <- plyr::round_any(as.integer(transplant_sample_timeline_final_biopsysample3$biopsy_1),30)
transplant_sample_timeline_final_biopsysample3$biopsy_2 <- plyr::round_any(as.integer(transplant_sample_timeline_final_biopsysample3$biopsy_2),30)

####generating HLA
transplant_baseline_HLA <-dplyr::select (transplant_meta2,c(transplant_id,hla_assay___1, class1_result, hla_assay___2, class2_result))
transplant_baseline_HLA <- transplant_baseline_HLA[(transplant_meta2$hla_assay___1==1 | transplant_meta2$hla_assay___2==1),] 
transplant_baseline_HLA <-dplyr::select (transplant_baseline_HLA,c(transplant_id, class1_result, class2_result))

transplant_baseline_HLA <- transplant_baseline_HLA %>% dplyr::group_by(transplant_id) %>% dplyr::summarise(class1_result=max(class1_result), class2_result=max(class2_result))

transplant_baseline_HLA <- transplant_baseline_HLA %>% dplyr::mutate(transplant_HLA = case_when((class1_result ==1 | class2_result==1) ~ "Yes", 
                                                                                         TRUE ~ "No"))
transplant_baseline_HLA <- transplant_baseline_HLA %>% dplyr::mutate(transplant_HLA_C1 = case_when((class1_result ==1) ~ "Yes", 
                                                                                            TRUE ~ "No"))
transplant_baseline_HLA <- transplant_baseline_HLA %>% dplyr::mutate(transplant_HLA_C2 = case_when((class2_result==1) ~ "Yes", 
                                                                                            TRUE ~ "No"))
####Baseline variables- clinical 
transplant_baseline_meta2<-transplant_meta2 %>% dplyr::filter_at(vars(matches("redcap_event_name")), all_vars(str_detect(., "baseline_"))) #keeping only baseline observations
####generate key clinical features/demographics information used for table 1
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_gender = case_when(sex_2 == 1 ~ "Male", sex_2 == 0 ~ "Female"))

transplant_baseline_meta2$transplant_age <- transplant_baseline_meta2$age_at_time_of_transplant
transplant_baseline_meta2$transplant_bmi <- transplant_baseline_meta2$bmi
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_race = case_when(race_2___4 == 1 ~ "White", 
                                                                                                    race_2___3 == 1 ~ "Black", 
                                                                                                    race_2___1 == 1 ~ "Asian", 
                                                                                                    race_2___8 == 1 ~ "More than one or not reported", 
                                                                                                    TRUE ~ "More than one or not reported")) #all other cases
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_race_detail = case_when(race_2___0 == 1 ~ "Native American/Alaska Native", 
                                                                                                     race_2___1 == 1 ~ "Asian", 
                                                                                                     race_2___2 == 1 ~ "Native Hawaiian or Other Pacific Islander", 
                                                                                                     race_2___3 == 1 ~ "Black or African American",
                                                                                                     race_2___4 == 1 ~ "White", 
                                                                                                     race_2___5 == 1 ~ "More Than One Race", 
                                                                                                     race_2___6 == 1 ~ "Hispanic",
                                                                                                     race_2___7 == 1 ~ "Unknown/Not Reported", 
                                                                                                     race_2___8 == 1 ~ "Other"))
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_ethnicity = case_when(ethnicity___1 == 1 ~ "Hispanic", 
                                                                                                  ethnicity___2 == 1 ~ "Non-Hispanic", 
                                                                                                  TRUE ~ "Unknown")) #all other cases
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_diagnosis = case_when(las_diagnosis_group___1 == 1 ~ "Obstructive Lung Disease", 
                                                                                                   las_diagnosis_group___2 == 1 ~ "Pulmonary Vascular Disease",
                                                                                                   las_diagnosis_group___3 == 1 ~ "Cystic Fibrosis",
                                                                                                   las_diagnosis_group___4 == 1 ~ "Restrictive Lung Disease")) 
transplant_baseline_meta2$transplant_LAS<- transplant_baseline_meta2$las_score_1
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_reflux = case_when(findings_aspirationtxp___1 == 1 ~ "Yes", ##diagnostic study showing reflux
                                                                                                TRUE ~ "No"))
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_preT_aspiration = case_when(aspiration_txp == 0 ~ "Yes", 
                                                                                                        aspiration_txp == 1 ~ "No",
                                                                                                        aspiration_txp == 2 ~ "Unknown"))
#transplant_preT_CMV_antibody: receipient CMV antibody test
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_preT_CMV_antibody = case_when(cmv_antibody == 1 ~ "Positive", 
                                                                                                           cmv_antibody == 2 ~ "Negative"))
#transplant_preT_30d_antimicro: on antibiotics/antifungal/antivirals 30 days preceding to or at time of transplant (excluding peri-op antibiotics)
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_preT_30d_antimicro = case_when(ax_pt == 0 ~ "Yes", 
                                                                                                            ax_pt == 1 ~ "No"))
#transplant_preT_30d_immuno: on high dose steroids or immunosuppression 30 days preceiding to or at the time of transplant 
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_preT_30d_immuno = case_when(immunos_steroid_1 == 0 ~ "Yes", 
                                                                                                         immunos_steroid_1 == 1 ~ "No"))
#transplant_preT_ecmo: before transplant ECMO 
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_preT_ecmo = case_when(ecmo_1 == 0 ~ "Yes", 
                                                                                                   ecmo_1 == 1 ~ "No"))
transplant_baseline_meta2$transplant_preT_6minwalk<-transplant_baseline_meta2$six_min_walk  
transplant_baseline_meta2$transplant_preT_FVC<-transplant_baseline_meta2$fvc_percent
transplant_baseline_meta2$transplant_preT_FEV<-transplant_baseline_meta2$fev1_percent
transplant_baseline_meta2$transplant_preT_FEV_FVC<-transplant_baseline_meta2$fev1fvc
transplant_baseline_meta2$transplant_preT_DLCO<-transplant_baseline_meta2$dlco_adj
transplant_baseline_meta2$transplant_preT_hgb<-transplant_baseline_meta2$hemoglobin
transplant_baseline_meta2$transplant_preT_bicarb<-transplant_baseline_meta2$hco3
transplant_baseline_meta2$transplant_preT_mPAP<-transplant_baseline_meta2$mpap
transplant_baseline_meta2$transplant_preT_PCWP<-transplant_baseline_meta2$pcwp
transplant_baseline_meta2$transplant_preT_PVR<-transplant_baseline_meta2$pvr
##transplant_donor_smoker20yr: donor smoke for 20+ years
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_donor_smoker20yr = case_when(t_smoking_1 == 0 ~ "Yes", 
                                                                                                          t_smoking_1 == 1 ~ "No"))
##transplant_donor_CMV_antibody: donor CMV positive
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_donor_CMV_antibody = case_when(cmv_2 == 0 ~ "Positive", 
                                                                                                            cmv_2 == 1 ~ "Negative"))
##transplant_donor_pos_cx: donor positive culture
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_donor_pos_cx = case_when(pos_donorcx == 1 ~ "Yes", 
                                                                                                        pos_donorcx == 0 ~ "No"))

transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_heart_lung_txp = case_when(heart_lung_txp == 1 ~ "Yes", 
                                                                                                        heart_lung_txp == 0 ~ "No"))

transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_txp_type = case_when(transpl_type == 0 ~ "Bilateral", 
                                                                                                  transpl_type == 1 ~ "Single"))

transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_ecmo_txp = case_when(ecmo_periop == 1 ~ "Yes", 
                                                                                                  ecmo_periop == 0 ~ "No"))
transplant_baseline_meta2$transplant_ischemic_time<-transplant_baseline_meta2$ischemic_time
#transplant_surgery_crystalloid: OR crystalloid volume
transplant_baseline_meta2$transplant_surgery_crystalloid<-transplant_baseline_meta2$or_crystalloid_volume
#transplant_surgery_pRBC: OR pRBC volume
transplant_baseline_meta2$transplant_surgery_pRBC<-transplant_baseline_meta2$prbc_vol
#transplant_surgery_periop_abx: periop antibiotics 
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_surgery_periop_abx = case_when(ax_sx == 0 ~ "Yes", 
                                                                                                            ax_sx == 1 ~ "No"))
#transplant_induction: periop induction therapy choice
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_induction = case_when(induction_choice == 1 ~ "Tyroglobulin", 
                                                                                                   induction_choice == 2 ~ "Basiliximab",
                                                                                                   induction_choice == 3 ~ "Other"))
#transplant_periop_immunosup: periop immunosuppression used
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_periop_immunosup = case_when(imunnosup_0 == 0 ~ "Yes", 
                                                                                                          imunnosup_0 == 1 ~ "No"))
#transplant_post_ECMO: ECMO post transplant
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_post_ECMO = case_when(ecmo_2 == 0 ~ "Yes", 
                                                                                                   ecmo_2 == 1 ~ "No"))
#transplant_post_ext_time: time to first extubation 
transplant_baseline_meta2$transplant_post_ext_time<-transplant_baseline_meta2$time_to_first_extubation
#transplant_post_ICU: post op ICU stay
transplant_baseline_meta2$transplant_post_ICU<-transplant_baseline_meta2$icu_1
#transplant_post_LOS: post op length of stay (hospital days)
transplant_baseline_meta2$transplant_post_LOS<-transplant_baseline_meta2$hosp_1
#transplant_pgd24: Primary Graft Dysfunction within 24 hours 
transplant_baseline_meta2$transplant_pgd24 <-transplant_baseline_meta2$pgd_24h
#transplant_pgd48: Primary Graft Dysfunction within 48 hours 
transplant_baseline_meta2$transplant_pgd48 <-transplant_baseline_meta2$pgd_48h
#transplant_pgd72: Primary Graft Dysfunction within 72 hours 
transplant_baseline_meta2$transplant_pgd72 <-transplant_baseline_meta2$pgd_72h
#transplant_pgd72_0_12_3: Primary Graft Dysfunction within 72 hours in 3 categories- 0, 1+2 and 3
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_pgd72_0_12_3 = case_when(pgd_72h == 0 ~ "PGD_0", 
                                                                                                   (pgd_72h == 1 | pgd_72h == 2) ~ "PGD_1_2",
                                                                                                    pgd_72h == 3 ~ "PGD_3")) 
#transplant_pgd_any: if any PGD 48 or 72 hours
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_pgd_any = case_when((transplant_pgd48 >0 | transplant_pgd72 >0) ~ "Yes", 
                                                                                                 TRUE ~ "No"))
#transplant_pgd_more2_A48hr: if PGD is 2 or greater on 48 or 72 hours (after 48 hours) 
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_pgd_more2_A48hr = case_when((transplant_pgd48 >=2 | transplant_pgd72 >=2) ~ "Yes", 
                                                                                                 TRUE ~ "No"))
#transplant_pgd_3_A48hr: if PGD is 3 on 48 or 72 hours (after 48 hours)
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_pgd_3_A48hr = case_when((transplant_pgd48 ==3 | transplant_pgd72 ==3) ~ "Yes", 
                                                                                                 TRUE ~ "No"))
#transplant_pgd_more2_A72hr: if PGD is 2 or greater on 72 hours
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_pgd_more2_A72hr = case_when(transplant_pgd72 >=2 ~ "Yes", 
                                                                                                 TRUE ~ "No"))
#transplant_pgd_3_A72hr: if PGD is 3 on 72 hours 
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_pgd_3_A72hr = case_when(transplant_pgd72 ==3 ~ "Yes", 
                                                                                                 TRUE ~ "No"))

#transplant_pgd_3_48_72: if PGD is 3 on 48 OR 72 hours 
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_pgd_3_48_72 = case_when((transplant_pgd_3_A72hr =="Yes" | transplant_pgd_3_A48hr =="Yes")~ "Yes", 
                                                                                                           TRUE ~ "No"))

#transplant_pgd_12_A72hr: PGD is 1 or 2 on 72 hours
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_pgd_12_A72hr = case_when((transplant_pgd72 ==2 | transplant_pgd72 ==1)~ "Yes", 
                                                                                                     TRUE ~ "No"))

#transplant_any_ECMO: if any ECMO (pre, peri, post)
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_any_ECMO = case_when((transplant_preT_ecmo == "Yes" | 
                                                                                                              transplant_ecmo_txp == "Yes" | 
                                                                                                              transplant_post_ECMO == "Yes") ~ "Yes", 
                                                                                                     TRUE ~ "No"))
#transplant_fio2_72: FIO2 at 24 hours 
transplant_baseline_meta2$transplant_fio2_24 <- transplant_baseline_meta2$fio2_at_24_hours

#transplant_fio2_72: FIO2 at 48 hours 
transplant_baseline_meta2$transplant_fio2_48 <- transplant_baseline_meta2$fio2_at_48_hours

#transplant_fio2_72: FIO2 at 72 hours 
transplant_baseline_meta2$transplant_fio2_72 <- transplant_baseline_meta2$fio2_at_72_hours

#transplant_fio2_72_level: FIO2 leve at 72 hours  (high >30 low<30)
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_fio2_72_level = case_when((transplant_fio2_72 > 0.3) ~ 1,
                                                                                                       (transplant_fio2_72 <= 0.3) ~0))

#transplant_pgd_highest_48_72: highest PGD per subject (counting only 48 and 72 hours)
transplant_baseline_meta2$transplant_pgd_highest_48_72 <- pmax(transplant_baseline_meta2$pgd_48h,transplant_baseline_meta2$pgd_72h)
transplant_baseline_meta2$transplant_pgd_highest_48_72_v2 <-transplant_baseline_meta2$transplant_pgd_highest_48_72 #pgd_highest_48_72_v2 would keep the original separation of PGD 1 and 2. 

transplant_baseline_meta2$transplant_pgd_highest_48_72_v3 <-transplant_baseline_meta2$transplant_pgd_highest_48_72 #pgd_highest_48_72_v3 would condense 0 and 1. 
transplant_baseline_meta2$transplant_pgd_highest_48_72_v3[transplant_baseline_meta2$transplant_pgd_highest_48_72_v3==1] <- 0 
transplant_baseline_meta2$transplant_pgd_highest_48_72_v3[transplant_baseline_meta2$transplant_pgd_highest_48_72_v3==2] <- 1 
transplant_baseline_meta2$transplant_pgd_highest_48_72_v3[transplant_baseline_meta2$transplant_pgd_highest_48_72_v3==3] <- 2 
 
### consolidating 1 and 2 into a single categories. so you have 3 groups: 0, 1-2 and 3. 
transplant_baseline_meta2$transplant_pgd_highest_48_72[transplant_baseline_meta2$transplant_pgd_highest_48_72==2] <- 1 
transplant_baseline_meta2$transplant_pgd_highest_48_72[transplant_baseline_meta2$transplant_pgd_highest_48_72==3] <- 2 

###merging in acute cellular rejection (ACR) infection 
transplant_baseline_meta2 <- merge(transplant_baseline_meta2,transplant_baseline_HLA,by="transplant_id", all=T)

#subgroup for comparsion: 
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_moderatepgd = case_when(transplant_pgd48 >1 | transplant_pgd72 >1 ~ "Yes", 
                                                                                                     TRUE ~ "No"))
transplant_baseline_meta2 <- transplant_baseline_meta2 %>%dplyr::mutate(transplant_nopgd_72 = case_when(transplant_pgd72 ==0 ~ "Yes",
                                                                                                  transplant_pgd72 >=1  ~ "No"))

#keeping only variables with prefix transplant and the Subject ID (SubjID)
transplant_BL_meta_final <- transplant_baseline_meta2 %>%dplyr::select(starts_with(c("transplant_", "SubjID")))

transplant_BL_meta_final <- transplant_BL_meta_final %>%  rename_all(~stringr::str_replace(.,"^transplant_","")) #removing the prefix transplant_ so that the final baseline meta file variable names are clean 
              
#merging mapping file with new baseline meta file  
mapping_baseline <- merge(x=transplant_BL_meta_final, y=transplant_baseline_meta, by=c("SubjID"), all=T)
mapping_baseline <- mapping_baseline %>%dplyr::select(-c("id"))

first_bronch_date <- mapping_baseline %>% subset(my_ranks %in% c(1))
first_bronch_date <- first_bronch_date %>%dplyr::select(c("SubjID", "my_ranks", "Date.collection")) 
first_bronch_date <- first_bronch_date %>% dplyr::rename(first_bronch_date=my_ranks)
mapping_baseline <- merge(first_bronch_date, mapping_baseline, by=c("SubjID","Date.collection"), all=T)
mapping_baseline <- mapping_baseline %>%dplyr::select(-c("my_ranks"))
mapping_baseline$first_bronch_date[is.na(mapping_baseline$first_bronch)]<-0
mapping_baseline <- mapping_baseline %>% dplyr::mutate(first_bronch = case_when((grepl("BAL",SampleID)== T & first_bronch_date==1) ~ 1, TRUE ~ 0)) 

#add gender and age to the timeline file
transplant_meta_timline_char<-transplant_BL_meta_final %>%dplyr::select(c("SubjID", "gender", "age", "diagnosis"))
transplant_sample_timeline_final<-merge(transplant_sample_timeline_final, transplant_meta_timline_char, by=("SubjID"), all.x=T)

table(transplant_BL_meta_final$gender,transplant_BL_meta_final$race_detail )

#####figure out who is dead before 13 months
transplant_dead<-transplant_sample_timeline_final[transplant_sample_timeline_final$death_day<405 & !is.na(transplant_sample_timeline_final$death_day),]## keep only patient who is dead before 13 months
transplant_dead <- transplant_dead %>% dplyr::select(c("SubjID","death_day"))
transplant_dead <- unique(transplant_dead)
## keep only patient who is dead after 13 months or patient who never die
transplant_sample_timeline_final_keepDeath<-transplant_sample_timeline_final
transplant_sample_timeline_final<-transplant_sample_timeline_final[transplant_sample_timeline_final$death_day>=405 | is.na(transplant_sample_timeline_final$death_day),]
#setting working directory: where intermediate output would be placed
setwd("C:/Users/kendr/OneDrive/Desktop/research_transplant/transplant_output/")

### add sample size to the group. Label will be the variable where you divide the sample 
transplant_sample_timeline_final <- transplant_sample_timeline_final %>% dplyr::left_join(transplant_sample_timeline_final %>% dplyr::group_by(ACR_2more_ever) %>% dplyr::summarise(N=n_distinct(SubjID)))%>%
 dplyr::mutate(Label=paste0(ACR_2more_ever,' (Sample size = ',N,')'))

#consolidate ACR grading to 0-1 and 2-3
transplant_sample_timeline_final<-transplant_sample_timeline_final %>% 
      mutate(acute_cellular_rejection_consolidate=case_when((acute_cellular_rejection=="0" |acute_cellular_rejection=="1")~"0",
                                                            (acute_cellular_rejection=="2" | acute_cellular_rejection=="3"| acute_cellular_rejection=="4")~"2", T~"none"))

###no legend- died before 6 months
transplant_sample_timeline_final_6mo<-transplant_sample_timeline_final %>% filter((death_day>=180)|is.na(death_day))

transplant_sample_selection <- transplant_sample_timeline_final ### can change to transplant_sample_timeline_final_6mo if does not want to include pt who was dead within 6 months
transplant_sample_selection <- transplant_sample_selection %>% dplyr::mutate(month4= case_when((obs_day<120) ~ 1, TRUE~0))
transplant_sample_selection <- transplant_sample_selection %>% dplyr::left_join(transplant_sample_selection %>% 
                                                                                  dplyr::group_by(SubjID) %>% 
                                                                                  dplyr::summarise(month4_tot=sum(month4)))


####finding out which observation is between 14 and 120 days and had a biopsy
transplant_sample_selection2 <- transplant_sample_selection %>%dplyr::mutate(biopsy_1month= case_when(((obs_day>=14 & obs_day<120) &  biopsy ==1 )~ 1, TRUE~0))
####temp is the number of days the particular observation (has biopsy) is away from 1 month 
transplant_sample_selection2 <- transplant_sample_selection2[transplant_sample_selection2$biopsy_1month == 1, ] ###keeping only observation which fit the criteria: between 14 and 120 days and with biopsy
transplant_sample_selection2$temp <- abs(transplant_sample_selection2$obs_day-30)
####temp2 is the smallest of temp within each subject, this means the observation where temp = temp2 would be the observation with biopsy and closest to the 1 month mark
transplant_sample_selection2<-transplant_sample_selection2 %>% dplyr::left_join(transplant_sample_selection2 %>% 
                                                                                  dplyr::group_by(SubjID) %>% dplyr::summarise(temp2=min(temp, na.rm = TRUE)))
###transplant_sample_firstbiopsy contains the observation which is marked as the 1st biopsy (around 1 month) which pt received 
transplant_sample_selection2 <- transplant_sample_selection2[transplant_sample_selection2$temp2 == transplant_sample_selection2$temp, ] 
transplant_sample_firstbiopsy <- transplant_sample_selection2 %>%dplyr::select (c("SubjID","acute_cellular_rejection"))
transplant_sample_firstbiopsy$firstbiopsy <- 1
transplant_sample_firstbiopsy <- transplant_sample_firstbiopsy %>% dplyr::mutate(firstbiopsy_ACR2= case_when((acute_cellular_rejection=="2" |
                                                                                                         acute_cellular_rejection=="3" |
                                                                                                         acute_cellular_rejection=="4" )~ 1, TRUE~0))
transplant_sample_firstbiopsy <- transplant_sample_firstbiopsy %>% dplyr::rename(firstbiopsyACR=acute_cellular_rejection) 

####finding out which observation is within first 30 days that did not have biopsy (this should be the baseline bronch posttransplant)
transplant_sample_selection2 <- transplant_sample_selection %>% dplyr::mutate(first_bronch_nonbiopsy= case_when(((obs_day<30) &  biopsy ==0 )~ 1, TRUE~0))
transplant_sample_selection2 <- transplant_sample_selection2[transplant_sample_selection2$first_bronch_nonbiopsy == 1, ] ###keeping only observation which fit the criteria: between 14 and 120 days and with biopsy
transplant_sample_selection2$temp <- transplant_sample_selection2$obs_day
####temp2 is the smallest of temp within each subject, this means the observation where temp = temp2 would be the earliest bronch
transplant_sample_selection2<-transplant_sample_selection2 %>% dplyr::left_join(transplant_sample_selection2 %>% 
                                                                                  dplyr::group_by(SubjID) %>% dplyr::summarise(temp2=min(temp, na.rm = TRUE)))
###transplant_sample_firstbronch contains the observation which is marked as the 1st bronch (within first month) which pt received 
transplant_sample_selection2 <- transplant_sample_selection2[transplant_sample_selection2$temp2 == transplant_sample_selection2$temp, ] 
transplant_sample_firstbronch <- transplant_sample_selection2 %>%dplyr::select (c("SubjID"))
transplant_sample_firstbronch$firstbronch <-1

#####finding out which observation is the second biopsy
transplant_sample_selection2 <- transplant_sample_selection %>% dplyr::left_join(transplant_sample_selection %>% 
                                                                                   dplyr::group_by(SubjID, biopsy) %>%
                                                                                   dplyr::mutate(biopsy_num= row_number(obs_day))) 
#####keeping only observation for biopsy and is the second one
transplant_sample_selection2 <- transplant_sample_selection2[(transplant_sample_selection2$biopsy == 1 & 
                                                              transplant_sample_selection2$biopsy_num == 2), ] 

transplant_sample_selection2 <- transplant_sample_selection2 %>%dplyr::mutate(secondbronchACR2=case_when((acute_cellular_rejection=="2" |  
                                                                                                   acute_cellular_rejection=="3" | 
                                                                                                   acute_cellular_rejection=="4") ~ "Second Bronch ACR2+", 
                                                                                                    TRUE ~ "Second Bronch no ACR"))
transplant_sample_selection2$secondbronchACR2<-as.factor(transplant_sample_selection2$secondbronchACR2)
transplant_sample_secondbiopsy <- transplant_sample_selection2 %>%dplyr::select (c("SubjID", "secondbronchACR2"))

#####keeping only observation for biopsy and is after first bronch
transplant_sample_selection2 <- transplant_sample_selection %>% dplyr::left_join(transplant_sample_selection %>% 
                                                                                   dplyr::group_by(SubjID, biopsy) %>%
                                                                                   dplyr::mutate(biopsy_num= row_number(obs_day))) 
transplant_sample_selection2 <- transplant_sample_selection2[(transplant_sample_selection2$obs_day<405 & #only care about bronch within the 405 days
                                                                transplant_sample_selection2$biopsy == 1 & 
                                                                transplant_sample_selection2$biopsy_num >= 2), ] #keeping only biopsy observation after the first
transplant_sample_selection2 <- transplant_sample_selection2 %>%dplyr::mutate(bronchACR2=case_when((acute_cellular_rejection=="2" |  
                                                                                                  acute_cellular_rejection=="3" | 
                                                                                                  acute_cellular_rejection=="4") ~ 1, 
                                                                                                  TRUE ~ 0))
transplant_sample_selection2 <- transplant_sample_selection2 %>% dplyr::left_join(transplant_sample_selection2 %>% 
                                                                                    dplyr::group_by(SubjID) %>%
                                                                                    dplyr::summarise(ACR2afterfirst= max(bronchACR2))) 
transplant_sample_selection2 <- transplant_sample_selection2 %>%dplyr::select (c("SubjID", "ACR2afterfirst"))
transplant_sample_afterfirstbiopsy <- unique(transplant_sample_selection2)

######merging in the variables back to the transplant_sample_selection dataframe which will be used to make the graph
transplant_sample_selection3<- merge(transplant_sample_selection,transplant_sample_firstbiopsy, by=(c("SubjID")), all.x=T)
transplant_sample_selection3<- merge(transplant_sample_selection3,transplant_sample_firstbronch, by=(c("SubjID")), all.x=T)
transplant_sample_selection3<- merge(transplant_sample_selection3,transplant_sample_secondbiopsy, by=(c("SubjID")), all.x=T)
transplant_sample_selection3<- merge(transplant_sample_selection3,transplant_sample_afterfirstbiopsy, by=(c("SubjID")), all.x=T)

transplant_sample_selection_final <- transplant_sample_selection3 %>%dplyr::mutate(sample_type= case_when((firstbronch==1 &  firstbiopsy_ACR2==0 & ACR2afterfirst==1)~ "ACR- 1st, ACR+ after 1st",
                                                                                               (firstbronch==1 &  firstbiopsy_ACR2==0 & ACR2afterfirst==0)~ "always ACR- 1st", 
                                                                                               (firstbronch==1 &  firstbiopsy_ACR2==1 )~ "ACR+ 1st",
                                                                                               TRUE~"other"))

#merging in PGD samples
transplant_BL_meta_final<-merge(transplant_BL_meta_final,transplant_PGD_sample, by="SubjID", all=T) # adding in the variable PGD_sample which determines if pt should be placed in the PGD study

#labeling variables
label(transplant_BL_meta_final$gender) <- "Sex"
label(transplant_BL_meta_final$bmi) <- "BMI"
          units(transplant_BL_meta_final$bmi) <- "kg/m2"
label(transplant_BL_meta_final$age) <- "Age"
          units(transplant_BL_meta_final$age) <- "years"
label(transplant_BL_meta_final$race) <- "Race"
label(transplant_BL_meta_final$ethnicity) <- "Ethnicity"
label(transplant_BL_meta_final$diagnosis) <- "Pre-Transplant Diagnsosis Group"
label(transplant_BL_meta_final$LAS) <- "Lung Allocation Score"
label(transplant_BL_meta_final$reflux) <- "Esophageal Reflux Diagnosed Pre-Transplant"
label(transplant_BL_meta_final$preT_aspiration) <- "Aspiration Diagnosed Pre-Transplant"
label(transplant_BL_meta_final$preT_CMV_antibody) <- "Recipient CMV Antibody Positive"
label(transplant_BL_meta_final$preT_30d_antimicro) <- "Antimicrobial use 30 days Prior to Transplant"
label(transplant_BL_meta_final$preT_30d_immuno) <- "Immunosuppression use 30 days prior to Transplant"
label(transplant_BL_meta_final$preT_ecmo) <- "ECMO pre-transplant"
label(transplant_BL_meta_final$preT_6minwalk) <- "6 minute walk distance (meters)"
label(transplant_BL_meta_final$preT_FVC) <- "FVC(%)"
label(transplant_BL_meta_final$preT_FEV) <- "FEV1(%)"
label(transplant_BL_meta_final$preT_FEV_FVC) <- "FEV1/FVC"
label(transplant_BL_meta_final$preT_DLCO) <- "DLCO(%)"
label(transplant_BL_meta_final$preT_hgb) <- "Serum Hemoglobin(g/dL)"
label(transplant_BL_meta_final$preT_bicarb) <- "Serum Bicarbonate(mEq/L)"
label(transplant_BL_meta_final$preT_mPAP) <- "mPAP(mmHg)"
label(transplant_BL_meta_final$preT_PCWP) <- "PCWP(mmHg)"
label(transplant_BL_meta_final$preT_PVR) <- "PVR (Woods units)"
label(transplant_BL_meta_final$donor_smoker20yr) <- "Donor > 20 pack year smoking history"
label(transplant_BL_meta_final$donor_CMV_antibody) <- "Donor CMV Antibody Positive"
label(transplant_BL_meta_final$donor_pos_cx) <- "Positive Donor Culture"
label(transplant_BL_meta_final$heart_lung_txp) <- "Heart/Lung Transplant"
label(transplant_BL_meta_final$txp_type) <- "Transplant Type"
label(transplant_BL_meta_final$ecmo_txp) <- "ECMO/Cardiopulmonary Bypass During Transplant"
label(transplant_BL_meta_final$ischemic_time) <- "Ischemic time (minutes)"
label(transplant_BL_meta_final$surgery_crystalloid) <- "Crystalloid volume (mL)"
label(transplant_BL_meta_final$surgery_pRBC) <- "pRBC volume (mL)"
label(transplant_BL_meta_final$surgery_periop_abx) <- "Perioperative Antimicrobitals"
label(transplant_BL_meta_final$induction) <- "Induction Therapy Choice"
label(transplant_BL_meta_final$periop_immunosup) <- "Perioperative Immunosuppression"
label(transplant_BL_meta_final$post_ECMO) <- "ECMO post-transplant"
label(transplant_BL_meta_final$post_ext_time) <- "Time to extubation (hours)"
label(transplant_BL_meta_final$post_ICU) <- "Length ICU stay (days)"
label(transplant_BL_meta_final$post_LOS) <- "Length of stay in hospital (days)"
label(transplant_BL_meta_final$pgd24) <- "PGD at 24 hours post-transplant"
transplant_BL_meta_final$pgd24 <- factor(transplant_BL_meta_final$pgd24, 
                                         levels=c(0,1,2,3),
                                         labels=c("PGD 0 at 24 hrs",
                                                  "PGD 1 at 24 hrs", 
                                                  "PGD 2 at 24 hrs",
                                                  "PGD 3 at 24 hrs"))
label(transplant_BL_meta_final$pgd24) <- "PGD at 24 hours post-transplant"

transplant_BL_meta_final$pgd48 <- factor(transplant_BL_meta_final$pgd48, 
                                    levels=c(0,1,2,3),
                                    labels=c("PGD 0 at 48 hrs",
                                             "PGD 1 at 48 hrs", 
                                             "PGD 2 at 48 hrs",
                                             "PGD 3 at 48 hrs"))
label(transplant_BL_meta_final$pgd48) <- "PGD at 48 hours post-transplant"
transplant_BL_meta_final$pgd72 <- factor(transplant_BL_meta_final$pgd72, 
                                         levels=c(0,1,2,3),
                                         labels=c("PGD 0 at 72 hrs", 
                                                  "PGD 1 at 72 hrs", 
                                                  "PGD 2 at 72 hrs",
                                                  "PGD 3 at 72 hrs"))
label(transplant_BL_meta_final$pgd72) <- "PGD at 72 hours post-transplant"
label(transplant_BL_meta_final$pgd72_0_12_3) <- "PGD at 72 hours post-transplant, group 1 and 2"
label(transplant_BL_meta_final$fio2_24) <- "FiO2 at 24 hours post-transplant"
label(transplant_BL_meta_final$fio2_48) <- "FiO2 at 48 hours post-transplant"
label(transplant_BL_meta_final$fio2_72) <- "FiO2 at 72 hours post-transplant"
label(transplant_BL_meta_final$fio2_72_level) <- "FiO at 72 hours post-transplant"
transplant_BL_meta_final$fio2_72_level <- factor(transplant_BL_meta_final$fio2_72_level, 
                                         levels=c(0,1),
                                         labels=c("FiO2 below or equal 30 at 72 hrs", 
                                                  "FiO2 above 30 at 72 hrs"))

label(transplant_BL_meta_final$pgd_any) <- "any PGD post-transplant"
label(transplant_BL_meta_final$pgd_more2_A48hr) <- "PGD grade 2 or more after 48 hours"
label(transplant_BL_meta_final$pgd_3_A48hr) <- "any PGD grade 3 after 48 hours"
label(transplant_BL_meta_final$pgd_more2_A72hr) <- "PGD grade 2 or more after 72 hours"
label(transplant_BL_meta_final$pgd_3_A72hr) <- "PGD grade 3 after 72 hours"
label(transplant_BL_meta_final$pgd_12_A72hr) <- "PGD grade 1 or 2 after 72 hours"
transplant_BL_meta_final$pgd_highest_48_72 <- factor(transplant_BL_meta_final$pgd_highest_48_72, 
                                         levels=c(0,1,2),
                                         labels=c("PGD-0", 
                                                  "PGD-1or2", 
                                                  "PGD-3"))
label(transplant_BL_meta_final$pgd_highest_48_72) <- "Highest PGD grade"
transplant_BL_meta_final$pgd_highest_48_72_v2 <- factor(transplant_BL_meta_final$pgd_highest_48_72_v2, 
                                               levels=c(0,1,2,3),
                                               labels=c("PGD-0", 
                                                        "PGD-1", 
                                                        "PGD-2", 
                                                        "PGD-3"))
label(transplant_BL_meta_final$pgd_highest_48_72_v2) <- "Highest PGD grade V2"

transplant_BL_meta_final$pgd_highest_48_72_v3 <- factor(transplant_BL_meta_final$pgd_highest_48_72_v3, 
                                                        levels=c(0,1,2),
                                                        labels=c("PGD-0or1", 
                                                                 "PGD-2", 
                                                                 "PGD-3"))
label(transplant_BL_meta_final$pgd_highest_48_72_v3) <- "Highest PGD grade V3"

transplant_BL_meta_final$pgd_3_48_72 <- factor(transplant_BL_meta_final$pgd_3_48_72, 
                                                  levels=c("No","Yes"),
                                                  labels=c("PGD <=2 on 48 or 72 hr", 
                                                           "PGD 3 on 48 or 72 hr"))
label(transplant_BL_meta_final$pgd_3_48_72) <- "PGD 3 on 48 or 72 hr"

transplant_BL_meta_final$pgd_any <- factor(transplant_BL_meta_final$pgd_any, 
                                                          levels=c("Yes","No"),
                                                          labels=c("Had PGD", 
                                                                   "Never had PGD"))
label(transplant_BL_meta_final$pgd_any) <- "Has PGD at any point"
transplant_BL_meta_final$moderatepgd <- factor(transplant_BL_meta_final$moderatepgd, 
                                               levels=c("Yes","No"),
                                               labels=c("at least moderate PGD",
                                                        "only mild PGD"))
label(transplant_BL_meta_final$moderatepgd) <- "At least moderate PGD at any point"

transplant_BL_meta_final$nopgd_72 <- factor(transplant_BL_meta_final$nopgd_72, 
                                               levels=c("Yes","No"),
                                               labels=c("No PGD at 72 hr", 
                                                        "PGD at 72 hr"))
label(transplant_BL_meta_final$nopgd_72) <- "Any PGD at 72 hours post transplant"

transplant_BL_meta_final$HLA <- factor(transplant_BL_meta_final$HLA, 
                                                  levels=c("No","Yes"),
                                                  labels=c("no HLA", 
                                                           "HLA"))
label(transplant_BL_meta_final$HLA) <-"any HLA"
transplant_BL_meta_final$HLA_C1 <- factor(transplant_BL_meta_final$HLA_C1, 
                                       levels=c("No","Yes"),
                                       labels=c("no HLA Class 1",
                                                "HLA Class 1"))
label(transplant_BL_meta_final$HLA_C1) <-"HLA Class 1"
transplant_BL_meta_final$HLA_C2 <- factor(transplant_BL_meta_final$HLA_C2, 
                                       levels=c("No","Yes"),
                                       labels=c("no HLA Class 2", 
                                                "HLA Class 2"))
label(transplant_BL_meta_final$HLA_C2) <-"HLA Class 2"

transplant_BL_meta_final$any_ECMO <- factor(transplant_BL_meta_final$any_ECMO, 
                                          levels=c("No","Yes"),
                                          labels=c("never had ECMO", 
                                                   "had ECMO"))
label(transplant_BL_meta_final$any_ECMO) <-"Any ECMO"

############keeping only subjects that will ultimately go into the analysis
transplant_BL_meta_final_org<-transplant_BL_meta_final ###keeping the original so the ultimate mapping file still has all the samples
transplant_BL_meta_final_PGDsample<- transplant_BL_meta_final[transplant_BL_meta_final$SubjID %in% transplant_final_PGD_samplelist,]

setwd("C:/Users/kendr/OneDrive/Desktop/research_transplant/transplant_output/PGD_GENUS/")
#########Table 1##########
section_separator("Table 1 by highest PGD_v3 
                  \n pmax(transplant_baseline_meta2$pgd_48h,transplant_baseline_meta2$pgd_72h)" )
overall_table1_pgd_highest_48_72_v3<-table1(~ gender + age + bmi + race + ethnicity +
                                              diagnosis  + HLA + HLA_C1 + HLA_C2 + LAS  + reflux + preT_aspiration + preT_CMV_antibody + preT_30d_antimicro + preT_30d_immuno + preT_ecmo +
                                              preT_6minwalk + preT_FVC  + preT_FEV + preT_FEV_FVC + preT_DLCO + 
                                              preT_hgb + preT_bicarb + preT_mPAP + preT_PCWP + preT_PVR +
                                              donor_smoker20yr + donor_CMV_antibody  + donor_pos_cx +
                                              heart_lung_txp + txp_type  + ecmo_txp + ischemic_time + surgery_crystalloid + 
                                              surgery_pRBC + surgery_periop_abx + induction + periop_immunosup +
                                              post_ECMO + post_ext_time  + post_ICU + post_LOS + any_ECMO  + 
                                              fio2_24 + fio2_48 + fio2_72 + fio2_72_level | pgd_highest_48_72_v3 ,
                                            data=transplant_BL_meta_final_PGDsample%>%filter(!is.na(pgd_highest_48_72_v3)), overall= "Total", render=render.NEW, caption="Table 1 by highest PGD_v3", extra.col=list(`P-value`=pvalue_posthoc))
output_table1("overall_table1_pgd_highest_48_72_v3")
table1_posthoc("overall_table1_pgd_highest_48_72_v3")

###load raw qza and meta file
transplant_mapping_1$Date.collection<-mdy(transplant_mapping_1$Date.collection)
transplant_mapping_final <-merge(transplant_mapping_1,transplant_mapping_2, by=c("SubjID", "Date.collection"), all.x=T)
#removing empty columns
empty_columns <- colSums(is.na(transplant_mapping_final) | transplant_mapping_final == "") == nrow(transplant_mapping_final)
empty_columns[is.na(empty_columns)] <- FALSE
transplant_mapping_final<- transplant_mapping_final[, !empty_columns]

transplant_meta_final<- merge(transplant_mapping_final,transplant_BL_meta_final_org,by=c("SubjID"), all=T) ###adding the variable of interested which was generated back to the original metadata
###making sure no symbols go into the metadata file that get merge in phyloseq-> phyloseq doesnt like weird symbol 
transplant_meta_final <- lapply(transplant_meta_final, function(x) as.character(gsub(" ", "_", x))) 
transplant_meta_final <- lapply(transplant_meta_final, function(x) as.character(gsub("\\(", "", x)))
transplant_meta_final <- lapply(transplant_meta_final, function(x) as.character(gsub("\\)", "", x)))

###generating a new sample type variables. the Description variable in the mapping file is incorrectly coded
transplant_meta_final$sample_type[grepl("BAL",transplant_meta_final$SampleID)]<-"Lower"
transplant_meta_final$sample_type[grepl("BALF",transplant_meta_final$SampleID)]<-"Lower"
transplant_meta_final$sample_type[grepl("Control",transplant_meta_final$SampleID)]<-"Lower"
transplant_meta_final$sample_type[grepl("Bronch",transplant_meta_final$SampleID)]<-"BKG"
transplant_meta_final$sample_type[grepl("Sup",transplant_meta_final$SampleID)]<-"Upper"
transplant_meta_final$sample_type[grepl("DFW",transplant_meta_final$SampleID)]<-"DFW"
transplant_meta_final$sample_type[grepl("Blank",transplant_meta_final$SampleID)]<-"Blank"
transplant_meta_final$sample_type[grepl("MOC",transplant_meta_final$SampleID)]<-"MOC"

####generating variables that Nat needs for QIIME
transplant_meta_final <- as.data.frame(transplant_meta_final)

first_bronch_date$Date.collection<-mdy(first_bronch_date$Date.collection)
transplant_meta_final <- merge(first_bronch_date, transplant_meta_final, by=c("SubjID", "Date.collection"), all.y=T)
transplant_meta_final$first_bronch_date[is.na(transplant_meta_final$first_bronch_date)] <-0
###generating a variable that is time from transplant
temp_data<- transplant_meta_timeline_var%>%dplyr::select(c("SubjID","transplant_date"))
temp_data<-temp_data[!is.na(temp_data$transplant_date),]
temp_data<-unique(temp_data)

transplant_meta_final <- merge(temp_data, transplant_meta_final, by=c("SubjID"), all.y=T)
transplant_meta_final$time_from_transplant<-transplant_meta_final$Date.collection-transplant_meta_final$transplant_date
transplant_meta_final <- transplant_meta_final %>%dplyr::mutate(bronch_within3days=case_when(time_from_transplant<=3~1, TRUE~0))
transplant_meta_final <- transplant_meta_final %>%dplyr::mutate(bronch_within1week=case_when(time_from_transplant<=7~1, TRUE~0))
transplant_meta_final <- transplant_meta_final %>%dplyr::mutate(bronch_within2week=case_when(time_from_transplant<=14~1, TRUE~0))
transplant_meta_final <- transplant_meta_final %>%dplyr::mutate(bronch_within1month=case_when(time_from_transplant<=30~1, TRUE~0))
transplant_meta_final <- transplant_meta_final %>%dplyr::mutate(bronch_within1year=case_when(time_from_transplant<=365~1, TRUE~0))
transplant_meta_final <- transplant_meta_final %>%dplyr::mutate(bronch_within13mon=case_when(time_from_transplant<=403~1, TRUE~0))

###fixing labeling of the sampleID. some samples were label incorrectly on red cap. Donor and recipient lungs labeling were swapped
###unable to change the SampleID at this step because it would only change the meta file. would need to change other qza files to match the change
#SampleID_temp would only be used for making the native_lung variable
transplant_meta_final$SampleID_temp<-transplant_meta_final$SampleID 
transplant_meta_final$SampleID_temp[transplant_meta_final$SampleID_temp=="TX.0022.BAL.RML.RL.4"] <- "TX.0022.BAL.RML.DL.4" #shouldnt be RL. mistake on redcap entry
transplant_meta_final$SampleID_temp[transplant_meta_final$SampleID_temp=="UNIV.0258.BAL.RML.RL.4"] <- "UNIV.0258.BAL.RML.DL.4" #shouldnt be RL. mistake on redcap entry
transplant_meta_final$SampleID_temp[transplant_meta_final$SampleID_temp=="UNIV.0258.BAL.RML.RL.3"] <- "UNIV.0258.BAL.RML.DL.3" #shouldnt be RL. mistake on redcap entry

###identify samples to be removed
transplant_meta_final$native_lung <- grepl(".RL.",transplant_meta_final$SampleID_temp, fixed = TRUE) #native_lung is true if the sample is obtained from native lung

#there are subject with bilateral lung transplant but have two samples obtained (some time even in the same location)- will have an index so we can limit the analysis to only 1 bronch
transplant_meta_final <- transplant_meta_final %>% dplyr::left_join(transplant_meta_final %>% dplyr::group_by(SubjID,sample_type,Date.collection, native_lung) %>% dplyr::mutate(bronch_index = row_number())) 

####Cell count 6/26/23
transplant_cell_count<-read_excel("C:/Users/kendr/OneDrive/Desktop/research_transplant/transplant_raw/PGD_Map_Cell_Count_Cluster.xlsx") ##reading raw red cap data

beta_cluster_input_cell_count<-transplant_cell_count %>% select(c("Sample", "Cell_Count_Cluster"))
beta_cluster_input_cell_count<-data.frame(beta_cluster_input_cell_count)
row.names(beta_cluster_input_cell_count)<-beta_cluster_input_cell_count$Sample
beta_cluster_input_cell_count<- beta_cluster_input_cell_count %>% dplyr::rename(SampleID=Sample)
beta_cluster_input_cell_count_output<-beta_cluster_input_cell_count
beta_cluster_input_cell_count<-beta_cluster_input_cell_count %>% mutate(Cluster_num=case_when(Cell_Count_Cluster=="Cluster_1_Macrophages"~"1", Cell_Count_Cluster=="Cluster_2_Neutrophils"~"2", T~NA))
beta_cluster_input_cell_count<-beta_cluster_input_cell_count %>% select(-c("Cell_Count_Cluster"))

transplant_cell_count<- transplant_cell_count %>% select(c("ntm_id_01", "Cell_Count_Cluster"))
transplant_cell_count$ntm_id_01<-toupper(gsub("_TX", "", transplant_cell_count$ntm_id_01))

#adding the cell count cluster information into meta file 
transplant_meta_final<-merge(transplant_meta_final, transplant_cell_count, by.x="SubjID", by.y="ntm_id_01", all.x=T)


####Cytokine Cluster- 8/7/23
Cytokine_cluster<-read_excel("C:/Users/kendr/OneDrive/Desktop/research_transplant/transplant_raw/Cytokine_Cluster.xlsx")

#adding the Cytokine cluster information into meta file 
transplant_meta_final<-merge(transplant_meta_final, Cytokine_cluster, by.x="SubjID", by.y="ID", all.x=T)

###PGD analysis group PGD severity
transplant_PGDsample_plot<-subset(transplant_BL_meta_final_PGDsample, select=c("SubjID", "pgd24", "pgd48", "pgd72"))
transplant_PGDsample_plot$pgd24<-as.numeric(transplant_PGDsample_plot$pgd24)
transplant_PGDsample_plot$pgd48<-as.numeric(transplant_PGDsample_plot$pgd48)
transplant_PGDsample_plot$pgd72<-as.numeric(transplant_PGDsample_plot$pgd72)
transplant_PGDsample_plot2<-transplant_PGDsample_plot
transplant_PGDsample_plot2$sum_pgd<-transplant_PGDsample_plot$pgd24+transplant_PGDsample_plot$pgd48+transplant_PGDsample_plot$pgd72
transplant_PGDsample_plot2$sum2_pgd<-transplant_PGDsample_plot$pgd48+transplant_PGDsample_plot$pgd72

transplant_PGDsample_plot2$rank<-frank(transplant_PGDsample_plot2,sum_pgd,sum2_pgd,pgd72,pgd48,pgd24)
transplant_PGDsample_plot2<-transplant_PGDsample_plot2[order(transplant_PGDsample_plot2$rank),]
rownames(transplant_PGDsample_plot2) <- 1:nrow(transplant_PGDsample_plot2)  
number_subject<-nrow(transplant_PGDsample_plot2)
transplant_PGDsample_plot2$index<-row.names(transplant_PGDsample_plot2)

transplant_PGDsample_plot2<-subset(transplant_PGDsample_plot2, select=c("SubjID","rank","index"))

transplant_PGDsample_plot_long<-melt(transplant_PGDsample_plot, id.vars=c("SubjID"))
transplant_PGDsample_plot_long$value <- factor(transplant_PGDsample_plot_long$value, 
                                                        levels=c(1,2,3,4),
                                                        labels=c("PGD 0", 
                                                                 "PGD 1", 
                                                                 "PGD 2", 
                                                                 "PGD 3"))
transplant_PGDsample_plot_long<-merge(transplant_PGDsample_plot_long,transplant_PGDsample_plot2, by="SubjID")

#split the graph into two because too many subjects
transplant_PGDsample_plot_long_a<-transplant_PGDsample_plot_long[transplant_PGDsample_plot_long$index<=number_subject/2,]
transplant_PGDsample_plot_long_b<-transplant_PGDsample_plot_long[transplant_PGDsample_plot_long$index>number_subject/2,]

plot1<-ggplot() +
  geom_point(data=transplant_PGDsample_plot_long_a %>% mutate(SubjID = fct_reorder(SubjID,rank)), 
             aes(x=SubjID, y=variable,color=value), shape=15, size=5)+ coord_flip()+
  scale_color_manual(values=c("PGD 0"="green","PGD 1"="yellow","PGD 2"="orange","PGD 3"="red"))+
  theme(axis.text.x =element_text(size=18),
  axis.text.y =element_text(size=14),
  axis.title.y = element_blank(),
  axis.title.x = element_blank())

plot2<-ggplot() +
  geom_point(data=transplant_PGDsample_plot_long_b %>% mutate(SubjID = fct_reorder(SubjID,rank)), 
             aes(x=SubjID, y=variable,color=value), shape=15, size=5)+ coord_flip()+
  scale_color_manual(values=c("PGD 0"="green","PGD 1"="yellow","PGD 2"="orange","PGD 3"="red"))+
  theme(axis.text.x =element_text(size=18),
        axis.text.y =element_text(size=14),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
jpeg(file="PGD_grading.jpeg", res = 300, width=250, height=350 , units='mm')
all_plot <- ggarrange(plot2, plot1, ncol=2, nrow=1, align="h",  common.legend = TRUE)
show(all_plot)
dev.off()

setwd("C:/Users/kendr/OneDrive/Desktop/research_transplant/transplant_raw/") #set the directory to the raw file directory 
transplant_meta_final <- transplant_meta_final %>% dplyr::select("SampleID.unique",everything())
write.table(transplant_meta_final, file = "msq.master.map.transplant_updated.txt", row.names = FALSE, sep ='\t', quote=F)

Transplant_PS<-qza_to_phyloseq(
  features="no-miss-table-dada2.transplant.qza",
  tree="rooted-tree_quality.transplant.qza",
  taxonomy="taxonomy.transplant.qza",
  metadata = "msq.master.map.transplant_updated.txt"
)
                  
                  N_Transplant_PS1<-nrow(sample_data(Transplant_PS))#### SAMPLE SIZE CAPTURE 
                  N_upper_Transplant_PS1<-nrow(sample_data(Transplant_PS)[sample_data(Transplant_PS)$sample_type %in% c("Upper")])#### Upper SAMPLE SIZE CAPTURE 
                  N_lower_Transplant_PS1<-nrow(sample_data(Transplant_PS)[sample_data(Transplant_PS)$sample_type %in% c("Lower")])#### Lower SAMPLE SIZE CAPTURE
                  N_BKG_Transplant_PS1<-nrow(sample_data(Transplant_PS)[sample_data(Transplant_PS)$sample_type %in% c("BKG")])#### BKG SAMPLE SIZE CAPTURE 
                  N_control_Transplant_PS1<-nrow(sample_data(Transplant_PS)[sample_data(Transplant_PS)$sample_type %in% c("MOC", "DFW", "Blank")])#### Control SAMPLE SIZE CAPTURE 
                  
                  Taxa_Transplant_PS1<-nrow(tax_table(Transplant_PS))#### TAXA SIZE CAPTURE
                  Subject_Transplant_PS1<-length(unique(sample_data(Transplant_PS)$SubjID)[! unique(sample_data(Transplant_PS)$SubjID) %in% c("MOC", "DFW", "Blank")])#### Subject Sample SIZE CAPTURE

###fixing labeling of the sampleID. some samples were label incorrectly on red cap. Donor and recipient lungs labeling were swapped
sample_data(Transplant_PS)$SampleID[sample_data(Transplant_PS)$SampleID=="TX.0022.BAL.RML.RL.4"] <- "TX.0022.BAL.RML.DL.4" #shouldnt be RL. mistake on redcap entry
sample_data(Transplant_PS)$SampleID[sample_data(Transplant_PS)$SampleID=="UNIV.0258.BAL.RML.RL.4"] <- "UNIV.0258.BAL.RML.DL.4" #shouldnt be RL. mistake on redcap entry
sample_data(Transplant_PS)$SampleID[sample_data(Transplant_PS)$SampleID=="UNIV.0258.BAL.RML.RL.3"] <- "UNIV.0258.BAL.RML.DL.3" #shouldnt be RL. mistake on redcap entry
#update the sample_names in the phyloseq
sample_names(Transplant_PS)<-sample_data(Transplant_PS)$SampleID

###setting up ddPCR data
ddPCR_raw <- loadWorkbook("TX_ddPCR_results_2023.xlsx")
Lower_ddPCR <- read.xlsx(ddPCR_raw, sheet ="All BAL", skipEmptyRows = TRUE, colNames = TRUE)
      Lower_ddPCR <- Lower_ddPCR %>% dplyr::select(c("Sample.ID", "Copies.per.ul.sample"))
      Lower_ddPCR <- Lower_ddPCR %>% dplyr::rename(SampleID=Sample.ID, ddPCR_count=Copies.per.ul.sample)
      Lower_ddPCR<-Lower_ddPCR[which(!is.na(Lower_ddPCR$SampleID)),] #remove rows with empty Sample.ID
      Lower_ddPCR$sample_type="Lower"
Upper_ddPCR <- read.xlsx(ddPCR_raw, sheet ="All supraglottic", skipEmptyRows = TRUE, colNames = TRUE)
      Upper_ddPCR <- Upper_ddPCR %>% dplyr::select(c("Sample.ID", "Copies.per.ul.sample"))
      Upper_ddPCR <- Upper_ddPCR %>% dplyr::rename(SampleID=Sample.ID, ddPCR_count=Copies.per.ul.sample)
      Upper_ddPCR<-Upper_ddPCR[which(!is.na(Upper_ddPCR$SampleID)),] #remove rows with empty Sample.ID
      Upper_ddPCR$sample_type="Upper"
BKG_ddPCR <- read.xlsx(ddPCR_raw, sheet ="All BKG", skipEmptyRows = TRUE, colNames = TRUE)
      BKG_ddPCR <- BKG_ddPCR %>% dplyr::select(c("Sample.ID", "Copies.per.ul.sample"))
      BKG_ddPCR <- BKG_ddPCR %>% dplyr::rename(SampleID=Sample.ID, ddPCR_count=Copies.per.ul.sample )
      BKG_ddPCR<-BKG_ddPCR[which(!is.na(BKG_ddPCR$SampleID)),] #remove rows with empty Sample.ID
      BKG_ddPCR$sample_type="BKG"
Total_ddPCR<-rbind(Lower_ddPCR,Upper_ddPCR,BKG_ddPCR)
Total_ddPCR$SampleID<-gsub(" ","",Total_ddPCR$SampleID)
Total_ddPCR$SampleID[Total_ddPCR$SampleID=="TX.0022.BAL.RML.RL.4"] <- "TX.0022.BAL.RML.DL.4" #shouldnt be RL. mistake on redcap entry
Total_ddPCR$SampleID[Total_ddPCR$SampleID=="UNIV.0258.BAL.RML.RL.4"] <- "UNIV.0258.BAL.RML.DL.4" #shouldnt be RL. mistake on redcap entry
Total_ddPCR$SampleID[Total_ddPCR$SampleID=="UNIV.0258.BAL.RML.RL.3"] <- "UNIV.0258.BAL.RML.DL.3" #shouldnt be RL. mistake on redcap entry
#some of the SampleID in the ddPCR file has extra space in there. need to remove these spacing so they match with the phyloseq file
###setting up pepsin data
#pepsin data is different than the ddpcr that it is match to SubjID. it doesnt need to match to the Sample. 
pepsin_raw <- loadWorkbook("TX BALF Pepsin ELISA result 020923.xlsx")
Lower_pepsin <- read.xlsx(pepsin_raw, sheet ="Yonghua", skipEmptyRows = TRUE, colNames = TRUE)
Lower_pepsin <- Lower_pepsin %>% dplyr::select(c("Study_Linked_Subject.ID", "Pmol/min/ml"))
Lower_pepsin <- Lower_pepsin %>% dplyr::rename(SubjID=Study_Linked_Subject.ID, pepsin="Pmol/min/ml" )

# Remove taxa with 0 abundance
Transplant_PS = subset_taxa(Transplant_PS, rowSums(otu_table(Transplant_PS)) != 0)
                  N_Transplant_PS2<-nrow(sample_data(Transplant_PS))#### SAMPLE SIZE CAPTURE 
                  N_upper_Transplant_PS2<-nrow(sample_data(Transplant_PS)[sample_data(Transplant_PS)$sample_type %in% c("Upper")])#### Upper SAMPLE SIZE CAPTURE 
                  N_lower_Transplant_PS2<-nrow(sample_data(Transplant_PS)[sample_data(Transplant_PS)$sample_type %in% c("Lower")])#### Lower SAMPLE SIZE CAPTURE 
                  N_BKG_Transplant_PS2<-nrow(sample_data(Transplant_PS)[sample_data(Transplant_PS)$sample_type %in% c("BKG")])#### BKG SAMPLE SIZE CAPTURE 
                  N_control_Transplant_PS2<-nrow(sample_data(Transplant_PS)[sample_data(Transplant_PS)$sample_type %in% c("MOC", "DFW", "Blank")])#### Control SAMPLE SIZE CAPTURE 
                  
                  Taxa_Transplant_PS2<-nrow(tax_table(Transplant_PS))#### TAXA SIZE CAPTURE 
                  Subject_Transplant_PS2<-length(unique(sample_data(Transplant_PS)$SubjID)[! unique(sample_data(Transplant_PS)$SubjID) %in% c("MOC", "DFW", "Blank")])#### Subject Sample SIZE CAPTURE
                  
#remove samples that had less than 1000 reads (total count)
Transplant_PS = prune_samples(sample_sums(Transplant_PS) > 1000, Transplant_PS)
                  N_Transplant_PS3<-nrow(sample_data(Transplant_PS))#### SAMPLE SIZE CAPTURE 
                  N_upper_Transplant_PS3<-nrow(sample_data(Transplant_PS)[sample_data(Transplant_PS)$sample_type %in% c("Upper")])#### Upper SAMPLE SIZE CAPTURE 
                  N_lower_Transplant_PS3<-nrow(sample_data(Transplant_PS)[sample_data(Transplant_PS)$sample_type %in% c("Lower")])#### Lower SAMPLE SIZE CAPTURE 
                  N_BKG_Transplant_PS3<-nrow(sample_data(Transplant_PS)[sample_data(Transplant_PS)$sample_type %in% c("BKG")])#### BKG SAMPLE SIZE CAPTURE 
                  N_control_Transplant_PS3<-nrow(sample_data(Transplant_PS)[sample_data(Transplant_PS)$sample_type %in% c("MOC", "DFW", "Blank")])#### Control SAMPLE SIZE CAPTURE 
                  
                  Taxa_Transplant_PS3<-nrow(tax_table(Transplant_PS))#### TAXA SIZE CAPTURE
                  Subject_Transplant_PS3<-length(unique(sample_data(Transplant_PS)$SubjID)[! unique(sample_data(Transplant_PS)$SubjID) %in% c("MOC", "DFW", "Blank")])#### Subject Sample SIZE CAPTURE
                  
normalizeSample <- function(x){x/sum(x)}
Transplant_PS_relab = transformSampleCounts(Transplant_PS,normalizeSample) ##relative abundance

#exclude MOC, blank and neg
Transplant_PS_all = subset_samples(Transplant_PS, sample_type  %in% c("Upper", "Lower", "BKG"))

setwd("C:/Users/kendr/OneDrive/Desktop/research_transplant/transplant_output/PGD_GENUS/")

read_count_KW(Transplant_PS, "sample_type", 
              c("BKG", "Lower", "Upper", "MOC", "DFW", "Blank"), c("darkgoldenrod1", "steelblue2", "purple2", "red", "black", "seagreen4"), 
              xlabel_size=12, ylabel_size=13, axis_title_size=15,
              p_value="no", output_name="read_count_all_nonegcontrol")
alpha_diversity_KW(Transplant_PS, "sample_type", 
                   c("BKG", "Lower", "Upper", "MOC", "DFW", "Blank"), c("darkgoldenrod1", "steelblue2", "purple2", "red", "black", "seagreen4"), 
                   xlabel_size=12, ylabel_size=13, axis_title_size=15,
                   p_value="no",output_name="alpha_diversity_all")
beta_diversity_KW(Transplant_PS_relab, "sample_type", 
                  c("BKG", "Lower", "Upper", "MOC", "DFW", "Blank"), c("darkgoldenrod1", "steelblue2", "purple2", "red", "black", "seagreen4"),
                  axis_title_size=18, label_size=7,
                  p_value="yes", output_name="beta_diversity_all", x_axis_flip = "yes", p_value_location = "BL")

setwd("C:/Users/kendr/OneDrive/Desktop/research_transplant/transplant_raw/") #set the directory to the raw file directory 

Transplant_PS<-qza_to_phyloseq(
features="no-miss-table-dada2.transplant.qza",
tree="rooted-tree_quality.transplant.qza",
taxonomy="taxonomy.transplant.qza",
metadata = "msq.master.map.transplant_updated.txt"
)
          N_Transplant_PS<-nrow(sample_data(Transplant_PS))#### SAMPLE SIZE CAPTURE 
          N_upper_Transplant_PS<-nrow(sample_data(Transplant_PS)[sample_data(Transplant_PS)$sample_type %in% c("Upper")])#### Upper SAMPLE SIZE CAPTURE 
          N_lower_Transplant_PS<-nrow(sample_data(Transplant_PS)[sample_data(Transplant_PS)$sample_type %in% c("Lower")])#### Lower SAMPLE SIZE CAPTURE 
          N_BKG_Transplant_PS<-nrow(sample_data(Transplant_PS)[sample_data(Transplant_PS)$sample_type %in% c("BKG")])#### BKG SAMPLE SIZE CAPTURE 
          N_control_Transplant_PS<-nrow(sample_data(Transplant_PS)[sample_data(Transplant_PS)$sample_type %in% c("MOC", "DFW", "Blank")])#### Control SAMPLE SIZE CAPTURE 
          
          Taxa_Transplant_PS<-nrow(tax_table(Transplant_PS))#### TAXA SIZE CAPTURE
          Subject_Transplant_PS<-length(unique(sample_data(Transplant_PS)$SubjID)[! unique(sample_data(Transplant_PS)$SubjID) %in% c("MOC", "DFW", "Blank")])#### Subject Sample SIZE CAPTURE

###renaming variable- pgd_highest_48_72_v3 per Nat request 
#this basically add several new variables to the phyloseq that can be used for later analysis 
#renaming PGD-0or1, PGD2, PGD3
Transplant_PS <- Transplant_PS %>% ps_mutate(pgd_highest_48_72_v3_rename=case_when(
  pgd_highest_48_72_v3=="PGD-0or1" ~ "No PGD", 
  pgd_highest_48_72_v3=="PGD-2" ~ "Moderate",
  pgd_highest_48_72_v3=="PGD-3" ~ "Severe"))
          
          
setwd("C:/Users/kendr/OneDrive/Desktop/research_transplant/transplant_output/PGD_GENUS/") #set the directory to the raw file directory 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~GENUS LEVEL~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#aggragate taxa rank to genus
Transplant_PS_GENUS <- tax_glom(Transplant_PS, taxrank="Genus", NArm=FALSE) #not dropping NA genus 
          N_Transplant_PS_GENUS1<-nrow(sample_data(Transplant_PS_GENUS))#### SAMPLE SIZE CAPTURE
          N_upper_Transplant_PS_GENUS1<-nrow(sample_data(Transplant_PS_GENUS)[sample_data(Transplant_PS_GENUS)$sample_type %in% c("Upper")])#### Upper SAMPLE SIZE CAPTURE 
          N_lower_Transplant_PS_GENUS1<-nrow(sample_data(Transplant_PS_GENUS)[sample_data(Transplant_PS_GENUS)$sample_type %in% c("Lower")])#### Lower SAMPLE SIZE CAPTURE 
          N_BKG_Transplant_PS_GENUS1<-nrow(sample_data(Transplant_PS_GENUS)[sample_data(Transplant_PS_GENUS)$sample_type %in% c("BKG")])#### BKG SAMPLE SIZE CAPTURE 
          N_control_Transplant_PS_GENUS1<-nrow(sample_data(Transplant_PS_GENUS)[sample_data(Transplant_PS_GENUS)$sample_type %in% c("MOC", "DFW", "Blank")])#### Control SAMPLE SIZE CAPTURE 
          
          
          Taxa_Transplant_PS_GENUS1<-nrow(tax_table(Transplant_PS_GENUS))#### TAXA SIZE CAPTURE
          Subject_Transplant_PS_GENUS1<-length(unique(sample_data(Transplant_PS_GENUS)$SubjID)[! unique(sample_data(Transplant_PS_GENUS)$SubjID) %in% c("MOC", "DFW", "Blank")])#### Subject Sample SIZE CAPTURE (excluding MOC Blank DFW)

###fixing labeling of the sampleID. some samples were label incorrectly on red cap. Donor and recipient lungs labeling were swapped
sample_data(Transplant_PS_GENUS)$SampleID[sample_data(Transplant_PS_GENUS)$SampleID=="TX.0022.BAL.RML.RL.4"] <- "TX.0022.BAL.RML.DL.4" #shouldnt be RL. mistake on redcap entry
sample_data(Transplant_PS_GENUS)$SampleID[sample_data(Transplant_PS_GENUS)$SampleID=="UNIV.0258.BAL.RML.RL.4"] <- "UNIV.0258.BAL.RML.DL.4" #shouldnt be RL. mistake on redcap entry
sample_data(Transplant_PS_GENUS)$SampleID[sample_data(Transplant_PS_GENUS)$SampleID=="UNIV.0258.BAL.RML.RL.3"] <- "UNIV.0258.BAL.RML.DL.3" #shouldnt be RL. mistake on redcap entry
#update the sample_names in the phyloseq
sample_names(Transplant_PS_GENUS)<-sample_data(Transplant_PS_GENUS)$SampleID

# Remove Genus with 0 abundance
Transplant_PS_GENUS = subset_taxa(Transplant_PS_GENUS, rowSums(otu_table(Transplant_PS_GENUS)) != 0)
          N_Transplant_PS_GENUS2<-nrow(sample_data(Transplant_PS_GENUS))#### SAMPLE SIZE CAPTURE
          N_upper_Transplant_PS_GENUS2<-nrow(sample_data(Transplant_PS_GENUS)[sample_data(Transplant_PS_GENUS)$sample_type %in% c("Upper")])#### Upper SAMPLE SIZE CAPTURE 
          N_lower_Transplant_PS_GENUS2<-nrow(sample_data(Transplant_PS_GENUS)[sample_data(Transplant_PS_GENUS)$sample_type %in% c("Lower")])#### Lower SAMPLE SIZE CAPTURE 
          N_BKG_Transplant_PS_GENUS2<-nrow(sample_data(Transplant_PS_GENUS)[sample_data(Transplant_PS_GENUS)$sample_type %in% c("BKG")])#### BKG SAMPLE SIZE CAPTURE 
          N_control_Transplant_PS_GENUS2<-nrow(sample_data(Transplant_PS_GENUS)[sample_data(Transplant_PS_GENUS)$sample_type %in% c("MOC", "DFW", "Blank")])#### Control SAMPLE SIZE CAPTURE 
          
          Taxa_Transplant_PS_GENUS2<-nrow(tax_table(Transplant_PS_GENUS))#### TAXA SIZE CAPTURE 
          Subject_Transplant_PS_GENUS2<-length(unique(sample_data(Transplant_PS_GENUS)$SubjID)[! unique(sample_data(Transplant_PS_GENUS)$SubjID) %in% c("MOC", "DFW", "Blank")])#### Subject Sample SIZE CAPTURE (excluding MOC Blank DFW)

#remove samples that had less than 1000 reads (total count)
Transplant_PS_GENUS = prune_samples(sample_sums(Transplant_PS_GENUS) > 1000, Transplant_PS_GENUS)
          N_Transplant_PS_GENUS3<-nrow(sample_data(Transplant_PS_GENUS))#### SAMPLE SIZE CAPTURE 
          N_upper_Transplant_PS_GENUS3<-nrow(sample_data(Transplant_PS_GENUS)[sample_data(Transplant_PS_GENUS)$sample_type %in% c("Upper")])#### Upper SAMPLE SIZE CAPTURE 
          N_lower_Transplant_PS_GENUS3<-nrow(sample_data(Transplant_PS_GENUS)[sample_data(Transplant_PS_GENUS)$sample_type %in% c("Lower")])#### Lower SAMPLE SIZE CAPTURE 
          N_BKG_Transplant_PS_GENUS3<-nrow(sample_data(Transplant_PS_GENUS)[sample_data(Transplant_PS_GENUS)$sample_type %in% c("BKG")])#### BKG SAMPLE SIZE CAPTURE 
          N_control_Transplant_PS_GENUS3<-nrow(sample_data(Transplant_PS_GENUS)[sample_data(Transplant_PS_GENUS)$sample_type %in% c("MOC", "DFW", "Blank")])#### Control SAMPLE SIZE CAPTURE 
          
          Taxa_Transplant_PS_GENUS3<-nrow(tax_table(Transplant_PS_GENUS))#### TAXA SIZE CAPTURE
          Subject_Transplant_PS_GENUS3<-length(unique(sample_data(Transplant_PS_GENUS)$SubjID)[! unique(sample_data(Transplant_PS_GENUS)$SubjID) %in% c("MOC", "DFW", "Blank")])#### Subject Sample SIZE CAPTURE

Transplant_PS_GENUS_relab = transformSampleCounts(Transplant_PS_GENUS,normalizeSample) ##relative abundance

read_count_KW(Transplant_PS_GENUS, "sample_type", 
              c("BKG", "Lower", "Upper", "MOC", "DFW", "Blank"), c("darkgoldenrod1", "steelblue2", "purple2", "red", "black", "seagreen4"), 
              xlabel_size=14, ylabel_size=13, axis_title_size=15,
              p_value="no", output="read_count_all_nonegcontrol_GENUS")
alpha_diversity_KW(Transplant_PS_GENUS, "sample_type", 
                   c("BKG", "Lower", "Upper", "MOC", "DFW", "Blank"), c("darkgoldenrod1", "steelblue2", "purple2", "red", "black", "seagreen4"), 
                   xlabel_size=14, ylabel_size=13, axis_title_size=15,
                   p_value="no", output="alpha_diversity_all_GENUS")
beta_diversity_KW(Transplant_PS_GENUS_relab, "sample_type", 
                  c("BKG", "Lower", "Upper", "MOC", "DFW", "Blank"), c("darkgoldenrod1", "steelblue2", "purple2", "red", "black", "seagreen4"), 
                  p_value="yes", output="beta_diversity_all_GENUS", x_axis_flip = "yes", p_value_location = "BL")

##keeping only subjects who has bronch within the first 72 hours
Transplant_PS_PGD_GENUS <- subset_samples(Transplant_PS_GENUS, SubjID %in% transplant_final_PGD_samplelist)
          N_Transplant_PS_PGD_GENUS1<-nrow(sample_data(Transplant_PS_PGD_GENUS))#### SAMPLE SIZE CAPTURE 
          N_upper_Transplant_PS_PGD_GENUS1<-nrow(sample_data(Transplant_PS_PGD_GENUS)[sample_data(Transplant_PS_PGD_GENUS)$sample_type %in% c("Upper")])#### Upper SAMPLE SIZE CAPTURE 
          N_lower_Transplant_PS_PGD_GENUS1<-nrow(sample_data(Transplant_PS_PGD_GENUS)[sample_data(Transplant_PS_PGD_GENUS)$sample_type %in% c("Lower")])#### Lower SAMPLE SIZE CAPTURE 
          N_BKG_Transplant_PS_PGD_GENUS1<-nrow(sample_data(Transplant_PS_PGD_GENUS)[sample_data(Transplant_PS_PGD_GENUS)$sample_type %in% c("BKG")])#### BKG SAMPLE SIZE CAPTURE 
          
          Taxa_Transplant_PS_PGD_GENUS1<-nrow(tax_table(Transplant_PS_PGD_GENUS))#### TAXA SIZE CAPTURE 
          Subject_Transplant_PS_PGD_GENUS1<-length(unique(sample_data(Transplant_PS_PGD_GENUS)$SubjID)[! unique(sample_data(Transplant_PS_PGD_GENUS)$SubjID) %in% c("MOC", "DFW", "Blank")])#### Subject Sample SIZE CAPTURE

Transplant_PS_PGD_GENUS_relab =  subset_samples(Transplant_PS_GENUS_relab, SubjID %in% transplant_final_PGD_samplelist)

#exclude MOC, blank and neg
Transplant_PS_PGD_GENUS_all = subset_samples(Transplant_PS_PGD_GENUS, sample_type  %in% c("Upper", "Lower", "BKG"))
Transplant_PS_PGD_GENUS_all_relab = subset_samples(Transplant_PS_PGD_GENUS_relab, sample_type  %in% c("Upper", "Lower", "BKG"))

####Decontaminant method for PGD
PGD_decontam_method<-"freq"
PGD_decontam_test_threshold<-0.5

Total_ddPCR_edit<-Total_ddPCR[,-3] #making sure that the bacterial_load file only has two columns: first column being the sample ID and then second column being the count
#There are sample ID that is label inconsistently in ddPCR 

Total_ddPCR_edit$SampleID[Total_ddPCR_edit$SampleID=="UNIV.0080.BAL.RML.RL.4.218.275"] <- "UNIV.0080.BAL.RML.DL.4.218.275"
Total_ddPCR_edit$SampleID[Total_ddPCR_edit$SampleID=="UNIV.0122.BAL.RML.DL.5" & Total_ddPCR_edit$ddPCR_count==35] <- "UNIV.0122.BAL.RML.DL.6" 
Total_ddPCR_edit$SampleID[Total_ddPCR_edit$SampleID=="UNIV.0206.BAL.RML.DL.3" & Total_ddPCR_edit$ddPCR_count==((380/3)*70/200)] <- "UNIV.0206.BAL.RML.DL.3.2" 
Total_ddPCR_edit$SampleID[Total_ddPCR_edit$SampleID=="UNIV.0232.BAL.RML.DL.2" & Total_ddPCR_edit$ddPCR_count==((500/3)*70/200)] <- "UNIV.0232.BAL.RML.DL.2.2" 
Total_ddPCR_edit$SampleID[Total_ddPCR_edit$SampleID=="UNIV.0238.BAL.Ling.DL.4"] <- "UNIV.0238.BAL.Ling.DL.4.2" 
Total_ddPCR_edit$SampleID[Total_ddPCR_edit$SampleID=="UNIV.0238.BAL.RML.RL.4" & Total_ddPCR_edit$ddPCR_count==56] <- "UNIV.0238.BAL.RML.RL.4.2"
Total_ddPCR_edit$SampleID[Total_ddPCR_edit$SampleID=="TX/0065.BAL.RML.DL.3"] <- "TX.0065.BAL.RML.DL.3" 
Total_ddPCR_edit$SampleID[Total_ddPCR_edit$SampleID=="UNIV.0186.BAL.RML.DL.5.dupe(.6)"] <- "UNIV.0186.BAL.RML.DL.5.dupe"
Total_ddPCR_edit$SampleID[Total_ddPCR_edit$SampleID=="UNIV.0250.BAL.RML.RL.4.dupe(.5)"] <- "UNIV.0250.BAL.RML.RL.4.dupe"
Total_ddPCR_edit$SampleID[Total_ddPCR_edit$SampleID=="UNIV.0281.Supraglottic.4.dupe"] <- "UNIV.0281.Supraglottic.4.duplicate"

####Supp Figure 3####
decontaminant_subplot_KW(Transplant_PS_PGD_GENUS_all,
                   sample_type_var_name="sample_type", 
                   sample_types=c("BKG", "Lower", "Upper"), 
                   sample_type_color= c("darkgoldenrod1", "steelblue2", "purple2"),
                   negative_sample_type=c("BKG"),
                   compare_type=c("Lower_and_Upper"),
                   method_type=PGD_decontam_method, 
                   stat_option = "mean", 
                   #bacterial_load=Total_ddPCR_edit,
                   test_threshold=PGD_decontam_test_threshold,
                   graph_option="boxplot", log_scale="yes",
                   output_suffix="GENUS_PGD",
                   taxa_genus_output = "yes")

###PRUNING STRATEGY for PGD
PGD_prune_read_cut_off<-10
PGD_prune_percent_cut_off<-0.02

####for lower airway samples keep only donor lung sample and 1 sample per date of collection, samples within 3 days and first bronch sample
####for upper and BKG samples- keep all samples 
Transplant_PS_PGD_GENUS_all2 <- subset_samples(Transplant_PS_PGD_GENUS, (sample_type=="Lower" &
                                                        native_lung==F & bronch_index==1 &
                                                        bronch_within3days==1 & first_bronch_date==1) |
                                                        sample_type=="Upper" | sample_type=="BKG")
Transplant_PS_PGD_GENUS_all2_relab = subset_samples(Transplant_PS_PGD_GENUS_relab, (sample_type=="Lower" &
                                                                   native_lung==F & bronch_index==1 &
                                                                   bronch_within3days==1 & first_bronch_date==1) |
                                                                   sample_type=="Upper" | sample_type=="BKG")

#export for SRA export
write.csv(sample_data(Transplant_PS_PGD_GENUS_all2),"Transplant_PS_PGD_GENUS_meta.csv")


Transplant_PS_PGD_GENUS_UL = subset_samples(Transplant_PS_PGD_GENUS_all2, sample_type  %in% c("Upper", "Lower"))
          N_Transplant_PS_PGD_GENUS2a<-nrow(sample_data(Transplant_PS_PGD_GENUS_UL))#### SAMPLE SIZE CAPTURE
          N_upper_Transplant_PS_PGD_GENUS2a<-nrow(sample_data(Transplant_PS_PGD_GENUS_UL)[sample_data(Transplant_PS_PGD_GENUS_UL)$sample_type %in% c("Upper")])#### Upper SAMPLE SIZE CAPTURE 
          N_lower_Transplant_PS_PGD_GENUS2a<-nrow(sample_data(Transplant_PS_PGD_GENUS_UL)[sample_data(Transplant_PS_PGD_GENUS_UL)$sample_type %in% c("Lower")])#### Lower SAMPLE SIZE CAPTURE 
          
          Taxa_Transplant_PS_PGD_GENUS2a<-nrow(tax_table(Transplant_PS_PGD_GENUS_UL))#### TAXA SIZE CAPTURE 
          Subject_Transplant_PS_PGD_GENUS2a<-length(unique(sample_data(Transplant_PS_PGD_GENUS_UL)$SubjID)[! unique(sample_data(Transplant_PS_PGD_GENUS_UL)$SubjID) %in% c("MOC", "DFW", "Blank")])#### Subject Sample SIZE CAPTURE

Transplant_PS_PGD_GENUS_UL_relab = subset_samples(Transplant_PS_PGD_GENUS_all2_relab, sample_type  %in% c("Upper", "Lower"))

#using pruning based on strategy count cut off 10 and percent of sample 2%
prune_phylo_KW(Transplant_PS_PGD_GENUS_UL, count_cut_off=PGD_prune_read_cut_off, percent_with_cut_off=PGD_prune_percent_cut_off, "Transplant_PS_PGD_GENUS_UL_prune")
          N_Transplant_PS_PGD_GENUS3a<-nrow(sample_data(Transplant_PS_PGD_GENUS_UL_prune))#### SAMPLE SIZE CAPTURE 
          N_upper_Transplant_PS_PGD_GENUS3a<-nrow(sample_data(Transplant_PS_PGD_GENUS_UL_prune)[sample_data(Transplant_PS_PGD_GENUS_UL_prune)$sample_type %in% c("Upper")])#### Upper SAMPLE SIZE CAPTURE 
          N_lower_Transplant_PS_PGD_GENUS3a<-nrow(sample_data(Transplant_PS_PGD_GENUS_UL_prune)[sample_data(Transplant_PS_PGD_GENUS_UL_prune)$sample_type %in% c("Lower")])#### Lower SAMPLE SIZE CAPTURE 
          
          Taxa_Transplant_PS_PGD_GENUS3a<-nrow(tax_table(Transplant_PS_PGD_GENUS_UL_prune))#### TAXA SIZE CAPTURE 
          Subject_Transplant_PS_PGD_GENUS3a<-length(unique(sample_data(Transplant_PS_PGD_GENUS_UL_prune)$SubjID)[! unique(sample_data(Transplant_PS_PGD_GENUS_UL_prune)$SubjID) %in% c("MOC", "DFW", "Blank")])#### Subject Sample SIZE CAPTURE
Transplant_PS_PGD_GENUS_UL_relab_prune<-keep_taxa_KW(Transplant_PS_PGD_GENUS_UL_relab,Transplant_PS_PGD_GENUS_UL_prune)#keeps the taxa in Transplant_PS_PGD_GENUS_UL_relab that is contained in Transplant_PS_PGD_GENUS_UL_prune  

##by sample type 
#ddPCR read count
phyloseq_keep_ddPCR_GENUS_PGD_all2 <-data.frame(sample_data(Transplant_PS_PGD_GENUS_all2)[,c("SampleID")])
Total_ddPCR_GENUS_PGD_all2 <- merge(Total_ddPCR, phyloseq_keep_ddPCR_GENUS_PGD_all2, by="SampleID") #keeping only the samples we care about (processed phyloseq)
####Supp Figure 2A####
read_count_KW(Total_ddPCR_GENUS_PGD_all2, "sample_type", 
              c("BKG", "Lower", "Upper"), c("darkgoldenrod1", "steelblue2", "purple2"), 
              xlabel_size=14, ylabel_size=13, axis_title_size=15,
              p_value="yes", output="ddPCR_all_PGD_GENUS", ylabel="Bacterial concentration (log10 copies per uL)", count_variable="ddPCR_count")
####Supp Figure 2B####
#16S data
read_count_KW(Transplant_PS_PGD_GENUS_all2, "sample_type", 
              c("BKG", "Lower", "Upper"), c("darkgoldenrod1", "steelblue2", "purple2"), 
              xlabel_size=14, ylabel_size=13, axis_title_size=15, 
              p_value="yes", output="read_count_all_PGD_GENUS")
####Supp Figure 2B####
alpha_diversity_KW(Transplant_PS_PGD_GENUS_all2, "sample_type", 
                   c("BKG", "Lower", "Upper"), c("darkgoldenrod1", "steelblue2", "purple2"), 
                   xlabel_size=14, ylabel_size=13, axis_title_size=15,
                   p_value="yes", output="alpha_diversity_all_PGD_GENUS")
####Supp Figure 2D####
beta_diversity_KW(Transplant_PS_PGD_GENUS_all2_relab, "sample_type", 
                  c("BKG", "Lower", "Upper"), c("darkgoldenrod1", "steelblue2", "purple2"), 
                  p_value="yes", output="beta_diversity_all_PGD_GENUS", p_value_location="BL")
### get p value for the individual comparsion 
beta_diversity_KW(Transplant_PS_PGD_GENUS_all_relab, "sample_type", 
                  c("Lower", "BKG"), c("steelblue2", "darkgoldenrod1"), 
                  p_value="yes", output="beta_diversity_LB_PGD_GENUS")
beta_diversity_KW(Transplant_PS_PGD_GENUS_all_relab, "sample_type", 
                  c("Upper", "BKG"), c( "purple2", "darkgoldenrod1"), 
                  p_value="yes", output="beta_diversity_UB_PGD_GENUS")
beta_diversity_KW(Transplant_PS_PGD_GENUS_all_relab, "sample_type", 
                  c("Upper", "Lower"), c("purple2", "steelblue2"), 
                  p_value="yes", output="beta_diversity_UL_PGD_GENUS")
####Supp Figure 4####
####use pruned data for differential
Edger_phylo_KW(Transplant_PS_PGD_GENUS_UL_prune, 
         variable_to_compare="sample_type",
         outcome_to_compare=c("Lower", "Upper"),
         outcome_to_compare_color=c("steelblue2", "purple2"),
         FDR_cut_off=0.2, number_display=25,
         legend_onplot="yes",
         output_name="Edger_bronch_Lower_Upper_PGD_GENUS", 
         decontam="Contam_list_NC_BKG_compare_Lower_and_Upper_GENUS_PGD",
         taxa_genus_output="yes")

####keep only lower airway samples
####keep only donor lung sample and 1 sample per date of collection
####keep only samples within 3 days and first bronch sample
Transplant_PS_PGD_GENUS_lower <- subset_samples(Transplant_PS_PGD_GENUS, sample_type=="Lower")
      N_Transplant_PS_PGD_GENUS2<-nrow(sample_data(Transplant_PS_PGD_GENUS_lower))#### SAMPLE SIZE CAPTURE 
      Taxa_Transplant_PS_PGD_GENUS2<-nrow(tax_table(Transplant_PS_PGD_GENUS_lower))#### TAXA SIZE CAPTURE 
      Subject_Transplant_PS_PGD_GENUS2<-length(unique(sample_data(Transplant_PS_PGD_GENUS_lower)$SubjID)[! unique(sample_data(Transplant_PS_PGD_GENUS_lower)$SubjID) %in% c("MOC", "DFW", "Blank")])#### Subject Sample SIZE CAPTURE

Transplant_PS_PGD_GENUS_lower <- subset_samples(Transplant_PS_PGD_GENUS_lower, native_lung==F & bronch_index==1)
      N_Transplant_PS_PGD_GENUS3<-nrow(sample_data(Transplant_PS_PGD_GENUS_lower))#### SAMPLE SIZE CAPTURE 
      Taxa_Transplant_PS_PGD_GENUS3<-nrow(tax_table(Transplant_PS_PGD_GENUS_lower))#### TAXA SIZE CAPTURE 
      Subject_Transplant_PS_PGD_GENUS3<-length(unique(sample_data(Transplant_PS_PGD_GENUS_lower)$SubjID)[! unique(sample_data(Transplant_PS_PGD_GENUS_lower)$SubjID) %in% c("MOC", "DFW", "Blank")])#### Subject Sample SIZE CAPTURE

Transplant_PS_PGD_GENUS_lower <- subset_samples(Transplant_PS_PGD_GENUS_lower, bronch_within3days==1 & first_bronch_date==1)
      N_Transplant_PS_PGD_GENUS4<-nrow(sample_data(Transplant_PS_PGD_GENUS_lower))#### SAMPLE SIZE CAPTURE 
      Taxa_Transplant_PS_PGD_GENUS4<-nrow(tax_table(Transplant_PS_PGD_GENUS_lower))#### TAXA SIZE CAPTURE 
      Subject_Transplant_PS_PGD_GENUS4<-length(unique(sample_data(Transplant_PS_PGD_GENUS_lower)$SubjID)[! unique(sample_data(Transplant_PS_PGD_GENUS_lower)$SubjID) %in% c("MOC", "DFW", "Blank")])#### Subject Sample SIZE CAPTURE

#relab (lower airway, donor sample; 1 sample per date of collection; samples within 3 days; first bronch sample)
Transplant_PS_PGD_GENUS_lower_relab = subset_samples(Transplant_PS_PGD_GENUS_relab, sample_type  %in% c("Lower") &
                                           native_lung==F & bronch_index==1 & 
                                           bronch_within3days==1 & first_bronch_date==1)  

#using pruning based on strategy count cut off 10 and percent of sample 2%
prune_phylo_KW(Transplant_PS_PGD_GENUS_lower, count_cut_off=PGD_prune_read_cut_off, percent_with_cut_off=PGD_prune_percent_cut_off, "Transplant_PS_PGD_GENUS_lower_prune")
      N_Transplant_PS_PGD_GENUS5<-nrow(sample_data(Transplant_PS_PGD_GENUS_lower_prune))#### SAMPLE SIZE CAPTURE 
      Taxa_Transplant_PS_PGD_GENUS5<-nrow(tax_table(Transplant_PS_PGD_GENUS_lower_prune))#### TAXA SIZE CAPTURE 
      Subject_Transplant_PS_PGD_GENUS5<-length(unique(sample_data(Transplant_PS_PGD_GENUS_lower_prune)$SubjID)[! unique(sample_data(Transplant_PS_PGD_GENUS_lower_prune)$SubjID) %in% c("MOC", "DFW", "Blank")])#### Subject Sample SIZE CAPTURE

Transplant_PS_PGD_GENUS_lower_relab_prune<-keep_taxa_KW(Transplant_PS_PGD_GENUS_lower_relab,Transplant_PS_PGD_GENUS_lower_prune)#keeps the taxa in Transplant_PS_PGD_GENUS_UL_relab that is contained in Transplant_PS_PGD_GENUS_lower_prune  

#####highest grade PGD 01 vs (2, 3)
#ddPCR read count
phyloseq_keep_ddPCR_PGD_GENUS_lower <-data.frame(sample_data(Transplant_PS_PGD_GENUS_lower)[,c("SampleID", "SubjID", "pgd_highest_48_72_v3_rename")])
Total_ddPCR_PGD_GENUS_lower <- merge(Total_ddPCR, phyloseq_keep_ddPCR_PGD_GENUS_lower, by="SampleID") #keeping only the samples we care about (processed phyloseq)
read_count_KW(Total_ddPCR_PGD_GENUS_lower, "pgd_highest_48_72_v3_rename", 
              c("No PGD","Moderate","Severe"), c("palegreen3","lightsalmon","indianred3"), 
              xlabel_size=14, ylabel_size=13, axis_title_size=15,
              p_value="yes", output="ddPCR_PGD_GENUS_lower",ylabel="Bacterial concentration (log10 copies per uL)", count_variable="ddPCR_count")

#pepsin
Lower_pepsin_PGD_GENUS_lower<- merge(Lower_pepsin, phyloseq_keep_ddPCR_PGD_GENUS_lower, by="SubjID") #keeping only the samples we care about (processed phyloseq)
read_count_KW(Lower_pepsin_PGD_GENUS_lower, "pgd_highest_48_72_v3_rename", 
              c("No PGD","Moderate","Severe"), c("palegreen3","lightsalmon","indianred3"), 
              xlabel_size=14, ylabel_size=13, axis_title_size=15, y_log_scale = "yes",
              p_value="yes", output="pepsin_PGD_GENUS_lower",ylabel="Pepsin concentration (log10 Pmol/min/ml)", count_variable="pepsin")

#16S data
######Figure 1A######
read_count_KW(Transplant_PS_PGD_GENUS_lower, "pgd_highest_48_72_v3_rename", 
              c("No PGD","Moderate","Severe"), c("palegreen3","lightsalmon","indianred3"), 
              xlabel_size=14, ylabel_size=13, axis_title_size=15,
              p_value="yes", output="read_count_PGD_01_2_3_GENUS")
######Figure 1B######
alpha_diversity_KW(Transplant_PS_PGD_GENUS_lower, "pgd_highest_48_72_v3_rename", 
                   c("No PGD","Moderate","Severe"), c("palegreen3","lightsalmon","indianred3"), 
                   xlabel_size=14, ylabel_size=13, axis_title_size=15, 
                   p_value="yes","alpha_PGD_01_2_3_GENUS")
######Figure 1C######
beta_diversity_KW(Transplant_PS_PGD_GENUS_lower_relab, "pgd_highest_48_72_v3_rename", 
                  c("No PGD","Moderate","Severe"), c("palegreen3","lightsalmon","indianred3"), 
                  p_value="yes", output="beta_PGD_01_2_3_GENUS")
#beta for pair comparison
beta_diversity_KW(Transplant_PS_PGD_GENUS_lower_relab, "pgd_highest_48_72_v3_rename", 
                  c("No PGD","Moderate"), c("palegreen3","lightsalmon"), 
                  p_value="yes", output="beta_PGD_01_2_GENUS")
beta_diversity_KW(Transplant_PS_PGD_GENUS_lower_relab, "pgd_highest_48_72_v3_rename", 
                  c("No PGD","Severe"), c("palegreen3","indianred3"), 
                  p_value="yes", output="beta_PGD_01_3_GENUS")
beta_diversity_KW(Transplant_PS_PGD_GENUS_lower_relab, "pgd_highest_48_72_v3_rename", 
                  c("Moderate","Severe"), c("lightsalmon","indianred3"), 
                  p_value="yes", output="beta_PGD_2_3_GENUS")
######Figure 1D######
multi_compare_KW(Transplant_PS_PGD_GENUS_lower_prune, 
                 variable_to_compare="pgd_highest_48_72_v3_rename",
                 control_group="No PGD",
                 control_group_color="palegreen3",
                 treatment_group=c("Moderate","Severe"), 
                 treatment_group_color=c("lightsalmon","indianred3"),
                 adj_p_cut_off=0.2,
                 number_display=25,
                 legend_onplot="yes",
                 diff_method="Edger",
                 output_name="multi_compare_Edger_PGD_01_v_2_3_GENUS",
                 decontam="Contam_list_NC_BKG_compare_Lower_and_Upper_GENUS_PGD",
                 width=200, 
                 height=220,
                 font_size_adjust_percent=90,
                 taxa_genus_output="yes")

##################DMM - clustering#################
#on pruned BAL data
####Figure 2E####
####Supp Figure 5####
DMM_cluster_KW(input_phyloseq=Transplant_PS_PGD_GENUS_lower_prune,
         variable_to_compare="pgd_highest_48_72_v3_rename",
         max_cluster=5,
         phyloseq_uniqueID="SampleID",
         output_name="PGD_GENUS_lower_prune",                
         DMM_cluster_color=c("cornflowerblue","hotpink1"),
         compare_stats="fisher")

write.csv(DMM_cluster_KW_PGD_GENUS_lower_prune, "DMM_cluster_KW_PGD_GENUS_lower_prune.csv")

####compare DMM cluster and cell count cluster
merge_clusters<-merge(DMM_cluster_KW_PGD_GENUS_lower_prune, beta_cluster_input_cell_count_output)

confusion_matrix <- table(merge_clusters$Cluster_num, merge_clusters$Cell_Count_Cluster)
# Convert the confusion matrix to a graphical object
table_grob <- tableGrob(confusion_matrix)
# Create a new PDF file
pdf("compare_clusters_matrix.pdf")
# Save the table as PDF
    grid.arrange(table_grob)
# Close the PDF device
dev.off()

#bacterial read count
phyloseq_keep_ddPCR_PGD_GENUS_lower_prune_DMM <-data.frame(sample_data(Transplant_PS_PGD_GENUS_lower_prune_DMM)[,c("SampleID","SubjID", "Cluster_num", "pgd_highest_48_72_v3_rename")])
Total_ddPCR_PGD_GENUS_lower_prune_DMM <- merge(Total_ddPCR, phyloseq_keep_ddPCR_PGD_GENUS_lower_prune_DMM, by="SampleID") #keeping only the samples we care about (processed phyloseq)
#ddPCR-separate by only cluster
read_count_KW(Total_ddPCR_PGD_GENUS_lower_prune_DMM, "Cluster_num", 
              c("1", "2"), c("cornflowerblue", "hotpink1"), 
              xlabel_size=14, ylabel_size=13, axis_title_size=15,
              p_value="yes", output="ddPCR_GENUS_lower_prune_clusteronly",ylabel="Bacterial concentration (log10 copies per uL)", count_variable="ddPCR_count")
#ddPCR-separate by only cluster and PGD
read_count_v2_KW(Total_ddPCR_PGD_GENUS_lower_prune_DMM, 
                 sample_type_var_name="pgd_highest_48_72_v3_rename", sample_types=c("No PGD","Moderate","Severe"), sample_type_color=c("palegreen3","lightsalmon","indianred3"), 
                 sample_type2_var_name="Cluster_num", sample_types2=c("1", "2"), sample_type2_color=c("cornflowerblue", "hotpink1"), 
                 xlabel_size=14, ylabel_size=13, axis_title_size=15,
                 p_value="yes", p_value_select = "both",
                 output="ddPCR_GENUS_lower_prune_cluster", ylabel="Bacterial concentration (log10 copies per uL)", count_variable="ddPCR_count")

#16S data
#separate by only cluster
####Figure 2A####
read_count_KW(Transplant_PS_PGD_GENUS_lower_prune_DMM, "Cluster_num", 
              c("1", "2"), c("cornflowerblue", "hotpink1"), 
              xlabel_size=14, ylabel_size=13, axis_title_size=15,
              p_value="yes", output="read_count_GENUS_lower_prune_clusteronly", count_variable="ddPCR_count")
#separate by only cluster and PGD
read_count_v2_KW(Transplant_PS_PGD_GENUS_lower_prune_DMM, 
                 sample_type_var_name="pgd_highest_48_72_v3_rename", sample_types=c("No PGD","Moderate","Severe"), sample_type_color=c("palegreen3","lightsalmon","indianred3"), 
                 sample_type2_var_name="Cluster_num", sample_types2=c("1", "2"), sample_type2_color=c("cornflowerblue", "hotpink1"), 
                 xlabel_size=14, ylabel_size=13, axis_title_size=15, 
                 p_value="yes", p_value_select = "both",output="read_count_GENUS_lower_prune_cluster")
####Figure 2B####
#separate by only cluster
alpha_diversity_KW(Transplant_PS_PGD_GENUS_lower_prune_DMM, "Cluster_num", 
                   c("1", "2"), c("cornflowerblue", "hotpink1"), 
                   xlabel_size=14, ylabel_size=13, axis_title_size=15, 
                   p_value="yes", output="alpha_diversity_GENUS_lower_prune_clusteronly")
#separate by only cluster and PGD
alpha_diversity_v2_KW(Transplant_PS_PGD_GENUS_lower_prune_DMM, 
                      sample_type_var_name="pgd_highest_48_72_v3_rename", sample_types=c("No PGD","Moderate","Severe"), sample_type_color=c("palegreen3","lightsalmon","indianred3"), 
                      sample_type2_var_name="Cluster_num", sample_types2=c("1", "2"), sample_type2_color=c("cornflowerblue", "hotpink1"), 
                      xlabel_size=14, ylabel_size=13, axis_title_size=15, 
                      p_value="yes", p_value_select = "both",output="alpha_diversity_GENUS_lower_prune_cluster")
####Figure 2C####
beta_diversity_KW(Transplant_PS_PGD_GENUS_lower_prune_DMM, "Cluster_num", c("1", "2"), 
            c("cornflowerblue", "hotpink1"), 
            p_value="yes", 
            output="beta_PGD_prune_DMM")

beta_diversity_KW(Transplant_PS_PGD_GENUS_lower_relab, "pgd_highest_48_72_v3_rename", c("No PGD","Moderate","Severe"), 
                  c("palegreen3","lightsalmon","indianred3"), 
                  p_value="yes", 
                  output="beta_PGD_01_2_3_prune_DMM", 
                  DMM_cluster_assign="DMM_cluster_KW_PGD_GENUS_lower_prune",
                  DMM_cluster_color=c("cornflowerblue","hotpink1"))


####Figure 2D####
Edger_phylo_KW(Transplant_PS_PGD_GENUS_lower_prune_DMM, 
         variable_to_compare="Cluster_num",
         outcome_to_compare=c("1", "2"),
         outcome_to_compare_color=c("cornflowerblue", "hotpink1"),
         FDR_cut_off=0.2, number_display=25,
         legend_onplot="yes",
         output_name="Edger_bronch_Lower_GENUS_prune_cluster", 
         decontam="Contam_list_NC_BKG_compare_Lower_and_Upper_GENUS_PGD",
         taxa_genus_output="yes")

##################Cytokine Cluster##################
Transplant_PS_PGD_GENUS_lower_prune_cytokine = subset_samples(Transplant_PS_PGD_GENUS_lower_prune, !is.na(Cytokine_Cluster))
####Figure 4B####                                                              
read_count_KW(Transplant_PS_PGD_GENUS_lower_prune_cytokine, "Cytokine_Cluster", 
              c("Cytokine Cluster 1", "Cytokine Cluster 2"), c("orange", "blue2"), 
              xlabel_size=14, ylabel_size=13, axis_title_size=15,
              p_value="yes", output="read_count_GENUS_lower_prune_cytokine_clusteronly", count_variable="ddPCR_count")
#separate by only cluster and PGD
read_count_v2_KW(Transplant_PS_PGD_GENUS_lower_prune_cytokine, 
                 sample_type_var_name="pgd_highest_48_72_v3_rename", sample_types=c("No PGD","Moderate","Severe"), sample_type_color=c("palegreen3","lightsalmon","indianred3"), 
                 sample_type2_var_name="Cytokine_Cluster", sample_types2=c("Cytokine Cluster 1", "Cytokine Cluster 2"), sample_type2_color=c("orange", "blue2"), 
                 xlabel_size=14, ylabel_size=13, axis_title_size=15, 
                 p_value="yes", p_value_select = "both",output="read_count_GENUS_lower_prune_cytokine_cluster")
####Figure 4C####
#separate by only cluster
alpha_diversity_KW(Transplant_PS_PGD_GENUS_lower_prune_cytokine, "Cytokine_Cluster", 
                   c("Cytokine Cluster 1", "Cytokine Cluster 2"), c("orange", "blue2"), 
                   xlabel_size=14, ylabel_size=13, axis_title_size=15, 
                   p_value="yes", output="alpha_diversity_GENUS_lower_prune_cytokine_clusteronly")
#separate by cluster and PGD
alpha_diversity_v2_KW(Transplant_PS_PGD_GENUS_lower_prune_cytokine, 
                      sample_type_var_name="pgd_highest_48_72_v3_rename", sample_types=c("No PGD","Moderate","Severe"), sample_type_color=c("palegreen3","lightsalmon","indianred3"), 
                      sample_type2_var_name="Cytokine_Cluster", sample_types2=c("Cytokine Cluster 1", "Cytokine Cluster 2"), sample_type2_color=c("orange", "blue2"), 
                      xlabel_size=14, ylabel_size=13, axis_title_size=15, 
                      p_value="yes", p_value_select = "both",output="alpha_diversity_GENUS_lower_prune_cytokine_cluster")

Transplant_PS_PGD_GENUS_lower_relab_cytokine = subset_samples(Transplant_PS_PGD_GENUS_lower_relab, !is.na(Cytokine_cluster))

####Figure 4D####
#beta diversity by only cluster
beta_diversity_KW(Transplant_PS_PGD_GENUS_lower_prune_cytokine, "Cytokine_Cluster", 
                  c("Cytokine Cluster 1", "Cytokine Cluster 2"), c("orange", "blue2"), 
                  p_value="yes", 
                  output="beta_diversity_GENUS_lower_prune_cytokine_clusteronly")

#beta diversity by cluster and PGD
beta_diversity_KW(Transplant_PS_PGD_GENUS_lower_prune_cytokine, 
                  sample_type_var_name="pgd_highest_48_72_v3_rename", sample_types=c("No PGD","Moderate","Severe"), sample_type_color=c("palegreen3","lightsalmon","indianred3"), 
                  sample_type2_var_name="Cytokine_Cluster", sample_types2=c("Cytokine Cluster 1", "Cytokine Cluster 2"), sample_type2_color=c("orange", "blue2"), 
                  p_value="yes", 
                  output="beta_diversity_GENUS_lower_prune_cytokine_cluster")

beta_diversity_KW(Transplant_PS_PGD_GENUS_lower_prune_cytokine, 
                  sample_type_var_name="Cytokine_Cluster", sample_types=c("Cytokine Cluster 1", "Cytokine Cluster 2"), sample_type_color=c("orange", "blue2"), 
                  sample_type2_var_name="pgd_highest_48_72_v3_rename", sample_types2=c("No PGD","Moderate","Severe"), sample_type2_color=c("palegreen3","lightsalmon","indianred3"), 
                  p_value="yes", 
                  output="beta_diversity_GENUS_lower_prune_cytokine_cluster_v2")

#beta diversity by cluster and DMM
beta_diversity_KW(Transplant_PS_PGD_GENUS_lower_prune_cytokine, "Cytokine_Cluster", c("Cytokine Cluster 1", "Cytokine Cluster 2"), 
                  c("orange", "blue2"), 
                  p_value="yes", 
                  output="beta_diversity_GENUS_lower_prune_cytokine_cluster_DMM", 
                  DMM_cluster_assign="DMM_cluster_KW_PGD_GENUS_lower_prune",
                  DMM_cluster_color=c("cornflowerblue","hotpink1"))


####Figure 4E####
Edger_phylo_KW(Transplant_PS_PGD_GENUS_lower_prune_cytokine, 
               variable_to_compare="Cytokine_Cluster",
               outcome_to_compare=c("Cytokine Cluster 1", "Cytokine Cluster 2"),
               outcome_to_compare_color=c("orange", "blue2"),
               FDR_cut_off=0.2, number_display=25,
               legend_onplot="yes",
               output_name="Edger_bronch_Lower_GENUS_prune_cytokine_cluster", 
               decontam="Contam_list_NC_BKG_compare_Lower_and_Upper_GENUS_PGD",
               taxa_genus_output="yes")
                                                            
#################FLOWCHART FOR SAMPLE#################
pdf(file="PGD_Cross_sectional_analysis_GENUS.pdf", width = 16, height = 20)

grid.newpage()
PGD_flow_prune <- boxGrob(glue("Prune Strategy:", "read count < {read}", "{percent}% of samples",
                               read = PGD_prune_read_cut_off, percent =PGD_prune_percent_cut_off * 100,
                               .sep = "\n"),
                          box_gp = gpar(fill = "lightyellow", col = "darkblue"))

Decontam_method_PGD <- boxGrob(glue("Identify Decontaminant",
                                    "method: {decontam_method}",
                                    "test threshold: {decontam_test_threshold}",
                                    decontam_method = PGD_decontam_method,
                                    decontam_test_threshold = PGD_decontam_test_threshold,
                                    .sep = "\n"),
                               box_gp = gpar(fill = "pink", col = "black"))

PGD_org_cohort <- boxGrob(glue("Initial Transplant Cohort", "N = {pop} ({upper}U, {lower}L, {BKG}B, {control}C)", "Taxa = {taxa}", "Unique Subjects = 113", #Nat wants subject number to be 113 instead of {Subject}
                               upper= N_upper_Transplant_PS1, lower=N_lower_Transplant_PS1, BKG=N_BKG_Transplant_PS1, control=N_control_Transplant_PS1,
                               pop = N_Transplant_PS1, taxa =Taxa_Transplant_PS1, Subject=Subject_Transplant_PS1,
                               .sep = "\n"),
                          box_gp = gpar(fill = "grey", col = "black", alpha=0.1))

PGD_flow_1 <- boxGrob(glue("Aggragate taxa- Genus level", "N = {pop} ({upper}U, {lower}L, {BKG}B, {control}C)", "Unique Subjects = 113", #Nat wants subject number to be 113 instead of {Subject}
                           upper= N_upper_Transplant_PS_GENUS1, lower=N_lower_Transplant_PS_GENUS1, BKG=N_BKG_Transplant_PS_GENUS1, control=N_control_Transplant_PS_GENUS1,
                           pop = N_Transplant_PS_GENUS1, taxa =Taxa_Transplant_PS_GENUS1, Subject=Subject_Transplant_PS_GENUS1,
                           .sep = "\n"),
                      box_gp = gpar(fill = "blue", col = "black", alpha=0.1), x=.5, y=.8)

PGD_flow_2 <- boxGrob(glue("Remove genus with 0 abundance", "N = {pop} ({upper}U, {lower}L, {BKG}B, {control}C)", "Unique Subjects = 113", #Nat wants subject number to be 113 instead of {Subject}
                           upper= N_upper_Transplant_PS_GENUS2, lower=N_lower_Transplant_PS_GENUS2, BKG=N_BKG_Transplant_PS_GENUS2, control=N_control_Transplant_PS_GENUS2,
                           pop = N_Transplant_PS_GENUS2, taxa =Taxa_Transplant_PS_GENUS2, Subject=Subject_Transplant_PS_GENUS2,
                           .sep = "\n"),
                      box_gp = gpar(fill = "grey", col = "black", alpha=0.1))
PGD_flow_3 <- boxGrob(glue("Remove subjects with < 1000 reads", "N = {pop} ({upper}U, {lower}L, {BKG}B, {control}C)",  "Unique Subjects =113", #Nat wants subject number to be 113 instead of {Subject}
                           upper= N_upper_Transplant_PS_GENUS3, lower=N_lower_Transplant_PS_GENUS3, BKG=N_BKG_Transplant_PS_GENUS3, control=N_control_Transplant_PS_GENUS3,
                           pop = N_Transplant_PS_GENUS3, taxa =Taxa_Transplant_PS_GENUS3,  Subject=Subject_Transplant_PS_GENUS3,
                           .sep = "\n"),
                      box_gp = gpar(fill = "grey", col = "black", alpha=0.1))
PGD_flow_PGD1 <- boxGrob(glue("PGD group (exclude subjects without bronch within 3 days after transplant)", "N = {pop} ({upper}U, {lower}L, {BKG}B)", "Genus = {taxa}",  "Unique Subjects = {Subject}",
                              upper= N_upper_Transplant_PS_PGD_GENUS1, lower=N_lower_Transplant_PS_PGD_GENUS1, BKG=N_BKG_Transplant_PS_PGD_GENUS1, 
                              pop = N_Transplant_PS_PGD_GENUS1, taxa =Taxa_Transplant_PS_PGD_GENUS1, Subject=Subject_Transplant_PS_PGD_GENUS1,
                              .sep = "\n"),
                         box_gp = gpar(fill = "grey", col = "black", alpha=0.1))

PGD_flow_PGD2b <- boxGrob(glue("Include only Lower airway", "N = {pop}", "Genus = {taxa}", "Unique Subjects = {Subject}",
                               pop = N_Transplant_PS_PGD_GENUS2, taxa =Taxa_Transplant_PS_PGD_GENUS2, Subject=Subject_Transplant_PS_PGD_GENUS2,
                               .sep = "\n"),
                          box_gp = gpar(fill = "grey", col = "black", alpha=0.1))
PGD_flow_PGD3b <- boxGrob(glue("Keeping only donor lung sample and", "1 sample per date of collection", "N = {pop}", "Genus = {taxa}", "Unique Subjects = {Subject}",
                               pop = N_Transplant_PS_PGD_GENUS3, taxa =Taxa_Transplant_PS_PGD_GENUS3, Subject=Subject_Transplant_PS_PGD_GENUS3,
                               .sep = "\n"),
                          box_gp = gpar(fill = "grey", col = "black", alpha=0.1))
PGD_flow_PGD4b <- boxGrob(glue("Keeping only samples within 3 days and first bronch only", "N = {pop}", "Genus = {taxa}", "Unique Subjects = {Subject}",
                               pop = N_Transplant_PS_PGD_GENUS4, taxa =Taxa_Transplant_PS_PGD_GENUS4, Subject=Subject_Transplant_PS_PGD_GENUS4,
                               .sep = "\n"),
                          box_gp = gpar(fill = "grey", col = "black", alpha=0.1))
PGD_flow_PGD5b <- boxGrob(glue("N = {pop}", "Genus = {taxa}", "Unique Subjects = {Subject}",
                               pop = N_Transplant_PS_PGD_GENUS5, taxa =Taxa_Transplant_PS_PGD_GENUS5, Subject=Subject_Transplant_PS_PGD_GENUS5,
                               .sep = "\n"),
                          box_gp = gpar(fill = "grey", col = "black", alpha=0.1))

PGD_flow_PGD2a <- boxGrob(glue("Include only Lower airway (1 sample per date)", "and Upper airway ", "N = {pop} ({upper}U, {lower}L)", "Genus = {taxa}", "Unique Subjects = {Subject}",
                               upper= N_upper_Transplant_PS_PGD_GENUS2a, lower=N_lower_Transplant_PS_PGD_GENUS2a,
                               pop = N_Transplant_PS_PGD_GENUS2a, taxa =Taxa_Transplant_PS_PGD_GENUS2a, Subject=Subject_Transplant_PS_PGD_GENUS2a,
                               .sep = "\n"))
PGD_flow_PGD3a <- boxGrob(glue("N = {pop} ({upper}U, {lower}L)", "Genus = {taxa}", "Unique Subjects = {Subject}",
                               upper= N_upper_Transplant_PS_PGD_GENUS3a, lower=N_lower_Transplant_PS_PGD_GENUS3a,
                               pop = N_Transplant_PS_PGD_GENUS3a, taxa =Taxa_Transplant_PS_PGD_GENUS3a, Subject=Subject_Transplant_PS_PGD_GENUS3a,
                               .sep = "\n"))
vert <- spreadVertical(PGD_org_cohort,
                 PGD_flow_1,
                 PGD_flow_2,
                 PGD_flow_3,
                 PGD_flow_PGD1=PGD_flow_PGD1,
                 PGD_flow_PGD2b,
                 PGD_flow_PGD3b,
                 PGD_flow_PGD4b,
                 PGD_flow_prune,
                 PGD_flow_PGD5b,.type = "center")

Decontam_method_PGD1 <- moveBox(Decontam_method_PGD,
                          x = .8,
                          y = coords(vert$PGD_flow_PGD1)$top - coords(vert$PGD_flow_PGD1)$height*0.5)

PGD_flow_PGD2a2 <- moveBox(PGD_flow_PGD2a,
                     x = .15,
                     y = coords(vert$PGD_flow_PGD1)$top - coords(vert$PGD_flow_PGD1)$height*1.5)

grps_a <-  alignHorizontal(reference=PGD_flow_PGD2a2, PGD_flow_prune, PGD_flow_PGD3a) %>% 
spreadVertical(.from=0.42, .to=0.23,.type = "between")

vert
for (i in 1:(length(vert) - 1)) {
connectGrob(vert[[i]], vert[[i + 1]], type = "vert") %>%
print
}
Decontam_method_PGD1
PGD_flow_PGD2a2
grps_a

for (i in 1:(length(grps_a) - 1)) {
connectGrob(grps_a[[i]], grps_a[[i + 1]], type = "vert") %>%
print
}
connectGrob(PGD_flow_PGD2a2,grps_a[[1]], type = "vert")
connectGrob(vert$PGD_flow_PGD1, PGD_flow_PGD2a2, type = "Z")
connectGrob(vert$PGD_flow_PGD1,Decontam_method_PGD1, type = "horizontal")

x <- unit(0.2, "npc")
y <- unit(.8, "npc")
grid.text("Analysis level: Genus", x=x, y=y,gp=gpar(fontsize=25, fontface="bold",col="black"))
dev.off()

save.image("ACR_DMM_data.Rdata")
save.image("HPC_PGD_SPIEC_EASI.Rdata")












