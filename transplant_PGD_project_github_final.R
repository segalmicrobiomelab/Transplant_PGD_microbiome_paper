## Segal Lab
## Programmer: Kendrew Wong
## Project: Transplant PGD

#clear all objects
rm(list=ls(all.names=T))
gc()

##open up the R script with all of the function created for the project
source("KW_functions_PGD.R")

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

##install qiime2R from github directly
#devtools::install_github("jbisanz/qiime2R")
#devtools::install_github("vmikk/metagMisc")
#devtools::install_github("Sebastien-Le/YesSiR")
#devtools::install_github("thomasp85/patchwork")
#devtools::install_github("david-barnett/microViz")
#devtools::install_github("zdk123/pulsar")
#devtools::install_github("zdk123/SpiecEasi")
library(qiime2R)
library(metagMisc)
library(YesSiR)
library(patchwork)
library(microViz)
library(pulsar)
library(SpiecEasi)

###############Setting up meta file###############
transplant_BL_meta_final_PGDsample<-read.csv("transplant_PGD_meta_file.csv")
#########Table 1##########
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

# Import OTU abundance data
otu_data_file <- "transplant_PGD_otu_abundance_data.csv"
otu_data <- read.csv(otu_data_file, row.names = 1)
# Import taxonomic data
tax_data_file <- "transplant_PGD_taxonomy_data.csv"
tax_data <- read.csv(tax_data_file, row.names = 1)
# Import sample metadata
sample_metadata_file <- "transplant_PGD_sample_metadata.csv"
sample_metadata <- read.csv(sample_metadata_file, row.names = 1)
# Import phylogenetic tree
phylogenetic_tree_file <- "transplant_PGD_phylogenetic_tree.nwk"
phylogenetic_tree <- read.tree(phylogenetic_tree_file)
# Create a new phyloseq object
otu_data_matrix <- as.matrix(otu_data)
tax_data_matrix <- as.matrix(tax_data)

Transplant_PS_PGD_GENUS_all <- phyloseq(otu_table(otu_data_matrix, taxa_are_rows = TRUE),
                                tax_table(tax_data_matrix),
                                sample_data(sample_metadata),
                                phy_tree(phylogenetic_tree))

normalizeSample <- function(x){x/sum(x)}
Transplant_PS_PGD_GENUS_all_relab = transformSampleCounts(Transplant_PS_PGD_GENUS_all,normalizeSample) ##relative abundance

#export for SRA export
write.csv(sample_data(Transplant_PS_PGD_GENUS_all),"Transplant_PS_PGD_GENUS_meta.csv")

####Decontaminant method for PGD
PGD_decontam_method<-"freq"
PGD_decontam_test_threshold<-0.5
###PRUNING STRATEGY for PGD
PGD_prune_read_cut_off<-10
PGD_prune_percent_cut_off<-0.02

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

Transplant_PS_PGD_GENUS_UL = subset_samples(Transplant_PS_PGD_GENUS_all, sample_type  %in% c("Upper", "Lower"))

Transplant_PS_PGD_GENUS_UL_relab = subset_samples(Transplant_PS_PGD_GENUS_all_relab, sample_type  %in% c("Upper", "Lower"))

#using pruning based on strategy count cut off 10 and percent of sample 2%
prune_phylo_KW(Transplant_PS_PGD_GENUS_UL, count_cut_off=PGD_prune_read_cut_off, percent_with_cut_off=PGD_prune_percent_cut_off, "Transplant_PS_PGD_GENUS_UL_prune")

Transplant_PS_PGD_GENUS_UL_relab_prune<-keep_taxa_KW(Transplant_PS_PGD_GENUS_UL_relab,Transplant_PS_PGD_GENUS_UL_prune)#keeps the taxa in Transplant_PS_PGD_GENUS_UL_relab that is contained in Transplant_PS_PGD_GENUS_UL_prune  

##by sample type 
#ddPCR read count
Total_ddPCR_GENUS_PGD_all<-read.csv("Total_ddPCR_GENUS_PGD_all.csv")

####Supp Figure 2A####
read_count_KW(Total_ddPCR_GENUS_PGD_all, "sample_type", 
              c("BKG", "Lower", "Upper"), c("darkgoldenrod1", "steelblue2", "purple2"), 
              xlabel_size=14, ylabel_size=13, axis_title_size=15,
              p_value="yes", output="ddPCR_all_PGD_GENUS", ylabel="Bacterial concentration (log10 copies per uL)", count_variable="ddPCR_count")
####Supp Figure 2B####
#16S data
read_count_KW(Transplant_PS_PGD_GENUS_all, "sample_type", 
              c("BKG", "Lower", "Upper"), c("darkgoldenrod1", "steelblue2", "purple2"), 
              xlabel_size=14, ylabel_size=13, axis_title_size=15, 
              p_value="yes", output="read_count_all_PGD_GENUS")
####Supp Figure 2B####
alpha_diversity_KW(Transplant_PS_PGD_GENUS_all, "sample_type", 
                   c("BKG", "Lower", "Upper"), c("darkgoldenrod1", "steelblue2", "purple2"), 
                   xlabel_size=14, ylabel_size=13, axis_title_size=15,
                   p_value="yes", output="alpha_diversity_all_PGD_GENUS")
####Supp Figure 2D####
beta_diversity_KW(Transplant_PS_PGD_GENUS_all_relab, "sample_type", 
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
Transplant_PS_PGD_GENUS_lower <- subset_samples(Transplant_PS_PGD_GENUS_all, sample_type=="Lower")

#relab (lower airway, donor sample; 1 sample per date of collection; samples within 3 days; first bronch sample)
Transplant_PS_PGD_GENUS_lower_relab = transformSampleCounts(Transplant_PS_PGD_GENUS_lower,normalizeSample)

#using pruning based on strategy count cut off 10 and percent of sample 2%
prune_phylo_KW(Transplant_PS_PGD_GENUS_lower, count_cut_off=PGD_prune_read_cut_off, percent_with_cut_off=PGD_prune_percent_cut_off, "Transplant_PS_PGD_GENUS_lower_prune")

Transplant_PS_PGD_GENUS_lower_relab_prune<-keep_taxa_KW(Transplant_PS_PGD_GENUS_lower_relab,Transplant_PS_PGD_GENUS_lower_prune)#keeps the taxa in Transplant_PS_PGD_GENUS_UL_relab that is contained in Transplant_PS_PGD_GENUS_lower_prune  

#####highest grade PGD 01 vs (2, 3)
#ddPCR read count
phyloseq_keep_ddPCR_PGD_GENUS_lower <-data.frame(sample_data(Transplant_PS_PGD_GENUS_lower)[,c("SampleID", "SubjID", "pgd_highest_48_72_v3_rename")])
Total_ddPCR_PGD_GENUS_lower <- merge(Total_ddPCR_GENUS_PGD_all, phyloseq_keep_ddPCR_PGD_GENUS_lower, by="SampleID") #keeping only the samples we care about (processed phyloseq)
read_count_KW(Total_ddPCR_PGD_GENUS_lower, "pgd_highest_48_72_v3_rename", 
              c("No PGD","Moderate","Severe"), c("palegreen3","lightsalmon","indianred3"), 
              xlabel_size=14, ylabel_size=13, axis_title_size=15,
              p_value="yes", output="ddPCR_PGD_GENUS_lower",ylabel="Bacterial concentration (log10 copies per uL)", count_variable="ddPCR_count")

#pepsin
transplant_PGD_Lower_pepsin<-read.csv("transplant_PDG_Lower_pepsin.csv")

Lower_pepsin_PGD_GENUS_lower<- merge(transplant_PGD_Lower_pepsin, phyloseq_keep_ddPCR_PGD_GENUS_lower, by="SubjID") #keeping only the samples we care about (processed phyloseq)
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
#multi_compare_KW(Transplant_PS_PGD_GENUS_lower_prune, 
#                 variable_to_compare="pgd_highest_48_72_v3_rename",
#                 control_group="No PGD",
#                 control_group_color="palegreen3",
#                 treatment_group=c("Moderate","Severe"), 
#                 treatment_group_color=c("lightsalmon","indianred3"),
#                 adj_p_cut_off=0.2,
#                 number_display=25,
#                 legend_onplot="yes",
#                 diff_method="Edger",
#                 output_name="multi_compare_Edger_PGD_01_v_2_3_GENUS",
#                 decontam="Contam_list_NC_BKG_compare_Lower_and_Upper_GENUS_PGD",
#                 width=200, 
#                 height=220,
#                 font_size_adjust_percent=90,
#                 taxa_genus_output="yes")
Edger_phylo_KW(Transplant_PS_PGD_GENUS_lower_prune, 
               variable_to_compare="pgd_highest_48_72_v3_rename",
               outcome_to_compare=c("No PGD", "Moderate"),
               outcome_to_compare_color=c("palegreen3", "lightsalmon"),
               FDR_cut_off=0.2, number_display=25,
               legend_onplot="yes",
               output_name="Edger_bronch_PGD_01_v_2_GENUS", 
               decontam="Contam_list_NC_BKG_compare_Lower_and_Upper_GENUS_PGD",
               taxa_genus_output="yes")

Edger_phylo_KW(Transplant_PS_PGD_GENUS_lower_prune, 
               variable_to_compare="pgd_highest_48_72_v3_rename",
               outcome_to_compare=c("No PGD", "Severe"),
               outcome_to_compare_color=c("palegreen3", "indianred3"),
               FDR_cut_off=0.2, number_display=25,
               legend_onplot="yes",
               output_name="Edger_bronch_PGD_01_v_3_GENUS", 
               decontam="Contam_list_NC_BKG_compare_Lower_and_Upper_GENUS_PGD",
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

Transplant_PS_PGD_GENUS_lower_DMM<-ps_join(Transplant_PS_PGD_GENUS_lower,DMM_cluster_KW_PGD_GENUS_lower_prune, .keep_all_taxa=T) #adding 

#bacterial read count
phyloseq_keep_ddPCR_PGD_GENUS_lower_DMM <-data.frame(sample_data(Transplant_PS_PGD_GENUS_lower_DMM)[,c("SampleID","SubjID", "Cluster_num", "pgd_highest_48_72_v3_rename")])
Total_ddPCR_PGD_GENUS_lower_DMM <- merge(Total_ddPCR_GENUS_PGD_all, phyloseq_keep_ddPCR_PGD_GENUS_lower_DMM, by="SampleID") #keeping only the samples we care about (processed phyloseq)
#ddPCR-separate by only cluster
read_count_KW(Total_ddPCR_PGD_GENUS_lower_DMM, "Cluster_num", 
              c("1", "2"), c("cornflowerblue", "hotpink1"), 
              xlabel_size=14, ylabel_size=13, axis_title_size=15,
              p_value="yes", output="ddPCR_GENUS_lower_clusteronly",ylabel="Bacterial concentration (log10 copies per uL)", count_variable="ddPCR_count")
#ddPCR-separate by only cluster and PGD
read_count_v2_KW(Total_ddPCR_PGD_GENUS_lower_DMM, 
                 sample_type_var_name="pgd_highest_48_72_v3_rename", sample_types=c("No PGD","Moderate","Severe"), sample_type_color=c("palegreen3","lightsalmon","indianred3"), 
                 sample_type2_var_name="Cluster_num", sample_types2=c("1", "2"), sample_type2_color=c("cornflowerblue", "hotpink1"), 
                 xlabel_size=14, ylabel_size=13, axis_title_size=15,
                 p_value="yes", p_value_select = "both",
                 output="ddPCR_GENUS_lower_cluster", ylabel="Bacterial concentration (log10 copies per uL)", count_variable="ddPCR_count")

#16S data
#separate by only cluster
####Figure 2A####
read_count_KW(Transplant_PS_PGD_GENUS_lower_DMM, "Cluster_num", 
              c("1", "2"), c("cornflowerblue", "hotpink1"), 
              xlabel_size=14, ylabel_size=13, axis_title_size=15,
              p_value="yes", output="read_count_GENUS_lower_clusteronly", count_variable="ddPCR_count")
#separate by only cluster and PGD
read_count_v2_KW(Transplant_PS_PGD_GENUS_lower_DMM, 
                 sample_type_var_name="pgd_highest_48_72_v3_rename", sample_types=c("No PGD","Moderate","Severe"), sample_type_color=c("palegreen3","lightsalmon","indianred3"), 
                 sample_type2_var_name="Cluster_num", sample_types2=c("1", "2"), sample_type2_color=c("cornflowerblue", "hotpink1"), 
                 xlabel_size=14, ylabel_size=13, axis_title_size=15, 
                 p_value="yes", p_value_select = "both",output="read_count_GENUS_lower_cluster")
####Figure 2B####
#separate by only cluster
alpha_diversity_KW(Transplant_PS_PGD_GENUS_lower_DMM, "Cluster_num", 
                   c("1", "2"), c("cornflowerblue", "hotpink1"), 
                   xlabel_size=14, ylabel_size=13, axis_title_size=15, 
                   p_value="yes", output="alpha_diversity_GENUS_lower_prune_clusteronly")
#separate by only cluster and PGD
alpha_diversity_v2_KW(Transplant_PS_PGD_GENUS_lower_DMM, 
                      sample_type_var_name="pgd_highest_48_72_v3_rename", sample_types=c("No PGD","Moderate","Severe"), sample_type_color=c("palegreen3","lightsalmon","indianred3"), 
                      sample_type2_var_name="Cluster_num", sample_types2=c("1", "2"), sample_type2_color=c("cornflowerblue", "hotpink1"), 
                      xlabel_size=14, ylabel_size=13, axis_title_size=15, 
                      p_value="yes", p_value_select = "both",output="alpha_diversity_GENUS_lower_cluster")

Transplant_PS_PGD_GENUS_lower_relab_DMM = transformSampleCounts(Transplant_PS_PGD_GENUS_lower_DMM,normalizeSample) ##relative abundance
####Figure 2C####
beta_diversity_KW(Transplant_PS_PGD_GENUS_lower_relab_DMM, "Cluster_num", c("1", "2"), 
            c("cornflowerblue", "hotpink1"), 
            p_value="yes", 
            output="beta_PGD_prune_DMM22")

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
Transplant_PS_PGD_GENUS_lower_cytokine = subset_samples(Transplant_PS_PGD_GENUS_lower, !is.na(Cytokine_Cluster))
Transplant_PS_PGD_GENUS_lower_prune_cytokine = subset_samples(Transplant_PS_PGD_GENUS_lower_prune, !is.na(Cytokine_Cluster))

####Figure 4B####                                                              
read_count_KW(Transplant_PS_PGD_GENUS_lower_cytokine, "Cytokine_Cluster", 
              c("Cytokine Cluster 1", "Cytokine Cluster 2"), c("orange", "blue2"), 
              xlabel_size=14, ylabel_size=13, axis_title_size=15,
              p_value="yes", output="read_count_GENUS_lower_cytokine_clusteronly", count_variable="ddPCR_count")
#separate by only cluster and PGD
read_count_v2_KW(Transplant_PS_PGD_GENUS_lower_cytokine, 
                 sample_type_var_name="pgd_highest_48_72_v3_rename", sample_types=c("No PGD","Moderate","Severe"), sample_type_color=c("palegreen3","lightsalmon","indianred3"), 
                 sample_type2_var_name="Cytokine_Cluster", sample_types2=c("Cytokine Cluster 1", "Cytokine Cluster 2"), sample_type2_color=c("orange", "blue2"), 
                 xlabel_size=14, ylabel_size=13, axis_title_size=15, 
                 p_value="yes", p_value_select = "both",output="read_count_GENUS_lower_cytokine_cluster")
####Figure 4C####
#separate by only cluster
alpha_diversity_KW(Transplant_PS_PGD_GENUS_lower_cytokine, "Cytokine_Cluster", 
                   c("Cytokine Cluster 1", "Cytokine Cluster 2"), c("orange", "blue2"), 
                   xlabel_size=14, ylabel_size=13, axis_title_size=15, 
                   p_value="yes", output="alpha_diversity_GENUS_lower_prune_cytokine_clusteronly")
#separate by cluster and PGD
alpha_diversity_v2_KW(Transplant_PS_PGD_GENUS_lower_cytokine, 
                      sample_type_var_name="pgd_highest_48_72_v3_rename", sample_types=c("No PGD","Moderate","Severe"), sample_type_color=c("palegreen3","lightsalmon","indianred3"), 
                      sample_type2_var_name="Cytokine_Cluster", sample_types2=c("Cytokine Cluster 1", "Cytokine Cluster 2"), sample_type2_color=c("orange", "blue2"), 
                      xlabel_size=14, ylabel_size=13, axis_title_size=15, 
                      p_value="yes", p_value_select = "both",output="alpha_diversity_GENUS_lower_cytokine_cluster")

Transplant_PS_PGD_GENUS_lower_relab_cytokine = subset_samples(Transplant_PS_PGD_GENUS_lower_relab, !is.na(Cytokine_Cluster))

####Figure 4D####
#beta diversity by only cluster
beta_diversity_KW(Transplant_PS_PGD_GENUS_lower_relab_cytokine, "Cytokine_Cluster", 
                  c("Cytokine Cluster 1", "Cytokine Cluster 2"), c("orange", "blue2"), 
                  p_value="yes", 
                  output="beta_diversity_GENUS_lower_cytokine_clusteronly")

#beta diversity by cluster and PGD
beta_diversity_KW(Transplant_PS_PGD_GENUS_lower_relab_cytokine, 
                  sample_type_var_name="pgd_highest_48_72_v3_rename", sample_types=c("No PGD","Moderate","Severe"), sample_type_color=c("palegreen3","lightsalmon","indianred3"), 
                  sample_type2_var_name="Cytokine_Cluster", sample_types2=c("Cytokine Cluster 1", "Cytokine Cluster 2"), sample_type2_color=c("orange", "blue2"), 
                  p_value="yes", 
                  output="beta_diversity_GENUS_lower_prune_cytokine_cluster")

beta_diversity_KW(Transplant_PS_PGD_GENUS_lower_relab_cytokine, 
                  sample_type_var_name="Cytokine_Cluster", sample_types=c("Cytokine Cluster 1", "Cytokine Cluster 2"), sample_type_color=c("orange", "blue2"), 
                  sample_type2_var_name="pgd_highest_48_72_v3_rename", sample_types2=c("No PGD","Moderate","Severe"), sample_type2_color=c("palegreen3","lightsalmon","indianred3"), 
                  p_value="yes", 
                  output="beta_diversity_GENUS_lower_prune_cytokine_cluster_v2")

#beta diversity by cluster and DMM
beta_diversity_KW(Transplant_PS_PGD_GENUS_lower_relab_cytokine, "Cytokine_Cluster", c("Cytokine Cluster 1", "Cytokine Cluster 2"), 
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
     
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##########################################################################TRANSCRIPTOME ANALYSIS##########################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
transplant_transcriptome_grp7_8_final<-read.csv(PGD_transcriptome_data_grp7_8.csv)

#pruning genes to ~11k
gene_list_keep<-Prune_w_Deseq_nonPS_KW(transplant_transcriptome_grp7_8_final, 
                                       sample_type_var_name="transcriptome_AG", 
                                       sample_types=c("Group_8", "Group_7"), 
                                       normalized_count=50 )
gene_list_keep<- rbind("transcriptome_AG",gene_list_keep)
transplant_transcriptome_grp7_8_final_prune<-merge(transplant_transcriptome_grp7_8_final,gene_list_keep, by.x="transcriptome", by.y="gene", sort=F)

Edger_nonPS_KW(input_table=transplant_transcriptome_grp7_8_final_prune, 
               sample_type_var_name="transcriptome_AG", 
               sample_types=c("Group_7", "Group_8"), 
               sample_type_color=c("palegreen3","indianred3"), 
               FDR_cut_off=0.1,
               graph_option="volcano",
               display_all_results_volcano="yes",
               abundance_size_volcano="no",
               output_name="TXP_study3_EdgeR_AG7__AG8_vol_0.1",
               legend_onplot="yes",
               output_format="pdf")

Edger_nonPS_KW(input_table=transplant_transcriptome_grp7_8_final_prune, 
               sample_type_var_name="transcriptome_AG", 
               sample_types=c("Group_7", "Group_8"), 
               sample_type_color=c("palegreen3","indianred3"), 
               FDR_cut_off=0.2,
               graph_option="volcano",
               display_all_results_volcano="yes",
               abundance_size_volcano="no",
               output_name="TXP_study3_EdgeR_AG7__AG8_vol_0.2",
               legend_onplot="yes",
               output_format="pdf")

#DMM group comparison
transplant_transcriptome_grpDMM_final<-read.csv(PGD_transcriptome_data_grpDMM.csv)

#pruning genes to ~11k
gene_list_keep<-Prune_w_Deseq_nonPS_KW(transplant_transcriptome_grpDMM_final, 
                                       sample_type_var_name="Cluster_number", 
                                       sample_types=c("Group_1", "Group_2"), 
                                       normalized_count=50 )
gene_list_keep<- rbind("Cluster_number",gene_list_keep)
transplant_transcriptome_grpDMM_final_prune<-merge(transplant_transcriptome_grpDMM_final,gene_list_keep, by.x="transcriptome", by.y="gene", sort=F)

Edger_nonPS_KW(input_table=transplant_transcriptome_grpDMM_final_prune, 
               sample_type_var_name="Cluster_number", 
               sample_types=c("Group_1", "Group_2"), 
               sample_type_color=c("cornflowerblue", "hotpink1"), 
               FDR_cut_off=0.1,
               label_only_volcano=c("TNF", "SERPINE1", "CSF1", "MPO", "MMP14", "CCL3", "HBEGF", "MMP1"),
               volcano_label_bold=c("TNF", "SERPINE1", "CSF1", "MPO", "MMP14", "CCL3", "HBEGF", "MMP1"),volcano_bold_size=4,volcano_bold_color="darkblue",
               display_all_results_volcano="yes",
               abundance_size_volcano="no",
               graph_option="volcano",
               output_name="TXP_EdgeR_DMM_PGD_vol_0.1",
               legend_onplot="yes",
               output_format="pdf")

Edger_nonPS_KW(input_table=transplant_transcriptome_grpDMM_final_prune, 
               sample_type_var_name="Cluster_number", 
               sample_types=c("Group_1", "Group_2"), 
               sample_type_color=c("cornflowerblue", "hotpink1"), 
               FDR_cut_off=0.2,
               label_only_volcano=c("TNF", "SERPINE1", "CSF1", "MPO", "MMP14", "CCL3", "HBEGF", "MMP1"),
               volcano_label_bold=c("TNF", "SERPINE1", "CSF1", "MPO", "MMP14", "CCL3", "HBEGF", "MMP1"),volcano_bold_size=4,volcano_bold_color="darkblue",
               display_all_results_volcano="yes",
               abundance_size_volcano="no",
               graph_option="volcano",
               output_name="TXP_EdgeR_DMM_PGD_vol_0.2",
               legend_onplot="yes",
               output_format="pdf")



pgd.cytokines <- read.csv("pgd.cytokines.csv", row.names = 1, header = T, check.names=F)
#Convert file to data matrix
pgd.cytokines <- as.matrix(pgd.cytokines)
#Log Transform File
pgd.cytokines_log<-log10(pgd.cytokines)
#Z-transform File
z_pgd.cytokines <- scale(pgd.cytokines)
### Add metadata for side color lab
pgd.cytokines.map <- read.csv("pgd.luminex.map.csv", header = T, check.names=F)
##Select the grouping based on first letter of sample ID (A=WT, B=Dys, C=LC, D=LC/Dys)
Cat_Label_code = pgd.cytokines.map$PGD
Colorvector <-Cat_Label_code
Colorvector <- replace(Colorvector, which (Colorvector == "0"), "green")
Colorvector <- replace(Colorvector, which (Colorvector == "2"), "orange")
Colorvector <- replace(Colorvector, which (Colorvector == "3"), "red")
##Select the grouping based on first letter of sample ID (A=WT, B=Dys, C=LC, D=LC/Dys)
Cat_Label_code = pgd.cytokines.map$DMM
Colorvector <-Cat_Label_code
Colorvector <- replace(Colorvector, which (Colorvector == "1"), "lightblue")
Colorvector <- replace(Colorvector, which (Colorvector == "2"), "pink")
Colorvector <- replace(Colorvector, which (Colorvector == "3"), "red")
##Add breaks to color panel
breaks = seq(0, max(pgd.cytokines), length.out=10000)
### define the colors within 4 zones
gradient1 = colorpanel( sum( breaks[-1]<=20 ), "#F8F8F8", "#FFD4CB" )
gradient2 = colorpanel( sum( breaks[-1]>20 & breaks[-1]<=100 ), "#FFD4CB", "#F77F70")
gradient3 = colorpanel( sum( breaks[-1]>100 & breaks[-1]<=1000 ), "#F77F70", "#D54046" )
gradient4 = colorpanel( sum( breaks[-1]>1000 ), "#D54046", "#AE123A" )
hm.colors = c(gradient1,gradient2, gradient3, gradient4)
# Define custom tick positions and labels
custom_ticks <- seq(0, 10000, length.out = 6)
custom_labels <- c("400", "800", "1200", "1600","2000", "2400")
##Plot Heat map, raw data AND Create Heatmap Object
pdf("PGD_Cytokines_Heatmap_And_PGD_Groups_key.pdf", height = 16, width = 16)
HeatmapObject <- heatmap.2(pgd.cytokines,
                           key=TRUE,
                           keysize=1.5,
                           key.xtickfun=function(min,max,n)seq(0, 10000, length.out=1000),
                           density.info = "none",
                           trace = "none",
                           dendrogram = "both",
                           distfun = function(x) dist(x, method= "manhattan"),
                           hclustfun = function(x) hclust(x, method="complete"),
                           margins=c(20,20),
                           breaks = breaks, col = hm.colors,
                           main = "PGD Cytokine Heatmap And PGD Groups",
                           ColSideColors=Colorvector
)
#Add custom axis labels manually
axis(3, at = custom_ticks, labels=custom_labels, las = 0)
dev.off()
##Plot Heat map, raw data AND Create Heatmap Object
pdf("PGD_Cytokines_Heatmap_And_PGD_Groups.pdf", height = 16, width = 16)
HeatmapObject <- heatmap.2(pgd.cytokines,
                           density.info = "none",
                           trace = "none",
                           dendrogram = "both",
                           distfun = function(x) dist(x, method= "manhattan"),
                           hclustfun = function(x) hclust(x, method="complete"),
                           margins=c(20,20),
                           col= colorRampPalette(brewer.pal(8, "Reds"))(25),
                           main = "PGD Cytokine Heatmap And PGD Groups",
                           ColSideColors=Colorvector
)
dev.off()