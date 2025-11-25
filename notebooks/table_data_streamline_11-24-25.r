# Attempt to precompute table for app

# Species-based clusters
df_label <- read.csv(
  "/Users/gallegosda/Library/CloudStorage/OneDrive-NationalInstitutesofHealth/11-18-25_kzfp_Shiny/kzfpClusterShinyApp-main/data/df_wide.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

df2 <- read.csv(
  "/Users/gallegosda/Library/CloudStorage/OneDrive-NationalInstitutesofHealth/11-18-25_kzfp_Shiny/kzfpClusterShinyApp-main/data/41586_2017_BFnature21683_MOESM103_ESM.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

df_pairs <- read.csv(
  "/Users/gallegosda/Library/CloudStorage/OneDrive-NationalInstitutesofHealth/11-18-25_kzfp_Shiny/kzfpClusterShinyApp-main/data/gene_cluster_pairs.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

df_gnomAD <- read.csv(
  "/Users/gallegosda/Library/CloudStorage/OneDrive-NationalInstitutesofHealth/11-18-25_kzfp_Shiny/kzfpClusterShinyApp-main/data/gnomAD_pli_oe_KZFP_genes.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# Pseudocode
# Compute the total number of species with a cluster matching a given gene
df_table <- df_gnomAD %>%
  outer_join(
    df_pairs %>% select(GeneLabel = Label, `Cluster #`),
    by = c("gene" = "Label"),
    relationship = "many-to-many"
  )

names(df_pairs)[names(df_pairs) == "Label"] <- "gene"

# Make new table by merging on "gene"
df_table <- merge(df_gnomAD, df_pairs, by = "gene", all = TRUE)

# drop NA
df_clean <- na.omit(df_table)
names(df_clean)[names(df_clean) == "Gene ID"] <- "gene_id"


# Collapse Cluster # column into a list within a single column
library(dplyr)

df_collapsed <- df_clean %>%
  group_by(gene, gene_id, pLI, oe) %>%
  summarise(
    Cluster = list(`Cluster #`),
    .groups = "drop"
  )

# Make a new column that tells the number of species with "TRUE" for a cluster associated with a given gene

# NOTE: This has failed ... will need to revisit

# PSUEDOCODE


# Test if logic works



gene_row_int <- 320 #ZNF777
print("Gene:")
print(df_collapsed[gene_row_int, "gene"])

clusterVector <- df_collapsed$Cluster[[gene_row_int]]

species_row_int <- 114
print(df_label[species_row_int, "Species"])

for (element in clusterVector) {
  print(element)
  elementString <- as.character(element)
  print(elementString) 
  print(df_label[species_row_int, elementString])
}



df_label[species_row_int, cluster]
print(df_label[species_row_int, cluster])

print("Gene:")
print(df_collapsed[gene_row_int, "gene"])
print("Evaluating species:")
print(df_label[species_row_int, "Species"])
print("Cluster: ")
print(cluster)
gene_count = gene_count + 1
print("Number of species evaluated:")
print(species_row_int)
print("gene_count:")
print(gene_count)
print("")

# create new empty column called num_species_w_cluster_associated_with_gene
df_collapsed$num_species_w_cluster_associated_with_gene <- NA_integer_     # integer NA

# for a given gene in df_collapsed
for (gene_row_int in 1:nrow(df_collapsed)) {
  # Reset gene count for each gene
  gene_count = 0
  #   for a given species in df_label:

  for (species_row_int in 1:nrow(df_label)){
    clustInSpecies <- FALSE
    #     for cluster in Cluster list in df_collapsed:
    for (clusterVector in df_collapsed$Cluster[[gene_row_int]]) {
      for (element in clusterVector) {
        # print(element)
        elementString <- as.character(element)
        # print(elementString) 
        # print(df_label[species_row_int, elementString])
        
        if (df_label[species_row_int, elementString] == TRUE) {
          print("Gene:")
          print(df_collapsed[gene_row_int, "gene"])
          print("Evaluating species:")
          print(df_label[species_row_int, "Species"])
          print("Cluster: ")
          print(elementString)
          # gene_count = gene_count + 1
          print("Number of species evaluated:")
          print(species_row_int)
          print("gene_count:")
          print(gene_count)
          print("")
          # break
          clustInSpecies = TRUE
        }
      }
    }
    if (clustInSpecies == TRUE) {
      gene_count = gene_count + 1
    }
  }
  print(df_collapsed[gene_row_int, "gene"])
  print(gene_count)
  df_collapsed[gene_row_int, "num_species_w_cluster_associated_with_gene"] = gene_count
}

# Make df_collapsed into a df where vector column is a long string separated by commas
df_collapsed_stringified <- df_collapsed
df_collapsed_stringified$Cluster_str <- sapply(df_collapsed_stringified$Cluster, function(x) paste(x, collapse = ","))
df_collapsed_stringified$Cluster <- NULL


df$mycol_string <- sapply(df$mycol, function(x) paste(x, collapse = ","))




# Save the dataframe as a CSV file
# "my_data.csv" is the desired filename and path
# row.names = FALSE prevents R from writing the row numbers as a column in the CSV



write.csv(df_collapsed_stringified, "df_collapsed_stringified.csv", row.names = FALSE)

#         increment num_species_w_cluster_associated_with_gene value for given gene by 1



# Save the matching cluter-gene pairs as a dict?

# Output the info as a table





# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------


  
  
  
