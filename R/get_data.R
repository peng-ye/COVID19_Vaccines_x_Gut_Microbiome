format_names <- function(x) {
  x <- gsub("_.*", "", x)
  x <- gsub("T0", "D0", x)
  x <- gsub("T2", "M1", x)
  return(x)
}

# Data
## Demographic and outcome data
phenotype.data <- read.table("metadata/samples_metadata_imm_272S_wide.txt", header = T, row.names = 1)
subjects.snv <- rownames(phenotype.data)[phenotype.data$Vac_Group=="SinoVac"]
subjects.bnt <- rownames(phenotype.data)[phenotype.data$Vac_Group=="BioNTech"]

## Microbial species relative abundance
species.data <- read.table("data/metaphlan3.merged.abundance.profile.species.272S.tsv", header = T, row.names = 1, check.names = F)
colnames(species.data) <- format_names(colnames(species.data))

### Generate a map from shortened names to full names
species_map <- data.frame(rownames(species.data))
rownames(species_map) <- gsub(".*s__", "", rownames(species.data))
species_map$phylum <- gsub("\\|c__.*", "", gsub(".*p__","", species_map[, 1]))

### Shorten species names
rownames(species.data) <- rownames(species_map)

### One-month data
species.data.m1 <- data.frame(t(species.data[,paste0(rownames(phenotype.data)[!is.na(phenotype.data$diff_M1D0_observed)], "-M1")]))
rownames(species.data.m1) <- gsub("-M1", "", rownames(species.data.m1))
species.data.m1.bnt <- na.omit(species.data.m1[subjects.bnt, ])
species.data.m1.snv <- na.omit(species.data.m1[subjects.snv, ])

### Baseline data
species.data.d0 <- data.frame(t(species.data[,paste0(rownames(phenotype.data), "-D0")]))
rownames(species.data.d0) <- gsub("-D0", "", rownames(species.data.d0))
#### All samples for each vaccine group
species.data.d0.snv.all <- species.data.d0[subjects.snv, ]
species.data.d0.bnt.all <- species.data.d0[subjects.bnt, ]
species.data.d0.snv.all <- species.data.d0.snv.all[, colSums(species.data.d0.snv.all) > 0]
species.data.d0.bnt.all <- species.data.d0.bnt.all[, colSums(species.data.d0.bnt.all) > 0]
#### Keep only those with one-month data
species.data.d0.bnt <- species.data.d0[rownames(species.data.m1.bnt), ] 
species.data.d0.snv <- species.data.d0[rownames(species.data.m1.snv), ]

## Microbial pathway data
pathway_map <- read.table("data/pathway_name_map.txt", sep="\t", header = T); rownames(pathway_map) <- pathway_map$pathway
pathway.data <- read.table("data/humann3_pathabundance_relab_joined_unstratified.272S-symbol_rm.tsv", sep = "\t", row.names = 1, header = T, check.names = F)
rownames(pathway.data) <- pathway_map[rownames(pathway.data), "pathway_rename"]

### Baseline data
pathway.data.d0 <- data.frame(t(pathway.data[,paste0(rownames(phenotype.data), "-D0")]))
rownames(pathway.data.d0) <- gsub("-D0", "", rownames(pathway.data.d0))

#### All samples for each vaccine group
pathway.data.d0.snv.all <- pathway.data.d0[subjects.snv, ]
pathway.data.d0.bnt.all <- pathway.data.d0[subjects.bnt, ]


## LEfSe results 
### differentially abundant species: binary sVNT
species.lefse.result.snv <- na.omit(read.table("tables/lefse/tax_s_lefse_D0_sinovac_sVNT_60percent_binary_group_w.lefse.out.tsv", sep = "\t")); colnames(species.lefse.result.snv) <- c("species", "highest", "group", "LDA_score", "p.value")
species.lefse.result.bnt <- na.omit(read.table("tables/lefse/tax_s_lefse_D0_biontech_sVNT_binary_group_w.lefse.out.tsv", sep = "\t")); colnames(species.lefse.result.bnt) <- c("species", "highest", "group", "LDA_score", "p.value")

### differentially abundant pathways: binary sVNT
pathway.lefse.result.snv <- na.omit(read.table("tables/lefse/pathabun_lefse_d0_snv_svnt_60percent_abun.tsv", sep = "\t", header=T))
pathway.lefse.result.bnt <- na.omit(read.table("tables/lefse/pathabun_lefse_d0_bnt_svnt_abun.tsv", sep = "\t", header=T))


