#!/usr/bin/env Rscript
group_color_values <- c("BioNTech"="#59b4e5",
                        "SinoVac"="#d56128")

timepoint_color_values <- c("D0"="#009E73", "M1"="#CC79A7")

gt_color_values <- c("Baseline & BioNTech"="#CC79A7",
                     "Baseline & SinoVac"="#0072B2",
                     "One Month & BioNTech"="#E69F00",
                     "One Month & SinoVac"="#009E73")

gt_color_values_v2 <- c("Baseline & BioNTech"="#59b4e5",
                        "Baseline & SinoVac"="#d56128",
                        "One Month & BioNTech"="#59b4e5",
                        "One Month & SinoVac"="#d56128")

gt2_color_values <- c("D0+sVNT>=20%"="#F0E442",
                    "D0+sVNT<20%"="#D55E00",
                    "M1+sVNT>=20%"="#56B4E9",
                    "M1+sVNT<20"="#000000")

gt3_color_values <- c(
"Baseline & BioNTech & Low"="#7fc97f",
"Baseline & BioNTech & High"="#beaed4",
"Baseline & Sinovac & Low"="#fdc086",
"Baseline & Sinovac & High"="#ffff99",
"One Month & BioNTech & Low"="#386cb0",
"One Month & BioNTech & High"="#f0027f",
"One Month & Sinovac & Low"="#bf5b17",
"One Month & Sinovac & High"="#666666")

gt4_color_values <- c(
"BioNTech & Low"="#CC79A7",
"BioNTech & High"="#E69F00",
"Sinovac & Low"="#0072B2",
"Sinovac & High"="#009E73")


timepoint_labels <- c("Baseline", "One Month")
names(timepoint_labels) <- c("D0", "M1")

gt_labels <- c("Baseline & BioNTech", "Baseline & SinoVac",
               "One Month & BioNTech", "One Month & SinoVac")
names(gt_labels) <- c("D0+BioNTech", "D0+SinoVac", "M1+BioNTech", "M1+SinoVac")

probiotic_labels <- c("Probiotic not use", "Probiotic use")
names(probiotic_labels) <- c("0", "1")

antibiotic_labels <- c("Antibiotic not use", "Antibiotic use")
names(antibiotic_labels) <- c("0", "1")

#000000 black
#E69F00 orange
#56B4E9 sky blue
#009E73 bluish green
#F0E442 yellow
#0072B2 blue
#D55E00 vermilion
#CC79A7 reddish purple

species_color_values <- c("Bacteroides plebeius"="#ffff99",
                          "Bacteroides stercoris"="#1f78b4",
                          "Bacteroides uniformis"="#b2df8a",
                          "Bacteroides vulgatus"="#33a02c",
                          "Bifidobacterium adolescentis"="#fb9a99",
                          "Blautia wexlerae"="#e31a1c",
                          "Eubacterium rectale"="#fdbf6f",
                          "Faecalibacterium prausnitzii"="#ff7f00",
                          "Prevotella copri"="#cab2d6",
                          "Roseburia faecis"="#6a3d9a",
                          "Others"="#a6cee3")


phylum_color_values <- c("Firmicutes"="#E69F00",
                         "Bacteroidetes"="#009E73",
                         "Actinobacteria"="#0072B2",
                         "Proteobacteria"="#D55E00",
                         "Verrucomicrobia"="#F0E442",
                         "Others"="#CC79A7")
