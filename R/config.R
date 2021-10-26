# set config profile
#pair = F
pair = T
if(pair == T){
  #sample_varname <- "id"
  sample_varname <- "sample_id"
  #time_varname <- "Stage"
  time_varname <- "TimePoint"
  pairID_varname <- "ID"
  group_varname <- "Vac_Group"

  #time_name   <- c("Before", "After")
  time_name   <- c("D0", "M1")
  input_micro <- "inst/extdata/abundance.profile"
  input_metadata <- "inst/extdata/phenotype.csv"

  # defalut parameter
  ProjectID   <- "c9_vaccine_gut_microbiota"
  time_colour <- c("#FB8072","#BEBADA")
  outputDir <- "./c9_vaccine_gut_microbiota_resutls"

}else{
  group_name <- c("BioNTech", "SinoVac")
  time_colour <- c("#FB8072","#BEBADA")
  group_varname <- "Vac_Group"
}

mytheme <- theme_bw(base_size = 12) +
  theme(plot.title = element_text(size = 10,color = "black", face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10, color = "black",face = "bold"),
        axis.text = element_text(size = 9, color = "black"), axis.ticks.length = unit(-0.05, "in"),
        axis.text.y = element_text(margin = unit(c(0.3,0.3, 0.3, 0.3), "cm"), size = 9),
        axis.text.x = element_text(margin = unit(c(0.3,0.3, 0.3, 0.3), "cm")),
        text = element_text(size = 8,color = "black"),
        strip.text = element_text(size = 9, color = "black", face = "bold"),
        panel.grid = element_blank())
