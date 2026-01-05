# two experiments, not including plasmin alone tho :((


setwd("C:/Users/cgao801/OneDrive - The University of Auckland/1 PHD/2509 B Drugscreen")

library(vascr)
library(tidyverse)
library(ggplot2)

dmso <- vascr_import("ECIS",
                               raw = "ECIS_250917_MFT_1_CG_dmso_minitest.abp",
                               model = "ECIS_250917_MFT_1_CG_dmso_minitest_RbA.csv", experiment = "exp1"
)


dmsomap <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, #triplicate treatments
  1, "A", "7 8 9",  "0.1% DMSO vehicle",
  2, "B", "7 8 9", "0.1% DMSO plasmin",
  3, "C", "7 8 9", "0.01% DMSO vehicle",
  4, "D", "7 8 9", "0.01% DMSO plasmin",
  5, "C", "10 11 12", "0.001% DMSO vehicle",
  6, "D", "10 11 12", "0.001% DMSO plasmin",
  7, "A", "10 11", "vehicle", 7, "B", "10", "vehicle")

dmsolabeled <- vascr:::vascr_apply_map(dmso, dmsomap)

dmsodata <- dmsolabeled %>%
  vascr_subset(unit = "Rb", sampleid = c(1:7)) %>%
  vascr_zero_time(47.48) %>%
  vascr_resample_time(500) %>%
  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(time = c(-2, 20))

dmsodata  %>%  vascr_subset(sampleid = c(1:7)) %>% vascr_summarise(level = "experiment") %>% ##ggplotly all
  vascr_plot_line()

##### DMSO in vehicle from previous experiment
setwd("C:/Users/cgao801/OneDrive - The University of Auckland/1 PHD/2507 C edav licl fasudil plasmin/Runs 3-")
drugcombosept4 <- vascr_import("ECIS",
                               raw = "ECIS_250908_MFT_1_CG_septdrugs_1.abp",
                               model = "ECIS_250908_MFT_1_CG_septdrugs_1_RbA.csv", experiment = "exp4"
)


drugskeysept4 <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, #triplicate treatments
  6, "H", "10 11 12", "0.001% DMSO plasmin",
  26, "H", "7 8 9", "plasmin no DMSO",
  27, "H", "4 5 6", "vehicle + 0.001 DMSO",
  28, "A", "4 5 6", "vehicle no DMSO"
  5, "H", "4 5 6", "0.001% DMSO vehicle",
  6, "H", "10 11 12", "0.001% DMSO plasmin",
  7, "A", "4 5 6", "vehicle")
  
drugdmsolabeled <- vascr:::vascr_apply_map(drugskeysept4, drugcombosept4)

drugdmsolabeledzeroed <- drugdmsolabeled %>%
  vascr_subset(unit = "Rb", sampleid = c(5:7)) %>%
  vascr_zero_time(73.37) %>%
  vascr_resample_time(500) %>%
  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(time = c(-2, 20))

combine=vascr_combine(dmsodata, drugdmsolabeledzeroed)



#### plot combined

combine %>%  vascr_subset(sampleid = c(5:7)) %>% vascr_summarise(level = "experiment") %>% ##ggplotly all
  vascr_plot_line()
