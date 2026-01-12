# Plasmin test, testing our plasmin against freshly made up EJ plasmin

library(vascr)
library(tidyverse)

plasmintestdata <- vascr_import("ECIS",
                                raw = "Drugscreen_panel_II_badplasmineffect/ECIS_251207_MFT_1_CG_plasmintestplusextra.abp",
                                model = "Drugscreen_panel_II_badplasmineffect/ECIS_251207_MFT_1_CG_plasmintestplusextra_RbA.csv", experiment = "exp1"
)

plasmintestkey <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, #triplicate treatments
  1, "F", "1 2 3",  "EJ's plasmin. Make up and frozen (one freeze thaw)",
  2, "F", "4 5 6", "15uL of our plasmin. Made up and frozen (one freeze thaw)", 
  3, "G", "1 2 3", "Our old plasmin. Has been through two subaliquoting since initial freeze (three freeze thaws)",
  4, "G", "4 5 6", "Plasmin vehicle (everything is 0.5% water)", 
  5, "H", "4 5 6", "Added all the leftover plasmins randomly")




plasmintestlabeled <- vascr:::vascr_apply_map(plasmintestdata, plasmintestkey)

#plasmin plot
plasmintestlabeled %>%
  vascr_subset(unit = "Rb") %>%
  vascr_zero_time(71.5658) %>%
  vascr_normalise(-2, divide = TRUE) %>% vascr_subset(sampleid=c(1:5), time=c(-2,30)) %>%  vascr_resample_time(500) %>% vascr_summarise(level = "experiment") %>% 
  vascr_plot_line()


