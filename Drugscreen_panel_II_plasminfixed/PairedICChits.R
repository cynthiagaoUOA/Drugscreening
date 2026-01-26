library(vascr)
library(tidyverse)


pairedhits <- vascr_import("ECIS",
                          raw = "Drugscreen_panel_II_plasminfixed/ECIS_260115_MFT_1_CG_drugscreenhits_janpairedICC_1.abp",
                          model = "Drugscreen_panel_II_plasminfixed/ECIS_260115_MFT_1_CG_drugscreenhits_janpairedICC_1_RbA.csv", experiment = "exp1"
)


pairedhitskey <- tribble( ~SampleID, ~Row, ~Column, ~Sample, #triplicate treatments
  1, "A", "2 3",  "Low astaxanthin vehicle", 
  1, "B", "2", "Low astaxanthin vehicle",
  2, "A", "4 5 6", "Low astaxanthin plasmin", 
  3, "B", "3", "High astaxanthin vehicle", 
  3, "C", "2 3", "High astaxanthin vehicle",
  4,  "B", "4 5 6", "High astaxanthin plasmin",
  
  5, "D", "1 2 3", "Low valproic acid vehicle",
  6, "D", "4 5 6", "Low valproic acid plasmin",
  7, "E", "2 3", "High valproic acid vehicle", 
  7, "H", "2", "High valproic acid vehicle",
  8, "F", "4 5 6", "High valproic acid plasmin",
  
  9, "F", "1 2 3", "Low edaravone vehicle", 
  10, "E", "4 5 6", "Low edaravone plasmin",
  11, "G", "1 2 3", "High edaravone vehicle", 
  12, "F", "4 5 6", "High edaravone plasmin",

  20, "G", "4 5 6", "vehicle",
  21, "H", "4 5 6", "plasmin")



pairedhitslabeled <- vascr:::vascr_apply_map(pairedhits, pairedhitskey)

pairedhitsplot <- pairedhitslabeled %>%
  vascr_subset(unit = "Rb") %>%
  vascr_zero_time(64.4876) %>%
  vascr_normalise(-2, divide = TRUE) 

pairedhitsplot  %>%  
  vascr_subset(sampleid = c(1:12, 20, 21), time=c(-4,24)) %>% 
  vascr_summarise(level = "experiment") %>% 
  vascr_plot_line() 


# asta
asta<- pairedhitsplot  %>%  
  vascr_subset(sampleid = c(1:4, 20, 21), time=c(-4,24)) %>% 
  vascr_summarise(level = "experiment") %>% 
  vascr_plot_line() 

asta

# VPA
VPA<- pairedhitsplot  %>%  
  vascr_subset(sampleid = c(5:8, 20, 21), time=c(-4,24)) %>% 
  vascr_summarise(level = "experiment") %>% 
  vascr_plot_line() 

# edav
edav<- pairedhitsplot  %>%  
  vascr_subset(sampleid = c(9:12, 20, 21), time=c(-4,24)) %>% 
  vascr_summarise(level = "experiment") %>% 
  vascr_plot_line() 


library(patchwork)

asta + VPA / edav
