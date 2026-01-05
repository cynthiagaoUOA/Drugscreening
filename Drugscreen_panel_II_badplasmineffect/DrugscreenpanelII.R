# Drug panel II

# Run 1

library(vascr)
library(tidyverse)
library(ggplot2)

setwd("C:/Users/cgao801/OneDrive - The University of Auckland/1 PHD/2511A Drugscreen panel II")

newpanel1 <- vascr_import("ECIS",
                               raw = "ECIS_251111_MFT_1_CG_drugscreenII_1.abp",
                               model = "ECIS_251111_MFT_1_CG_drugscreenII_1_RbA.csv", experiment = "exp1"
)

newpanel1key <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, #triplicate treatments
  1, "A", "1 2 3",  "High marimastat vehicle",
  2, "A", "4 5 6", "High marimastat plasmin", 
  3, "A", "7 8 9", "Low marimastat vehicle",
  4, "A", "10 11 12", "Low marimastat plasmin", 
  
  5, "B", "1 2 3", "High valproic acid vehicle", 
  6, "B", "4 5 6", "High valproic acid plasmin", 
  7, "B", " 9", "Low valproic acid vehicle",  7, "H", "10", "Low valproic acid vehicle", 
  #B8 bad well, moved. #B7 looks weird as well, but post treatment. Have taken this out
  8, "B", "10 11 12", "Low valproic acid plasmin", 
  
  9, "C", "1 2 3", "High insulin vehicle", 
  10, "C", "4 6", "High insulin plasmin", # C5 was a bad well but forgot to move :/
  11, "C", "7 9", "Low insulin vehicle",   11, "H", "11", "Low insulin vehicle",  # C8 bad well moved
  12, "C", "11 12", "Low insulin plasmin",   12, "H", "12", "Low insulin plasmin", # C10 bad well moved to H12, but H12 also an outlier??
  
  13, "D", "1 2 3", "High edaravone2 vehicle", 
  14, "D", "4 5 6", "High edaravone2 plasmin",
  15, "D", "7 8 9", "Low edaravone2 vehicle", 
  16, "D", "10 11 12", "Low edaravone2 plasmin", 
  
  17, "E", "1 2 3", "High SB29057 vehicle",
  18, "E", "4 5 6", "High SB29057 plasmin", 
  19, "E", "7 8 9", "Low SB29057 vehicle",
  20, "E", "10 11 12", "Low SB29057 plasmin", 
  
  21, "F", "1 2 3", "High Astaxanthin vehicle",
  22, "F", "4 5 6", "High Astaxanthin plasmin", 
  23, "F", "7 8 9", "Low Astaxanthin vehicle",
  24, "F", "10 11 12", "Low Astaxanthin plasmin", 

  25, "G", "1 2 3", "High Butylphthalide vehicle",
  26, "G", "4 5 6", "High Butylphthalide plasmin", 
  27, "G", "7 8 9", "Low Butylphthalide vehicle",
  28, "G", "10 11 12", "Low Butylphthalide plasmin", 
  
  29, "H", "4 5 6", "vehicle alone", 
  30, "H", "7 8 9", "plasmin alone")


newpanel1labeled <- vascr:::vascr_apply_map(newpanel1, newpanel1key)

newpanel1plot <- newpanel1labeled %>%
  vascr_subset(unit = "Rb") %>%
  vascr_zero_time(64.297) %>%
  vascr_normalise(-2, divide = TRUE) 




newpanel2 <- vascr_import("ECIS",
                          raw = "ECIS_251121_MFT_CG_drugscreenII_3.abp",
                          model = "ECIS_251121_MFT_CG_drugscreenII_3_RbA.csv", experiment = "exp2")

newpanel2key <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, #triplicate treatments
  
  5, "G", "10 11 12", "High valproic acid vehicle", 
  6, "G", "7 8 9", "High valproic acid plasmin", 
  7, "H", "10 11", "Low valproic acid vehicle",  #meant to be in H12 but think this is the one I forgot
  8, "H", "7 8 9", "Low valproic acid plasmin", 
  
  9, "C", "5 6 7", "High insulin vehicle", 
  10, "B", "1 2", "High insulin plasmin",   10, "E", "2", "High insulin plasmin", 
  11, "C", "8 9", "Low insulin vehicle",      11, "D", "8", "Low insulin vehicle",    
  12, "D", "5 6 7", "Low insulin plasmin",   
  
  13, "B", "10 11 12", "High edaravone2 vehicle", 
  14, "A", "10 11 12", "High edaravone2 plasmin",
  15, "D", "9 10 11", "Low edaravone2 vehicle", 
  16, "C", "10 11", "Low edaravone2 plasmin",   16, "G", "12", "Low edaravone2 plasmin", 
  
  31, "A", "4 5 6", "High licl vehicle",
  32, "A", "1 2 3", "High licl plasmin", 
  33, "B", "5 6 8", "Low licl vehicle",
  34, "A", "7 8 9", "Low licl plasmin",     

  21, "E", "10 11 12", "High Astaxanthin vehicle",
  22, "E", "7 8 9", "High Astaxanthin plasmin", 
  24, "F", "10 11 12", "Low Astaxanthin plasmin", # convinced that 23 and 24 wells were swapped, based on figure. 
  23, "F", "7 8 9", "Low Astaxanthin vehicle", 
  
  29, "H", "4 5 6", "vehicle alone",   29, "G", "6", "vehicle alone", 
  30, "E", "5 6", "plasmin alone",   30, "F", "6", "plasmin alone")

newpanel2labeled <- vascr:::vascr_apply_map(newpanel2, newpanel2key)

newpanel2plot <- newpanel2labeled %>%
  vascr_zero_time(75.225) %>%
  vascr_normalise(-2, divide = TRUE)
  
newpanelcombined<- vascr_combine(newpanel1plot, newpanel2plot) %>% 
  vascr_subset(unit = "Rb") %>% 
  vascr_resample_time(500) 


newpanel3<- vascr_import("ECIS",
                         raw = "ECIS_251130_MFT_1CG_drugscreenII_4.abp",
                         model = "ECIS_251130_MFT_1CG_drugscreenII_4_RbA.csv", experiment = "exp3")



newpanel3key <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, #triplicate treatments
  1, "F", "1 2 3",  "High marimastat vehicle",
  2, "F", "4 5 6", "High marimastat plasmin", 
  3, "F", "7 8 9", "Low marimastat vehicle",
  4, "F", "10 11 12", "Low marimastat plasmin", 
  
  5, "B", "1 2 3", "High valproic acid vehicle", 
  6, "B", "4 5 6", "High valproic acid plasmin", 
  7, "B", "7 8 9", "Low valproic acid vehicle",
  8, "B", "10 12", "Low valproic acid plasmin", #weird high outlier B11
  

  13, "A", "2 3", "High edaravone2 vehicle", 13, "H", "1", "High edaravone2 vehicle", #bad well A1, moved. 
  14, "A", "4 5 6", "High edaravone2 plasmin", #double upped on well 5, so don't trust 5 or 6.........
  15, "A", "7 8 9", "Low edaravone2 vehicle", 
  16, "A", "10 11 12", "Low edaravone2 plasmin", 
  
  17, "G", "1 2 3", "High SB29057 vehicle",
  18, "G", "4 5 6", "High SB29057 plasmin", 
  19, "G", "7 8 9", "Low SB29057 vehicle",
  20, "G", "10 11 12", "Low SB29057 plasmin", 
  
  21, "E", "1 2 3", "High Astaxanthin vehicle",
  22, "E", "4 5 6", "High Astaxanthin plasmin", 
  23, "E", "7 8 9", "Low Astaxanthin vehicle",
  24, "E", "10 11 12", "Low Astaxanthin plasmin", 
  
  25, "D", "1 2 3", "High Butylphthalide vehicle",
  26, "D", "4 5 6", "High Butylphthalide plasmin", 
  27, "D", "7 8 9", "Low Butylphthalide vehicle",
  28, "D", "10 11 12", "Low Butylphthalide plasmin", 
  
  30, "H", "4 5 6", "plasmin alone", 
  29, "H", "7 8 9", "vehicle alone",
  
  31, "C", "1 2 3", "High licl vehicle",
  32, "C", "4 5 6", "High licl plasmin", 
  33, "C", "7 8 9", "Low licl vehicle",
  34, "C", "10 11 12", "Low licl plasmin")


newpanel3labeled <- vascr:::vascr_apply_map(newpanel3, newpanel3key)

newpanel3plot <- newpanel3labeled %>%
  vascr_zero_time(63.693) %>%
  vascr_normalise(-2, divide = TRUE)


newpanelcombined<- vascr_combine(newpanel1plot, newpanel2plot, newpanel3plot) %>% 
  vascr_subset(unit = "Rb") %>% 
  vascr_resample_time(500) 




######## combined plots

#valproic acid
newpanelcombined  %>%  vascr_subset(sampleid = c(5:8, 29,30), time=c(-4,48)) %>% vascr_summarise(level = "summary") %>% 
  vascr_plot_line() 

newpanelcombined  %>%  vascr_subset(sampleid = c(5:8, 29,30), time=c(-4,18), experiment=3) %>% vascr_summarise(level = "well") %>% 
  vascr_plot_line()

#new edaravone
newpanelcombined  %>%  vascr_subset(sampleid = c(13:16, 29,30), time=c(-4,20)) %>% vascr_summarise(level = "summary") %>% 
  vascr_plot_line()
newpanelcombined  %>%  vascr_subset(sampleid = c(13:16, 29,30), time=c(-4,20)) %>% vascr_summarise(level = "well") %>% 
  vascr_plot_line()  +facet_wrap(~Experiment)

#insulin
newpanelcombined  %>%  vascr_subset(sampleid = c(9:12, 29,30), time=c(-4,20)) %>% vascr_summarise(level = "summary") %>% 
  vascr_plot_line()

#astaxanthin
newpanelcombined  %>%  vascr_subset(sampleid = c(21,22, 29,30), time=c(-4,20)) %>% vascr_summarise(level = "summary") %>% 
  vascr_plot_line()
newpanelcombined  %>%  vascr_subset(sampleid = c(21:24, 29,30), time=c(-4,20)) %>% vascr_summarise(level = "summary") %>% ##ggplotly all
  vascr_plot_line() 

newpanelcombined  %>%  vascr_subset(sampleid = c(29,30), time=c(-4,20)) %>% vascr_summarise(level = "summary") %>% ##ggplotly all
  vascr_plot_line() 

#licl
newpanel2labeled %>%
  vascr_zero_time(72.225) %>% 
  vascr_normalise(-2, divide = TRUE)  %>%  
  vascr_resample_time(500) %>% vascr_subset(unit="Rb", sampleid = c(31:34, 29,30), time=c(-4,48)) %>% vascr_summarise(level = "experiment") %>% ##ggplotly all
  vascr_plot_line()


################ run one


#valproic acid

newpanelcombined  %>%  vascr_subset(sampleid = c(5:8, 29,30), time=c(-4,20)) %>% vascr_summarise(level = "experiment") %>% ##ggplotly all
  vascr_plot_line() + facet_wrap(~Experiment)


#marimastat - not expected to work
newpanel1plot  %>%  vascr_subset(sampleid = c(1:4, 29,30), time=c(-4,24)) %>% vascr_summarise(level = "experiment") %>% ##ggplotly all
  vascr_plot_line()

# insulin
newpanel1plot  %>%  vascr_subset(sampleid = c(10,12, 29,30), time=c(-4,24)) %>% vascr_exclude(well="H12") %>% vascr_summarise(level = "experiment") %>% ##ggplotly all
  vascr_plot_line()

# edaravone. 
newpanel1plot  %>%  vascr_subset(sampleid = c(13,14, 29,30), time=c(-4,48)) %>% vascr_summarise(level = "experiment") %>% ##ggplotly all
  vascr_plot_line()

# Sb - complement
newpanel1plot  %>%  vascr_subset(sampleid = c(17:20, 29,30), time=c(-4,24)) %>% vascr_summarise(level = "experiment") %>% ##ggplotly all
  vascr_plot_line()

# Asthaxanthin
newpanel1plot  %>%  vascr_subset(sampleid = c(21:24, 29,30), time=c(-4,24)) %>% vascr_summarise(level = "experiment") %>% ##ggplotly all
  vascr_plot_line()

newpanel1plot  %>%  vascr_subset(sampleid = c(22,24, 29,30), time=c(-4,48)) %>% vascr_summarise(level = "experiment") %>% ##ggplotly all
  vascr_plot_line()


# butylphthalide
newpanel1plot  %>%  vascr_subset(sampleid = c(25:28, 29,30), time=c(-4,24)) %>% vascr_summarise(level = "experiment") %>% ##ggplotly all
  vascr_plot_line()


newpanelcombined  %>%  vascr_subset(sampleid = c(29,30), time=c(-4,20)) %>% vascr_summarise(level = "experiment") %>% ##ggplotly all
  vascr_plot_line() 


### testing if plasmin issues -------------------------------------

plasmintestdata <- vascr_import("ECIS",
                          raw = "ECIS_251207_MFT_1_CG_plasmintestplusextra.abp",
                          model = "ECIS_251207_MFT_1_CG_plasmintestplusextra_RbA.csv", experiment = "exp1"
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

#reference code
newpanel2labeled %>%
  vascr_zero_time(72.225) %>% 
  vascr_normalise(-2, divide = TRUE)  %>%  
  vascr_resample_time(500) %>% vascr_subset(unit="Rb", sampleid = c(31:34, 29,30), time=c(-4,48)) %>% vascr_summarise(level = "experiment") %>% 
  vascr_plot_line()