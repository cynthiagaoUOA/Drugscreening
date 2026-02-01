#############

#first run DMSO issues
#second run strange, drugs don't look protective but that includes LiCl as well
#third run succesful
#drug combo 4 was plasmin in hypoxia with various other 


# loading packages --------------------------------------------------------
library(vascr)
library(tidyverse)
library(ggplot2)


# DRUGCOMBO3 SUCCESSFUL RUN -----------------------------------------------


#working directory in the drug combo folder
drugcombo3 <- vascr_import("ECIS",
                           raw = "Drugcombo_edav_fasudil_licl/ECIS_250729_MFT_1_CG_drugcombo3.abp",
                           model = "Drugcombo_edav_fasudil_licl/ECIS_250729_MFT_1_CG_drugcombo3_RbA.csv", experiment = "exp3"
)

drugskey3 <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, #triplicate treatments
  1, "A", "4 5 6", "high combo + plasmin",
  2, "A", "7 8 9", "low combo + plasmin",
  3, "A", "10 11 12", "high edaravone + plasmin", 
  4, "B", "4 5 6", "low edaravone + plasmin",
  5, "B", "7 8 9 ", "high combo + vehicle",
  6, "B", "10 11 12", "low combo + vehicle",
  7, "C", "4 5 6 ", "high licl + plasmin",
  8, "C", "7 8 9", "low licl + plasmin",
  9, "C", "10 11 12", "plasmin",
  10, "D", "4 5 6", "vehicle",
  11, "D", "7 8 9", "high fasudil + plasmin",
  12, "D", "10 11 12", "low fasudil + plasmin",
)

drugs_labeled3 <- vascr:::vascr_apply_map(drugcombo3, drugskey3)

drugs3 <- drugs_labeled3 %>%
  vascr_subset(unit = "Rb", sampleid = c(1:12)) %>%
  vascr_zero_time(72.8) %>%
  vascr_resample_time(500) %>%
  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(time = c(-2, 16), sampleid = c(1:12))

(drugs3  %>% vascr_summarise(level = "experiment") %>% ##ggplotly all
    vascr_plot_line()) %>% ggplotly()

# combo drugs
drugs3 %>%
  vascr_subset(sampleid = c(12,9,10)) %>%
  vascr_summarise(level = "experiment") %>%
  vascr_plot_line()

#edav alone
drugs3 %>%
  vascr_subset(sampleid = c(3,4,9,10)) %>%
  vascr_summarise(level = "experiment") %>%
  vascr_plot_line()


# Previous runs -----------------------------------------------------------


# loading in data from hypoxiaprobe3
drugcombo1 <- vascr_import("ECIS",
  raw = "ECIS_250721_MFT_1_CG_glucosefree1.abp",
  model = "ECIS_250721_MFT_1_CG_glucosefree1_RbA.csv", experiment = "exp1"
)
drugskey <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, # add treatment to dataframe by creating a map,
  1, " E", "1 2 3", "10mM LiCl + 320nM plasmin + 1% DMSO",
  2, "E", "4 5 6", "1mM LiCl + 320nM plasmin + 1% DMSO",
  3, "E", "7 8 9", "High drug combo + 320nM plasmin",
  4, "E", "10 11 12", "Low drug combo + 320nM plasmin",
  5, "F", "1 2 3", "500uM Edaravone + 320nM plasmin",
  6, "F", "4 5 6", "50uM Edaravone + 320nM plasmin",
  7, "F", "7 8 9", "High drug combo + vehicle",
  8, "F", "10 11 12", "Low drug combo + vehicle",
  9, "G", "1 2 3", "1.9uM Fasudil + 320nM plasmin + 1% DMSO",
  10, "G", "4 5 6", "0.19uM Fasudil + 320nM plasmin + 1% DMSO",
  11, "G", "7 8 9", "320nM plasmin + 1% DMSO",
  12, "G", "10 11 12", "Vehicle + 1% DMSO",
)



drugs_labeled <- vascr:::vascr_apply_map(drugcombo1, drugskey)

plot_data_drugs <- drugs_labeled %>%
  vascr_subset(unit = "Rb", sampleid = c(1:12)) %>%
  vascr_zero_time(52) %>%
  vascr_resample_time(500) %>%
  vascr_normalise(-2, divide = TRUE) %>% # normalizing to 2hr before treatment. normalization by division rather than subtraction
  vascr_subset(time = c(-2, 72), sampleid = c(1:12))

plot_data_drugs %>% vascr_summarise(level = "experiment") %>% # summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line()

plot_data_drugs %>%
  vascr_subset(sampleid = c(1, 2, 9, 10, 11, 12)) %>%
  vascr_summarise(level = "experiment") %>%
  vascr_plot_line()

#  # loading packages --------------------------------------------------------
library(vascr)
library(tidyverse)
library(ggplot2)

# loading in data from hypoxiaprobe3
drugcombo1 <- vascr_import("ECIS",
  raw = "ECIS_250721_MFT_1_CG_glucosefree1.abp",
  model = "ECIS_250721_MFT_1_CG_glucosefree1_RbA.csv", experiment = "exp1"
)
drugskey <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, # add treatment to dataframe by creating a map,
  1, " E", "1 2 3", "10mM LiCl + 320nM plasmin + 1% DMSO",
  2, "E", "4 5 6", "1mM LiCl + 320nM plasmin + 1% DMSO",
  3, "E", "7 8 9", "High drug combo + 320nM plasmin",
  4, "E", "10 11 12", "Low drug combo + 320nM plasmin",
  5, "F", "1 2 3", "500uM Edaravone + 320nM plasmin",
  6, "F", "4 5 6", "50uM Edaravone + 320nM plasmin",
  7, "F", "7 8 9", "High drug combo + vehicle",
  8, "F", "10 11 12", "Low drug combo + vehicle",
  9, "G", "1 2 3", "1.9uM Fasudil + 320nM plasmin + 1% DMSO",
  10, "G", "4 5 6", "0.19uM Fasudil + 320nM plasmin + 1% DMSO",
  11, "G", "7 8 9", "320nM plasmin + 1% DMSO",
  12, "G", "10 11 12", "Vehicle + 1% DMSO",
)

drugs_labeled <- vascr:::vascr_apply_map(drugcombo1, drugskey)

plot_data_drugs <- drugs_labeled %>%
  vascr_subset(unit = "Rb", sampleid = c(1:12)) %>%
  vascr_zero_time(52) %>%
  vascr_resample_time(500) %>%
  vascr_normalise(-2, divide = TRUE) %>% # normalizing to 2hr before treatment. normalization by division rather than subtraction
  vascr_subset(time = c(-2, 72), sampleid = c(1:12))

plot_data_drugs %>% vascr_summarise(level = "experiment") %>% # summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line()

plot_data_drugs %>%
  vascr_subset(sampleid = c(1, 2, 9, 10, 11, 12)) %>%
  vascr_summarise(level = "experiment") %>%
  vascr_plot_line()

#  drugcombotry2
setwd("~/Library/CloudStorage/OneDrive-TheUniversityofAuckland/1 PHD/2507 C edav licl fasudil plasmin")
drugcombo2 <- vascr_import("ECIS",
  raw = "ECIS_250724_MFT_1_CG_combodrug2.abp",
  model = "ECIS_250724_MFT_1_CG_combodrug2_RbA.csv", experiment = "exp2"
)


drugskey <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, # add treatment to dataframe by creating a map,
  1, "E F G", "1", "high combo + plasmin",
  2, "E F G", "2", "low combo + plasmin",
  3, "E F", "3", "high LiCl + plasmin", 3, "G", "12", "high LiCl + plasmin",
  4, "E F H", "4", "low LiCl + plasmin",
  5, "E F G", "5", "high edaravone + plasmin",
  6, "E F G", "6", "low edaravone + plasmin",
  7, "E F G", "7", "high combo + vehicle",
  8, "E F H", "8", "low combo + vehicle",
  9, "E F H", "9", "high fasudil + plasmin",
  10, "E F H", "10", "low fasudil + plasmin",
  11, "E F H", "11", "plasmin",
  12, "H", "9 10 11", "vehicle",
)

drugs_labeled <- vascr:::vascr_apply_map(drugcombo2, drugskey)

plot_data_drugs <- drugs_labeled %>%
  vascr_subset(unit = "Rb", sampleid = c(1:12)) %>%
  vascr_zero_time(65) %>%
  vascr_resample_time(500) %>%
  vascr_normalise(-2, divide = TRUE) %>% # normalizing to 2hr before treatment. normalization by division rather than subtraction
  vascr_subset(time = c(-2, 16), sampleid = c(1:12))

plot_data_drugs %>% vascr_summarise(level = "experiment") %>% # summary gives median only, experiment gives mean+/SEM, wells gives a line to every well
  vascr_plot_line()

plot_data_drugs %>%
  vascr_subset(sampleid = c(9,10,11, 12)) %>%
  vascr_summarise(level = "experiment") %>%
  vascr_plot_line()

(plot_data_drugs %>%
  vascr_subset(sampleid = c(1, 2, 11, 12, 7, 8, 3)) %>%
  vascr_summarise(level = "experiment") %>% vascr_plot_line()) %>%
  ggplotly()




  