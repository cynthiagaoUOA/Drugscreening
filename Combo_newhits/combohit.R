#ICC with paired ECIS
#hits alone using fresh VPA, LiCl, asta. Combinations of two of the above. Glutathione as well
library(tidyverse)
library(vascr)

newcombosdata <- vascr_import("ECIS",
                            raw = "Combo_newhits/ECIS_260209_MFT_1_CG_pairedICChits2 (2).abp",
                            model = "Combo_newhits/ECIS_260209_MFT_1_CG_pairedICChits2 (2)_RbA.csv", experiment = "exp1")


newcomboskey <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, #triplicate treatments
  1, "A B C", "1",  "Low VPA LiCl",
  2, "A B C", "2", "High VPA LiCl",
  3, "A B C", "3", "Low VPA LiCl plasmin",
  4, "A B C", "4", "High VPA LiCl plasmin",
  
  5, "A B C", "5",  "Low asta LiCl",
  6, "A B C", "6", "High asta LiCl",
  7, "A B C", "7", "Low asta LiCl plasmin",
  8, "A B C", "8", "High asta LiCl plasmin",
  
  9, "A B C", "9",  "Low asta VPA",
  10, "A B C", "10", "High asta VPA",
  11, "A B C", "11", "Low asta VPA plasmin",
  12, "A B C", "12", "High asta VPA plasmin",
  
  13, "D E F", "5",  "Low VPA",
  14, "D E F", "6", "High VPA",
  15, "D E F", "7", "Low VPA plasmin",
  16, "D E F", "8", "High VPA plasmin",
  
  17, "D E F", "9",  "Low asta",
  18, "D E F", "10", "High asta",
  19, "D E F", "11", "Low asta plasmin",
  20, "D E F", "12", "High asta plasmin",
  
  21, "G", "1 2",  "Low LiCl", 21, "H", "1",  "Low LiCl",
  22, "H", "2 3", "High LiCl", 22, "G", "3", "High LiCl",
  23, "G", "4 5", "Low LiCl plasmin", 23, "G", "4", "Low LiCl plasmin",
  24, "H", "5 6", "High LiCl plasmin", 24, "H", "6", "High LiCl plasmin",
  
  25, "G", "7 8",  "Low glutathione", 25, "H", "7",  "Low glutathione",
  26, "H", "8 9", "High glutathione", 26, "G", "9", "High glutathione",
  27, "G", "10 11", "Low glutathione plasmin",  27, "H", "10", "Low glutathione plasmin",
  28, "H", "11 12", "High glutathione plasmin", 28, "G", "12", "High glutathione plasmin",
  
  100, "D E F", "1", "vehicle", 
  101, "D E F", "3", "320nM plasmin",
  102, "D E F", "2", "vehicle for glutathione", 
  103, "D E F", "4", "320nM plasmin for glutathione")




newcomboslabeled <- vascr:::vascr_apply_map(newcombosdata, newcomboskey)


newcombosplotdata <- newcomboslabeled %>%
  vascr_subset(unit = "Rb") %>%
  vascr_zero_time(65) %>%
  vascr_resample_time(500) %>%
  vascr_normalise(-2, divide = TRUE) 

newcombosplotdata %>%  vascr_subset(sampleid = c(1:4, 100, 101), time=c(-4,25)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line()

#VPA is protective, especially at the higher concentration

newcombosplotdata %>%  vascr_subset(sampleid = c(13:16, 100, 101), time=c(-4,25)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line()


# astaxanthin is not protective here. Newly made up, dark red, definitely went in
newcombosplotdata %>%  vascr_subset(sampleid = c(17:20, 100, 101), time=c(-4,25)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line()

# newly made up LiCl not looking super protective. Slightly more toxic than previously seen, 
# made up as a 1000 x stock
newcombosplotdata %>%  vascr_subset(sampleid = c(21:24, 100, 101), time=c(-4,25)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line()

# dose dependently toxic. Opposite pattern to what we saw previously
newcombosplotdata %>%  vascr_subset(sampleid = c(25:28, 102, 103), time=c(-4,25)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line()


#
newcombosplotdata %>%  vascr_subset(sampleid = c(5:8, 100, 101), time=c(-4,25)) %>% vascr_summarise(level = "experiment") %>%
vascr_plot_line()
# basally, adding asta to VPA strenghtens
newcombosplotdata %>%  vascr_subset(sampleid = c(9:12,14,10,18, 100,101), time=c(-4,25)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line() +theme_minimal()+ylim(0.6, 1.3)

#
newcombosplotdata %>%  vascr_subset(sampleid = c(5:8, 100, 101), time=c(-4,25)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line()

newcombosplotdata %>%  vascr_subset(sampleid = c(100:104), time=c(-4,25)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line()




