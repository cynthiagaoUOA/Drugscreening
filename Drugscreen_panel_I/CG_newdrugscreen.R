library(vascr)
library(tidyverse)
library(ggplot2)


# Loading in data ---------------------------------------------------------

# REPLICATE 1 - HIGH and LOW LOW

drugscreen1 <- vascr_import("ECIS",
                            raw = "Drugscreen_panel_I/ECIS_250921_MFT_1_CG_drugscreen1.abp",
                            model = "Drugscreen_panel_I/ECIS_250921_MFT_1_CG_drugscreen1_RbA.csv", experiment = "exp1"
)

screen1key <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, #triplicate treatments
  1, "A", "1 2 3",  "10nM rapamycin vehicle",
  51, "B", "1 2 3", "0.1nM rapamycin vehicle",
  3, "A", "4 5 6", "10nM rapamycin plasmin",
  52, "B", "4 5 6", "0.1nM rapamycin plasmin",
  
  5, "A", "7 8 9",  "50uM dipyridamole vehicle",
  53, "B", "7 8 9", "0.5uM dipyridamole vehicle",
  7, "A", "10 11 12", "50uM dipyridamole plasmin",
  54, "B", "10 11 12", "0.5uM dipyridamole plasmin",
  
  9, "C", "1 2 3",  "10uM ticagrelor vehicle",
  55, "D", "1 2 3", "0.1uM ticagrelor vehicle",
  11, "C", "4 5 6", "10uM ticagrelor plasmin",
  56, "D", "4 5 6", "0.1uM ticagrelor plasmin",
  
  13, "C", "7 8 9",  "250nM vorapaxar vehicle",
  57, "D", "7 8 9", "2.5nM vorapaxar vehicle",
  15, "C", "10 11 12", "250nM vorapaxar plasmin",
  58, "D", "10 11 12", "2.5nM vorapaxar plasmin",
  
  17, "E", "1 2 3", "10uM imatinib vehicle",
  59, "F", "1 2 3", "0.1uM imatinib vehicle",
  19, "E", "4 5 6", "10uM imatinib plasmin",
  60, "F", "4 5 6", "0.1uM imatinib plasmin",
  
  21, "E", "7 8 9",  "1uM icatibant vehicle",
  61, "F", "7 8 9", "0.01uM icatibant vehicle",
  23, "E", "10 11 12", "1uM icatibant plasmin",
  62, "F", "10 11 12", "0.01uM icatibant plasmin",
  
  25, "G", "2 3 7 8 9", "vehicle", 
  26, "G", "4 5 6 10 12", "320nM plasmin")


screen1labeled <- vascr:::vascr_apply_map(drugscreen1, screen1key)

screen1plotdata <- screen1labeled %>%
  vascr_subset(unit = "Rb") %>%
  vascr_zero_time(65.26) %>%
  vascr_resample_time(500) %>%
  vascr_normalise(-2, divide = TRUE) 

# REPLICATE 2 - HIGH AND LOW
drugscreen2 <- vascr_import("ECIS",
                            raw = "Drugscreen_panel_I/ECIS_250925_MFT_1_CG_drugscreen2.abp",
                            model = "Drugscreen_panel_I/ECIS_250925_MFT_1_CG_drugscreen2_RbA.csv", experiment = "exp2"
)

screen2key <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, #triplicate treatments
  1, "A B C", "9",  "10nM rapamycin vehicle",
  2, "A C", "10", "1nM rapamycin vehicle", 2, "G", "1", "1nM rapamycin vehicle",
  3, "E F", "9", "10nM rapamycin plasmin", 3, "H", "2", "10nM rapamycin plasmin", 
  4, "D E F", "10", "1nM rapamycin plasmin",
  
  5, "A B C", "1",  "50uM dipyridamole vehicle",
  6, "A B C", "2", "5uM dipyridamole vehicle",
  7, "D E F", "1", "50uM dipyridamole plasmin",
  8, "D E F", "2", "5uM dipyridamole plasmin",
  
  9, "A B C", "3",  "10uM ticagrelor vehicle",
  10, "A B C", "4", "1uM ticagrelor vehicle",
  11, "D E F", "3", "10uM ticagrelor plasmin",
  12, "D E F", "4", "1uM ticagrelor plasmin",
  
  13, "A B C", "5",  "250nM vorapaxar vehicle",
  14, "A B C", "6", "25nM vorapaxar vehicle",
  15, "D E F", "5", "250nM vorapaxar plasmin",
  16, "D E F", "6", "25nM vorapaxar plasmin",
  
  17, "A B C", "11", "10uM imatinib vehicle",
  18, "A B C", "12", "1uM imatinib vehicle",
  19, "D E F", "11", "10uM imatinib plasmin",
  20, "D E F", "12", "1uM imatinib plasmin",
  
  21, "A B C", "7",  "1uM icatibant vehicle",
  22, "A B C", "8", "0.1uM icatibant vehicle",
  23, "D E F", "7", "1uM icatibant plasmin",
  24, "D E F", "8", "0.1uM icatibant plasmin",
  
  25, "G H", "9 10", "vehicle", 
  26, "G H", "11 12", "320nM plasmin",
  
  27, "G", "2 4 5", "50uM edaravone vehicle",
  28, "H", "2 4 5", "5uM edaravone vehicle",
  29, "G", "6 7 8", "50uM edaravone plasmin",
  30, "H", "6 7 8", "5uM edaravone plasmin")


screen2labeled <- vascr:::vascr_apply_map(drugscreen2, screen2key)

screen2plotdata <- screen2labeled %>%
  vascr_subset(unit = "Rb") %>%
  vascr_zero_time(71.66) %>%
  vascr_resample_time(500) %>%
  vascr_normalise(-2, divide = TRUE) 



# REPLICATE 3 - HIGH AND LOW
drugscreen3 <- vascr_import("ECIS",
                            raw = "Drugscreen_panel_I/ECIS_251001_MFT_1_CG_drugscreen3.abp",
                            model = "Drugscreen_panel_I/ECIS_251001_MFT_1_CG_drugscreen3_RbA.csv", experiment = "exp3")

screen3key <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, 
  1, "A B C", "5",  "10nM rapamycin vehicle",
  2, "A C C", "6", "1nM rapamycin vehicle", 
  3, "D E F", "5", "10nM rapamycin plasmin", 
  4, "D E F", "6", "1nM rapamycin plasmin",
  
  5, "A B C", "7",  "50uM dipyridamole vehicle",
  6, "A B C", "8", "5uM dipyridamole vehicle",
  7, "D E F", "7", "50uM dipyridamole plasmin",
  8, "D E F", "8", "5uM dipyridamole plasmin",
  
  9, "A B C", "3",  "10uM ticagrelor vehicle",
  10, "A B C", "4", "1uM ticagrelor vehicle",
  11, "D E F", "3", "10uM ticagrelor plasmin",
  12, "D E F", "4", "1uM ticagrelor plasmin",
  
  13, "A B C", "1",  "250nM vorapaxar vehicle",
  14, "A B C", "2", "25nM vorapaxar vehicle",
  15, "D E F", "1", "250nM vorapaxar plasmin",
  16, "D E F", "2", "25nM vorapaxar plasmin",
  
  17, "A B C", "9", "10uM imatinib vehicle",
  18, "A B C", "10", "1uM imatinib vehicle",
  19, "D E F", "9", "10uM imatinib plasmin",
  20, "D E F", "10", "1uM imatinib plasmin",
  
  21, "A B C", "11",  "1uM icatibant vehicle",
  22, "A B C", "12", "0.1uM icatibant vehicle",
  23, "D E F", "11", "1uM icatibant plasmin",
  24, "D E F", "12", "0.1uM icatibant plasmin",
  
  25, "G", "1 2 4", "vehicle", 
  26, "G", "5 6", "320nM plasmin", 26, "H", "5", "320nM plasmin",
  
  27, "H", "6 8 9", "50uM edaravone vehicle",
  28, "G", "7 8 9", "5uM edaravone vehicle",
  29, "H", "10 11 12", "50uM edaravone plasmin",
  30, "G", "10 11 12", "5uM edaravone plasmin")

screen3labeled <- vascr:::vascr_apply_map(drugscreen3, screen3key)

screen3plotdata <- screen3labeled %>%
  vascr_subset(unit = "Rb") %>%
  vascr_zero_time(72.87) %>%
  vascr_resample_time(500) %>%
  vascr_normalise(-2, divide = TRUE)

# REPLICATE 4 - LOW
drugscreen4 <- vascr_import("ECIS",
                            raw = "Drugscreen_panel_I/ECIS_251005_MFT_1_CG_drugscreen4.abp",
                            model = "Drugscreen_panel_I/ECIS_251005_MFT_1_CG_drugscreen4_RbA_corrected.csv", experiment = "exp4")

screen4key <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, #triplicate treatments
  
  2, "D E F", "5", "1nM rapamycin vehicle", 
  4, "D E F", "6", "1nM rapamycin plasmin",
  
  6, "A B C", "1", "5uM dipyridamole vehicle",
  8, "A B C", "2", "5uM dipyridamole plasmin",
  
  10, "A B C", "3", "1uM ticagrelor vehicle",
  12, "A B C", "4", "1uM ticagrelor plasmin",
  
  14, "A B C", "5", "25nM vorapaxar vehicle",
  16, "A B C", "6", "25nM vorapaxar plasmin",
  
  18, "D E F", "3", "1uM imatinib vehicle",
  20, "D E F", "4", "1uM imatinib plasmin",
  
  22, "D E F", "1", "0.1uM icatibant vehicle",
  24, "D E F", "2", "0.1uM icatibant plasmin",
  
  101, "A B C", "9",  "high phenothiazine Vehicle",
  102, "A C D", "10", "high phenothiazine plasmin", 
  103, "D E F", "9", "low phenothiazine vehicle", 
  104, "D E F", "10", "low phenothiazine plasmin",
  
  105, "A B C", "11",  "high catalase Vehicle",
  106, "A C D", "12", "high catalase plasmin", 
  107, "D E F", "11", "low catalase vehicle", 
  108, "D E F", "12", "low catalase plasmin",
  
  27, "A B C", "7", "50uM edaravone vehicle",
  28, "D E F", "7", "5uM edaravone vehicle",
  29, "A B C", "8", "50uM edaravone plasmin",
  30, "D E F", "8", "5uM edaravone plasmin",
  
  25, "G", "4 5 6", "vehicle", 
  26, "H", "4 5 6", "320nM plasmin",
  
  
  201, "H", "9 10 11 12", "AIM GOx",
  202, "G", "9 10 11 12", "Heated AIM-V media change control",
  203, "G H", "7 8", "AIM GOx with 15mM glucose added back in",
  204, "G H", "1", "No media change control", 204, "G", "2", "No media change control")


screen4labeled <- vascr:::vascr_apply_map(drugscreen4, screen4key)

screen4plotdata <- screen4labeled %>%
  vascr_subset(unit = "Rb") %>%
  vascr_zero_time(64.99) %>%
  vascr_resample_time(500) %>%
  vascr_normalise(-2, divide = TRUE) 

# REPLICATE 5 - LOW LOW of a select few

drugscreen5 <- vascr_import("ECIS",
                            raw = "Drugscreen_panel_I/ECIS_251010_MFT_1_CG_Goxtreatment1restart.abp",
                            model = "Drugscreen_panel_I/ECIS_251010_MFT_1_CG_Goxtreatment1restart_RbA.csv", experiment = "exp5")

#chose a select few to do low-low concentrations for
screen5key <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample,
  
  53, "A B C", "7", "0.5uM dipyridamole vehicle",
  54, "A B C", "8", "0.5uM dipyridamole plasmin",
  
  55, "A B C", "9", "0.1uM ticagrelor vehicle",
  56, "A B C", "10", "0.1uM ticagrelor plasmin",
  
  59, "A B C", "11", "0.1uM imatinib vehicle",
  60, "A B C", "12", "0.1uM imatinib plasmin",
  
  25, "D E F", "7", "vehicle", 
  26, "D E F", "8", "320nM plasmin",
  
  13, "A B C", "1",  "250nM vorapaxar vehicle",
  14, "D E F", "1", "25nM vorapaxar vehicle",
  15, "A B C", "2", "250nM vorapaxar plasmin",
  16, "D E F", "2", "25nM vorapaxar plasmin",
  
  
  109, "A B C", "5", "low low catalase vehicle",
  110, "D E F", "5", "low low low catalase vehicle",
  111, "A B C", "6", "low low catalase plasmin",
  112, "D E F", "6", "low low low catalase plasmin",
  
  27, "A B C", "3", "50uM edaravone vehicle",
  28, "D E F", "3", "5uM edaravone vehicle",
  29, "A B C", "4", "50uM edaravone plasmin",
  30, "D E F", "4", "5uM edaravone plasmin")


screen5labeled <- vascr:::vascr_apply_map(drugscreen5, screen5key)

screen5plotdata <- screen5labeled %>%
  vascr_subset(unit = "Rb") %>%
  vascr_zero_time(64.367) %>%
  vascr_resample_time(500) %>%
  vascr_normalise(-2, divide = TRUE) 


# Combining data and plotting ---------------------------------------------

# 25 26 is water. 1-30 are normal drugs, 
# 50-70 are the low low concentrations of some drugs.
# 100 onwards is the new drugs, catalase, phenothiazine, 
# 200 onwards is some Gox stuff in extra wells. Not relevant to this script

alldrugscreendata<- vascr_combine(screen1plotdata, screen2plotdata, screen3plotdata, screen4plotdata, screen5plotdata) %>% 
  vascr_subset(time=c(-5,20))
  

#subsetted by high and low concentrations because of missing data problem. Use as separate plots or redo an experiment. For paper n=2 is okay, for thesis redo once or use do separate high and low conc plots. If can frame as high then low then low low then could get away with it. ALl about framing.

#rapamycin high conc
alldrugscreendata %>%  vascr_subset(experiment=c("1","2","3"),
                                    sampleid = c(1,3, 25, 26)) %>% vascr_summarise(level = "summary") %>%
  vascr_plot_line()+ylim(0.55,1.15)

#rapamycin low conc
alldrugscreendata %>%  vascr_subset(experiment=c("2","3","4"),
                                    sampleid = c(2,4, 25, 26)) %>% vascr_summarise(level = "summary") %>%
  vascr_plot_line()+ylim(0.55,1.15)

#dipyridamole high
alldrugscreendata  %>%  vascr_subset(experiment=c("1","2","3"),
                                                  sampleid = c(5,7, 25, 26)) %>% vascr_summarise(level = "summarise") %>%
  vascr_plot_line()+ylim(0.55,1.15) #went through and looked at which well was the outlier from rep 3. BC of switching up platemap this should only affect rep 3

#low conc
alldrugscreendata %>%  vascr_subset(experiment=c("2","3","4"),
                                    sampleid = c(6,8, 25, 26)) %>% vascr_summarise(level = "summary") %>%
  vascr_plot_line()+ylim(0.55,1.15)
# low low conc
alldrugscreendata %>%  vascr_subset(experiment=c("1","5"),
                                    sampleid = c(53,54, 25, 26)) %>% vascr_summarise(level = "summary") %>%
  vascr_plot_line()




#tricagrelor
alldrugscreendata  %>% vascr_subset(experiment=c("1","2","3"),
                                sampleid = c(9,11, 25, 26)) %>% vascr_summarise(level = "summarise") %>% vascr_plot_line()
alldrugscreendata  %>% vascr_subset(experiment=c("4","2","3"),
                                    sampleid = c(10,12, 25, 26)) %>% vascr_summarise(level = "summarise") %>% vascr_plot_line()
alldrugscreendata %>%  vascr_subset(experiment=c("1","5"),
                                    sampleid = c(55,56, 25, 26)) %>% vascr_summarise(level = "summary") %>% vascr_plot_line()


#vorapaxar
alldrugscreendata  %>%  vascr_subset(experiment=c("1","2","3","5"),
                                     sampleid = c(13,15, 25, 26)) %>% vascr_summarise(level = "experiment") %>% vascr_plot_line() + 
  facet_wrap(~Experiment)

alldrugscreendata  %>%  vascr_subset(experiment=c("2","3","4","5"),
                                                  sampleid = c(14,16, 25, 26)) %>% vascr_summarise(level = "experiment") %>% 
  vascr_plot_line()+ facet_wrap(~Experiment)


#imatinib
alldrugscreendata  %>%  vascr_subset(experiment=c("1","2","3"),
                                                  sampleid = c(17,19, 25, 26)) %>% vascr_summarise(level = "summarise") %>% vascr_plot_line()

alldrugscreendata  %>%  vascr_subset(experiment=c("4","2","3"),
                                     sampleid = c(18,20, 25, 26)) %>% vascr_summarise(level = "summarise") %>% vascr_plot_line()

alldrugscreendata %>%  vascr_subset(experiment=c("1","5"),
                                    sampleid = c(59,60, 25, 26)) %>% vascr_summarise(level = "summary") %>% vascr_plot_line()


#icatibant
alldrugscreendata  %>%  vascr_subset(experiment=c("1","2","3"),
                                     sampleid = c(21,23, 25, 26)) %>% vascr_summarise(level = "summarise") %>% vascr_plot_line()+ylim(0.6,1.1)

alldrugscreendata  %>%  vascr_subset(experiment=c("4","2","3"),
                                     sampleid = c(22,24, 25, 26)) %>% vascr_summarise(level = "summarise") %>% vascr_plot_line()+ylim(0.6,1.1)



#edaravone
alldrugscreendata  %>%  vascr_subset(experiment=c("4","5","2","3"),sampleid = c(27,29, 25, 26)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line()+facet_wrap(~Experiment)
alldrugscreendata  %>%  vascr_subset(experiment=c("4","5","2","3"),sampleid = c(28,30, 25, 26), time = c(-5,20)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line()+facet_wrap(~Experiment)


# catalase

alldrugscreendata  %>%  vascr_subset(experiment="5",sampleid = c(109:112, 25, 26), time = c(-5,20)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line()+facet_wrap(~Experiment)

# Need to keep in mind the missing data problem, ie. only include data from full datasets in one graph. Can't select from different repeats. 
# Makes things tricky.. 


# n=2, high and low -------------------------------------------------------

# dataframe highandlow just contains replicates 2 and 3, n=2 for drugscreen paper
highandlow<- vascr_combine(screen2plotdata, screen3plotdata) %>% 
  vascr_subset(time=c(-5,20))

highandlow %>% vascr_summarise(level="summary") %>% vascr_plot_line() + facet_wrap(~Sample)

# don't care about plasmin story so splitting the sample column into whether it's vehicle or plasmin, then filtering out plasmin

library(stringr)


highandlow %>% mutate(plasminorvehicle = word(highandlow$Sample, -1)) %>% filter(plasminorvehicle=="vehicle") %>% #make new column that contains the last word of the sample column
  vascr_summarise(level="summary") %>% vascr_plot_line()+ geom_hline(yintercept=1, alpha=0.5, linetype=2) + facet_wrap(~Sample)


#curiosity/ for thesis, just plasmin
highandlow %>% mutate(plasminorvehicle = word(highandlow$Sample, -1)) %>% filter(plasminorvehicle=="plasmin") %>% 
  vascr_summarise(level="summary") %>% vascr_plot_line()+ geom_hline(yintercept=1, alpha=0.5, linetype=2) + facet_wrap(~Sample)



#would want to refactor so that each drug is grouped together, started to below but need to redo the ggplot mapping so a little bit complicated maybe

facetedhighandlow<- highandlow %>% 
  mutate(plasminorvehicle = word(highandlow$Sample, -1)) %>% 
  mutate(drug = word(highandlow$Sample, -2)) %>% 
  filter(plasminorvehicle=="vehicle") %>% 
  vascr_summarise(level="summary") 



vascr_plot_line()+ facet_wrap(~drug)
facetedhighandlow