# CGdrugscreen panel I


# n=2, high and low -------------------------------------------------------
drugscreen2 <- vascr_import("ECIS",
                            raw = "Drugscreen_panel_I/ECIS_250925_MFT_1_CG_drugscreen2.abp",
                            model = "Drugscreen_panel_I/ECIS_250925_MFT_1_CG_drugscreen2_RbA.csv", experiment = "exp2"
)

## Just putting 'high' and 'low' as concentrations. Real numbers are the CG_newdrugscreen script

screen2key <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, #triplicate treatments
  1, "A B C", "9",  "High rapamycin vehicle",
  2, "A C", "10", "Low rapamycin vehicle", 2, "G", "1", "Low rapamycin vehicle",
  3, "E F", "9", "High rapamycin plasmin", 3, "H", "2", "High rapamycin plasmin", 
  4, "D E F", "10", "Low rapamycin plasmin",
  
  5, "A B C", "1",  "High dipyridamole vehicle",
  6, "A B C", "2", "Low dipyridamole vehicle",
  7, "D E F", "1", "High dipyridamole plasmin",
  8, "D E F", "2", "Low dipyridamole plasmin",
  
  9, "A B C", "3",  "High ticagrelor vehicle",
  10, "A B C", "4", "Low ticagrelor vehicle",
  11, "D E F", "3", "High ticagrelor plasmin",
  12, "D E F", "4", "Low ticagrelor plasmin",
  
  13, "A B C", "5",  "High vorapaxar vehicle",
  14, "A B C", "6", "Low vorapaxar vehicle",
  15, "D E F", "5", "High vorapaxar plasmin",
  16, "D E F", "6", "Low vorapaxar plasmin",
  
  17, "A B C", "11", "High imatinib vehicle",
  18, "A B C", "12", "Low imatinib vehicle",
  19, "D E F", "11", "High imatinib plasmin",
  20, "D E F", "12", "Low imatinib plasmin",
  
  21, "A B C", "7",  "High icatibant vehicle",
  22, "A B C", "8", "Low icatibant vehicle",
  23, "D E F", "7", "High icatibant plasmin",
  24, "D E F", "8", "Low icatibant plasmin",
  
  25, "G H", "9 10", "vehicle", 
  26, "G H", "11 12", "320nM plasmin",
  
  27, "G", "2 4 5", "High edaravone vehicle",
  28, "H", "2 4 5", "Low edaravone vehicle",
  29, "G", "6 7 8", "High edaravone plasmin",
  30, "H", "6 7 8", "Low edaravone plasmin")


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
  1, "A B C", "5",  "High rapamycin vehicle",
  2, "A C C", "6", "Low rapamycin vehicle", 
  3, "D E F", "5", "High rapamycin plasmin", 
  4, "D E F", "6", "Low rapamycin plasmin",
  
  5, "A B C", "7",  "High dipyridamole vehicle",
  6, "A B C", "8", "Low dipyridamole vehicle",
  7, "D E F", "7", "High dipyridamole plasmin",
  8, "D E F", "8", "Low dipyridamole plasmin",
  
  9, "A B C", "3",  "High ticagrelor vehicle",
  10, "A B C", "4", "Low ticagrelor vehicle",
  11, "D E F", "3", "High ticagrelor plasmin",
  12, "D E F", "4", "Low ticagrelor plasmin",
  
  13, "A B C", "1",  "High vorapaxar vehicle",
  14, "A B C", "2", "Low vorapaxar vehicle",
  15, "D E F", "1", "High vorapaxar plasmin",
  16, "D E F", "2", "Low vorapaxar plasmin",
  
  17, "A B C", "9", "High imatinib vehicle",
  18, "A B C", "10", "Low imatinib vehicle",
  19, "D E F", "9", "High imatinib plasmin",
  20, "D E F", "10", "Low imatinib plasmin",
  
  21, "A B C", "11",  "High icatibant vehicle",
  22, "A B C", "12", "Low icatibant vehicle",
  23, "D E F", "11", "High icatibant plasmin",
  24, "D E F", "12", "Low icatibant plasmin",
  
  25, "G", "1 2 4", "vehicle", 
  26, "G", "5 6", "320nM plasmin", 26, "H", "5", "320nM plasmin",
  
  27, "H", "6 8 9", "High edaravone vehicle",
  28, "G", "7 8 9", "Low edaravone vehicle",
  29, "H", "10 11 12", "High edaravone plasmin",
  30, "G", "10 11 12", "Low edaravone plasmin")

screen3labeled <- vascr:::vascr_apply_map(drugscreen3, screen3key)

screen3plotdata <- screen3labeled %>%
  vascr_subset(unit = "Rb") %>%
  vascr_zero_time(72.87) %>%
  vascr_resample_time(500) %>%
  vascr_normalise(-2, divide = TRUE)


# dataframe highandlow just contains replicates 2 and 3, n=2 for drugscreen paper
highandlow<- vascr_combine(screen2plotdata, screen3plotdata) %>% 
  vascr_subset(time=c(-5,20))


# don't care about plasmin story so splitting the sample column into whether it's vehicle or plasmin, then filtering out plasmin


summarisedhighandlow<- highandlow %>% vascr_summarise(level="summary") #gets mean, sd, ribbon ymin and max values

library(stringr) #lets me separate out column Sample by position of word

facetteddrugdf<- summarisedhighandlow %>%  
  mutate(plasminorvehicle = word(summarisedhighandlow$Sample, -1)) %>% 
  mutate(drug = word(summarisedhighandlow$Sample, -2)) %>% 
  mutate(concentration = word(summarisedhighandlow$Sample, 1)) %>% 
  filter(plasminorvehicle=="vehicle") %>% 
  replace_na(list(drug="vehicle"))  #because vehicle column is only one word, it becomes 'NA' in the new drug column. Recoding so it's vehicle



# Separating out a 'vehicle only' dataframe so can add as a layer, ie. add as a background in all facets


drugsnovehicle<- facetteddrugdf %>% filter(drug!="vehicle") #just drugs
vehiclelayer<- facetteddrugdf %>% filter(drug=="vehicle") %>% 
  mutate(drug=NULL) #cant have any 'vehicle' entries in the drug column else will make a 'vehicle' facet. Only when there is no vehicle facet, will vehicle be forced to be added to all facets

ggplot()+
  geom_line(data= vehiclelayer, aes(x=Time, y=Value), color="darkgrey")+  #base layer of vehicle line/ribbon
  geom_ribbon(data=vehiclelayer, aes(x=Time, ymax=max, ymin=min), alpha=0.3)+
  geom_line(data= drugsnovehicle, mapping= aes(x=Time, y=Value, color=concentration))+  
  geom_ribbon(data=drugsnovehicle, mapping= aes(x= Time, ymax=max, ymin=min, fill=concentration), alpha=0.4)+
  facet_wrap(~drug) + theme_bw()





## vascrsummarise<- sampleseparatedhighandlow<- highandlow %>% 
##  mutate(plasminorvehicle = word(highandlow$Sample, -1)) %>% 
##  mutate(drug = word(highandlow$Sample, -2)) %>% 
##  mutate(concentration = word(highandlow$Sample, 1)) %>% vascr_summarise(level="summary")

vascr_plot_line()+ facet_wrap(~drug)
facetedhighandlow



# panelII bad plasmin -----------------------------------------------------


newpanelcombined<- vascr_combine(newpanel1plot, newpanel3plot) %>% 
  vascr_subset(unit = "Rb") %>% 
  vascr_resample_time(500) %>% vascr_subset(sampleid = c(1:8, 13:30))


summarisedpanelII<- newpanelcombined %>%  vascr_subset(time=c(-5,20)) %>% vascr_summarise(level="summary")
# no exp 2
# summarisedpanelII<- newpanelcombined %>%  vascr_subset(time=c(-5,20)) %>% 
#  vascr_summarise(level="summary") %>% vascr_exclude(experiment = "exp2")




facetteddrugIIdf<- summarisedpanelII %>%  
  mutate(plasminorvehicle = word(summarisedpanelII$Sample, -1)) %>% 
  mutate(drug = word(summarisedpanelII$Sample, -2)) %>% 
  mutate(concentration = word(summarisedpanelII$Sample, 1)) %>% 
  filter(plasminorvehicle=="vehicle") %>% 
  replace_na(list(drug="vehicle"))  #because vehicle column is only one word, it becomes 'NA' in the new drug column. Recoding so it's vehicle



# Separating out a 'vehicle only' dataframe so can add as a layer, ie. add as a background in all facets


drugsnovehicleII<- facetteddrugIIdf %>% filter(drug!="vehicle") #just drugs
vehiclelayerII<- facetteddrugIIdf %>% filter(drug=="vehicle") %>% 
  mutate(drug=NULL) #cant have any 'vehicle' entries in the drug column else will make a 'vehicle' facet. Only when there is no vehicle facet, will vehicle be forced to be added to all facets

# vehiclelayerII

ggplot()+
  geom_line(data= vehiclelayerII, aes(x=Time, y=Value), color="darkgrey")+  #base layer of vehicle line/ribbon
  geom_ribbon(data=vehiclelayerII, aes(x=Time, ymax=max, ymin=min), alpha=0.3)+
  geom_line(data= drugsnovehicleII, mapping= aes(x=Time, y=Value, color=concentration))+  
  geom_ribbon(data=drugsnovehicleII, mapping= aes(x= Time, ymax=max, ymin=min, fill=concentration), alpha=0.4)+
  facet_wrap(~drug) + theme_bw()


# QC

newpanelcombined %>%  vascr_subset(time=c(-5,20), sampleid = c(29)) %>% 
  vascr_summarise(level="well") %>%
  vascr_plot_line()
