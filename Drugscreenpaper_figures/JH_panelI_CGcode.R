library(tidyverse)
library(vascr)

# JH Tranche 1, from Angel share drive, Tranche 1 

#data files
# Adjustting to my code template

JHpanelIexp1 <- vascr_import("ECIS",
                            raw = "Drugscreenpaper_figures/ECIS_240304_MFT_1_JH_Drug_1.abp",
                            model = "Drugscreenpaper_figures/ECIS_240304_MFT_1_JH_Drug_1_RbA_regenerated.csv", experiment = "exp1"
)

# only including the vehicle ones, don't care about plasmin
JHpanelIexp1key <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, 
    1, "C", "1 2 3",  "High Imapramine",
    2, "D", "1 2 3", "Low Imapramine", 
    
    3, "G", "1 2 3", "High Fingolimod",  
    4, "H", "1 2 3", "Low Fingolimod",
    
    5, "C", "4 5 6",  "High Marimastat",
    6, "D", "4 5 6", "Low Marimastat", 
    
    7, "G", "4 5 6", "High Fasudil",  
    8, "H", "4 5 6", "Low Fasudil",
    
    9, "C", "7 8 9", "High SCH79797",
    10, "D", "7 8 9", "Low SCH79797",
    
    11, "G", "7 8 9", "High Ibuprofen",
    12, "H", "7 8 9", "Low Ibuprofen",
    
    13, "C", "10 11 12", "High RAP",
    14, "D", "10 11 12", "Low RAP",
    
    20, "F", "10 11 12", "LoDMSO",
    21, "G", "10 11 12", "HiDMSO",
    22, "H", "10 11 12", "H2O")
  
JHpanelIexp1labeled <- vascr:::vascr_apply_map(JHpanelIexp1, JHpanelIexp1key)
  

# rep2
JHpanelIexp2 <- vascr_import("ECIS",
                             raw = "Drugscreenpaper_figures/ECIS_240312_MFT_1_Drug_2.abp",
                             model = "Drugscreenpaper_figures/ECIS_240312_MFT_1_Drug_2_RbA.csv", experiment = "exp2"
)

JHpanelIexp2key <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, 
  1, "C", "4 5 6",  "High Imapramine",
  2, "D", "4 5 6", "Low Imapramine", 
  
  3, "G", "4 5 6", "High Fingolimod",  
  4, "H", "4 5 6", "Low Fingolimod",
  
  5, "C", "1 2 3",  "High Marimastat",
  6, "D", "1 2 3", "Low Marimastat", 
  
  7, "G", "1 2 3", "High Fasudil",  
  8, "H", "1 2 3", "Low Fasudil",
  
  9, "C", "10 11 12", "High SCH79797",
  10, "D", "10 11 12", "Low SCH79797",
  
  11, "G", "7 8 9", "High Ibuprofen",
  12, "H", "7 8 9", "Low Ibuprofen",
  
  13, "C", "7 8 92", "High RAP",
  14, "D", "7 8 9", "Low RAP",
  
  20, "F", "10 11", "LoDMSO", #F12 was vascr_excluded in JH code, so just took out here
  21, "G", "10 11 12", "HiDMSO",
  22, "H", "10 11 12", "H2O")
  

JHpanelIexp2labeled <- vascr:::vascr_apply_map(JHpanelIexp2, JHpanelIexp2key)
  
## combining n=2
JHpanelIfulldata<- vascr_combine(JHpanelIexp1labeled, JHpanelIexp2labeled) %>% drop_na() %>% 
  vascr_subset(unit = "Rb") %>% 
  vascr_zero_time(65) %>% 
  vascr_subset(time = c(-5, 24)) %>% 
  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_resample_time() 





#first couple low DMSO drugs
loDMSO<- gridplot(drugdata= JHpanelIfulldata, samples= c(1:10, 20:22), vehicle= "LoDMSO") +ggtitle('0.01% DMSO vehicle')


#ibuprofen in high DMSO
HiDMSO<- gridplot(drugdata= JHpanelIfulldata, samples = c(11:12, 20:22), vehicle="HiDMSO")+ggtitle('0.1% DMSO vehicle')

#RAP in water
water<- gridplot(drugdata= JHpanelIfulldata, samples = c(13:14, 20:22), vehicle="HiDMSO")+ggtitle('water vehicle')

loDMSO /( HiDMSO+ water)

#vascr_subset not subsetting. 

# Grid function -----------------------------------------------------------


### Writing a function that will let me input arguments of dataframe, and of sampleID, and of vehicle dataframe, that will produce corresponding grid plot. 

gridplot<- function(drugdata, samples, vehicle){ #argument sampleID as a vector
  
  library(vascr)
  
  #take drugdata and subset to select drugs of choice, which is specified by sampleID. 
  
  summariseddrugdata<- drugdata %>% filter(SampleID %in% samples) %>% vascr_summarise(level="summary")
  
  dissecteddf<- summariseddrugdata %>% 
    mutate(drug=word(summariseddrugdata$Sample, 2)) %>% #drug for vehicle will be NA
    mutate(concentration = word(summariseddrugdata$Sample, 1))
           
  drugonlydf <- dissecteddf %>% drop_na() #vehicles all have NA. So this only leaves our drugs
    
  vehicleonlydf <- dissecteddf %>% filter(if_any(everything(), is.na))  ## vehicle only df which keeps the rows with NA. Ie. all the vehicle rows
  vehicleonlydf$drug = NULL
  
  
  associatedvehicle<- vehicleonlydf %>% filter(concentration == vehicle) # using argument, selecting the correct vehicle to put in background of plots
  
#plotting
  ggplot()+
    geom_line(data= associatedvehicle, aes(x=Time, y=Value), color="darkgrey")+  #base layer of vehicle line/ribbon
    geom_ribbon(data=associatedvehicle, aes(x=Time, ymax=max, ymin=min), alpha=0.3)+
    geom_line(data= drugonlydf, mapping= aes(x=Time, y=Value, color=concentration))+  
    geom_ribbon(data=drugonlydf, mapping= aes(x= Time, ymax=max, ymin=min, fill=concentration), alpha=0.4)+
    facet_wrap(~drug) + theme_bw()
}



# QC of individual drugs to find outlier wells

JHpanelIfulldata %>% vascr_summarise(level="well") %>% vascr_subset() %>% vascr_plot_line()



# panel II ----------------------------------------------------------------


JHpanelIIexp1 = vascr_import("ECIS", raw = "Drugscreenpaper_figures/ECIS_240507_MFT_1_g3_s1.abp",
                     model ="Drugscreenpaper_figures/ECIS_240507_MFT_1_g3_s1_RbA.csv",
                     experiment= "Exp1")
JHpanelIIexp1key = tribble(~SampleID, ~Row, ~ Column, ~ Sample,

                    1, "C", "1 2 3", "High Edavarone",
                    2, "D", "1 2 3", "Low Edavarone",

                    3, "C", "4 5 6", "High Batimistat",
                    4, "D", "4 5 6", "Low Batimistat",
                    
                    5, "C", "7 8 9", "High Minocycline",
                    6, "D", "7 8 9", "Low Minocycline",
                    
                    7, "G", "1 2 3", "High Doxycycline",
                    8, "H", "1 2 3", "Low Doxycycline",
                    
                    9, "G", "4 5 6", "High Y-27632",
                    10, "H", "4 5 6", "Low Y-27632",
                    
                    11, "G", "7 8 9", "High LiCl",
                    12, "H", "7 8 9", "Low LiCl",
                    
                    
                    20, "A", "10 11 12", "LoDMSO",
                    21, "E", "10 11 12", "HiDMSO",
                    22, "C", "10 11", "H20",) #C12 water looks iffy. Exclude
JHpanelIIexp1labeled = vascr:::vascr_apply_map(JHpanelIIexp1, JHpanelIIexp1key)


JHpanelIIexp2 = vascr_import("ECIS", "Drugscreenpaper_figures/ECIS_240528_MFT_1.abp",
                     "Drugscreenpaper_figures/ECIS_240528_MFT_1_RbA_V2.csv",
                     "Exp2")
JHpanelIIexp2key = tribble(~SampleID, ~Row, ~ Column, ~ Sample, 
                    1, "G", "7 8 9", "High Edavarone", 
                    2, "H", "7 8 9", "Low Edavarone", 
 
                    3, "G", "10 11", "High Batimistat",
                    4, "H", "10 11", "Low Batimistat", # H12 excluded from JH code
                    
                    5, "C", "2 3", "High Minocycline", # D1 and C1 excluded from JH code, unsure why?
                    6, "D", "2 3", "Low Minocycline",
                    
                    7, "G", "1 2 3", "High Doxycycline",
                    8, "H", "4 5 6", "Low Doxycycline",
                    
                    9, "C", "10", "High Y-27632", #C12 C11 excluded
                    10, "D", "10 11 12", "Low Y-27632",
                    
                    11, "C", "7 9", "High LiCl", # excluded outlier C8
                    12, "D", "7 8 9", "Low LiCl",  
                    
                    20, "A", "4 5 6", "LoDMSO", 
                    21, "E", "4 5 6", "HiDMSO", 
                    22, "C", "4 5 6", "H20")
JHpanelIIexp2labeled = vascr:::vascr_apply_map(JHpanelIIexp2, JHpanelIIexp2key)


#combining two runs
JHpanelIIfulldata <- vascr_combine(JHpanelIIexp1labeled, JHpanelIIexp2labeled) %>% drop_na() %>% 
  vascr_subset(unit = "Rb") %>% 
  vascr_zero_time(65) %>% 
  vascr_subset(time = c(-5, 24)) %>% 
  vascr_normalise(-2, divide=TRUE) %>% 
  vascr_resample_time() 




#Batimistat
loDMSOII<- gridplot(drugdata= JHpanelIIfulldata, samples= c(3,4, 20:22), vehicle= "LoDMSO") +ggtitle('0.01% DMSO vehicle')
loDMSOII

#edaravone
HiDMSOII<- gridplot(drugdata= JHpanelIIfulldata, samples = c(1:2, 20:22), vehicle="HiDMSO")+ggtitle('0.1% DMSO vehicle')

#everything else - Minocycline, doxycyclin, Y-27632, LiCl
waterII<- gridplot(drugdata= JHpanelIIfulldata, samples = c(5:12, 20:22), vehicle="HiDMSO")+ggtitle('water vehicle')

waterII

(loDMSOII + HiDMSOII) 



JHpanelIIfulldata %>% vascr_subset(sampleid = c(1:4)) %>% vascr_summarise(level="well") %>% vascr_plot_line()
JHpanelIIfulldata %>% vascr_subset(sampleid = c(22)) %>% vascr_summarise(level="well") %>% vascr_plot_line()
