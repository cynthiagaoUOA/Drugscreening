# VPA has been protective in three runs with good plasmin now

# combine combohits1 with pl

vpa1 <- vascr_import("ECIS",
                              raw = "Combo_newhits/ECIS_260209_MFT_1_CG_pairedICChits2 (2).abp",
                              model = "Combo_newhits/ECIS_260209_MFT_1_CG_pairedICChits2 (2)_RbA.csv", experiment = "exp1")

vpa1key <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, #triplicate treatments
  1, "D E F", "5",  "low VPA",
  2, "D E F", "6", "high VPA",
  3, "D E F", "7", "low VPA plasmin",
  4, "D E F", "8", "high VPA plasmin",
  100, "D E F", "1", "vehicle", 
  101, "D E F", "3", "plasmin")

vpa1labeled <- vascr:::vascr_apply_map(vpa1, vpa1key)


vpa1plot <- vpa1labeled %>%
  vascr_subset(unit = "Rb") %>%
  vascr_zero_time(65) %>%
  vascr_resample_time(500) %>%
  vascr_normalise(-2, divide = TRUE) 



vpa2<- vascr_import("ECIS", raw = "Drugscreen_panel_II_plasminfixed/ECIS_260120_MFT_1_CG_drughitsfullplasmin1.abp",
                            model ="Drugscreen_panel_II_plasminfixed/ECIS_260120_MFT_1_CG_drughitsfullplasmin1_RbAfixed.csv",
                            experiment= "Exp2")
#filename does not match experiment because I changed plans

vpa2key = tribble(~SampleID, ~Row, ~ Column, ~ Sample,
                          
                          1, "A", "4 5 6", "low VPA",
                          3, "B", "5 6", "low VPA plasmin", #B4 was edentified as bad well pretreatment. But forgot to swap placement to good well
                          2, "C", "4 5 6", "high VPA",
                          4, "D", "4 5 6", "high VPA plasmin",
                    100, "A", "2 3", "vehicle", 10, "B", "2", "vehicle",
                    101, "C", "2 3", "plasmin", 101, "B", "3", "plasmin",
                  )



vpa2labeled = vascr:::vascr_apply_map(vpa2, vpa2key)

vpa2plot<- vpa2labeled %>% 
  vascr_subset(unit = "Rb") %>% 
  vascr_zero_time(64.146) %>% 
  vascr_subset(time = c(-5, 24)) %>% 
  vascr_normalise(-2, divide=TRUE) %>% 
  vascr_resample_time(500) 
  
  
  
vpa3 test<- vascr_import("ECIS",
                    raw = "Drugscreen_panel_II_plasminfixed/ECIS_260115_MFT_1_CG_drugscreenhits_janpairedICC_1.abp",
                    model = "Drugscreen_panel_II_plasminfixed/ECIS_260115_MFT_1_CG_drugscreenhits_janpairedICC_1_RbA.csv", experiment = "exp1"
)
  #filename does not match experiment because I changed plans
  
  vpa3key = tribble(~SampleID, ~Row, ~ Column, ~ Sample,
                    
                    1, "D", "1 2 3", "low VPA",
                    4, "D", "4 5 6", "high VPA plasmin", #assuming high and low was mistakening swapped
                    2, "E", "2 3", "high VPA", 
                    2, "H", "2", "high VPA",
                    3, "F", "4 5 6", "low VPA plasmin",
                        
                    100, "G", "4 5 6", "vehicle",
                    101, "H", "4 5 6", "plasmin")
  
vpa3labeled<- vascr_apply_map(test, vpa3key)
                            
vpa3plot<- vpa3labeled %>% 
    vascr_subset(unit = "Rb", sampleid=c(1:101)) %>% 
    vascr_zero_time(64.4876) %>% 
    vascr_subset(time = c(-2, 24)) %>% 
    vascr_normalise(-2, divide=TRUE) %>% 
    vascr_resample_time(500) 


vpaall_data<- vascr_combine(vpa1plot,vpa2plot, vpa3plot)

vpaall_data %>% 
  vascr_subset(time = c(-4,20),sampleid = c(1:101)) %>%
  vascr_summarise(level = "summary") %>% vascr_plot_line()
