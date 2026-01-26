# Full drugs panel II redo 1

panelIIredo1<- vascr_import("ECIS", raw = "Drugscreen_panel_II_plasminfixed/ECIS_260120_MFT_1_CG_drughitsfullplasmin1.abp",
                            model ="Drugscreen_panel_II_plasminfixed/ECIS_260120_MFT_1_CG_drughitsfullplasmin1_RbAfixed.csv",
                            experiment= "Exp1")
  #filename does not match experiment because I changed plans

panelIIredo1key = tribble(~SampleID, ~Row, ~ Column, ~ Sample,
                           
                           1, "A", "4 5 6", "low valproic acid vehicle",
                           2, "B", "5 6", "low valproic acid plasmin", #B4 was edentified as bad well pretreatment. But forgot to swap placement to good well
                           3, "C", "4 5 6", "high valproic acid vehicle",
                           4, "D", "4 5 6", "high valproic acid plasmin",
                          
                           5, "E", "4 5 6", "low butylphthalide vehicle",
                           6, "F", "4 5 6", "low butylphthalide plasmin",
                           7, "G", "4 5 6", "high butylphthalide vehicle",
                           8, "H", "4 5 6", "high butylphthalide plasmin",
                          
                           9, "A", "7 8 9", "low SB290572 vehicle",
                           10, "B", "7 8 9", "low SB290572 plasmin",
                           11, "C", "7 8 9", "high SB290572 vehicle",
                           12, "D", "7 8 9", "high SB290572 plasmin",
                          
                          13, "E", "7 8 9", "low marimastat vehicle",
                          14, "F", "7 8 9", "low marimastat plasmin",
                          15, "G", "7 8 9", "high marimastat vehicle",
                          16, "H", "7 8 9", "high marimastat plasmin",
                          
                          17, "A", "10 11 12", "low glutathione vehicle",
                          18, "B", "10 11 12", "low glutathione plasmin",
                          19, "C", "10 11 12", "high glutathione vehicle",
                          20, "D", "10 11 12", "high glutathione plasmin",
                          
                          21, "E", "10 11 12", "low insulin vehicle",
                          22, "F", "10 12", "low insulin plasmin", #F11 bad well - connection issue
                          23, "G", "10 11 12", "high insulin vehicle",
                          24, "H", "10 11 12", "high insulin plasmin",
                          
                          30, "A", "2 3", "vehicle", 30, "B", "2", "vehicle",
                          31, "C", "2 3", "plasmin", 31, "B", "3", "plasmin",
                          
                          32, "G", "7 8 9", "glutathione vehicle",
                          33, "H", "7 8 9", "glutathione plasmin")

panelIIredo1labeled = vascr:::vascr_apply_map(panelIIredo1, panelIIredo1key)

panelIIredo1plot<- panelIIredo1labeled %>% 
  vascr_subset(unit = "Rb") %>% 
  vascr_zero_time(64.146) %>% 
  vascr_subset(time = c(-5, 24)) %>% 
  vascr_normalise(-2, divide=TRUE) %>% 
  vascr_resample_time(500) 

#VPA
panelIIredo1plot %>% vascr_subset(sampleid = c(1:4, 30,31)) %>% vascr_summarise(level="experiment") %>% vascr_plot_line()

#butyl
panelIIredo1plot %>% vascr_subset(sampleid = c(5:8, 30,31)) %>% vascr_summarise(level="experiment") %>% vascr_plot_line()

#SB
panelIIredo1plot %>% vascr_subset(sampleid = c(9:12, 30,31)) %>% vascr_summarise(level="experiment") %>% vascr_plot_line()

#marimastat
panelIIredo1plot %>% vascr_subset(sampleid = c(13:16, 30,31)) %>% vascr_summarise(level="experiment") %>% vascr_plot_line()

#glutathione
panelIIredo1plot %>% vascr_subset(sampleid = c(17:20, 32,33)) %>% vascr_summarise(level="experiment") %>% vascr_plot_line()



#insulin
panelIIredo1plot %>% vascr_subset(sampleid = c(21:24, 30,31)) %>% vascr_summarise(level="experiment") %>% vascr_plot_line()
