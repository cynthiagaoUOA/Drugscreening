library(tidyverse)

newdrugsplasmincleavage1 <- read_csv("Drugscreen_panel_II_badplasmineffect/newdrugsplasmincleavage1.csv")


newdrughit1<- newdrugsplasmincleavage1 %>% as.data.frame() %>% 
  pivot_longer(cols = -`Time`, names_to= "well", values_to= "fluorescence") %>% 
  mutate(treatment=well) #making a copy of the column

newdrughit1$treatment<- recode(newdrughit1$treatment,
                               A1='VPA high', A2='VPA high', A3='VPA high',
                               B1='VPA low', B2='VPA low', B3='VPA low',
                               C1='LiCl high', C2='LiCl high', C3='LiCl high',
                               D1='LiCl low', D2='LiCl low', D3='LiCl low',
                               E1='Edav high', E2='Edav high', E3='Edav high',
                               F1='Edav low', F2='Edav low', F3='Edav low',
                               G1='AXT high', G2='AXT high', G3='AXT high',
                               H1='AXT low', H2='AXT low', H3='AXT low',
                               
                               A4='Plasmin only', B4='Plasmin only', C4='Plasmin only',
                               D4='No plasmin', E4='No plasmin', F4='No plasmin'
                               ) %>% as.factor() 

newdrughit1$Time<-   newdrughit1$Time %>% round()                   

newdrughit1summary <- newdrughit1 %>% group_by(Time, treatment) %>% 
  summarise(mean_fluoro= mean(fluorescence),
            sd_fluoro= sd(fluorescence)) %>% drop_na()


#all
ggplot(newdrughit1summary, 
       aes(y=mean_fluoro, x=Time, fill=treatment))+
  geom_line(aes(colour=treatment))+
  geom_ribbon(aes(
    ymax=mean_fluoro+sd_fluoro, 
    ymin=mean_fluoro-sd_fluoro, fill=treatment),alpha=0.3)


#plasmin
#sem
newdrughit1summary %>% filter(treatment %in% c("Plasmin only","No plasmin")) %>% 
  ggplot(., 
         aes(y=mean_fluoro, x=Time, fill=treatment))+
  geom_line(aes(colour=treatment))+
  geom_ribbon(aes(
    ymax=mean_fluoro+sd_fluoro, 
    ymin=mean_fluoro-sd_fluoro, fill=treatment),alpha=0.3)

#by wells
newdrughit1 %>% filter(treatment %in% c("Plasmin only","No plasmin")) %>% 
  ggplot(., 
         aes(y=fluorescence, x=Time, color=well))+
  geom_line(aes(colour=well))


#AXT
AXT <- newdrughit1 %>% filter(well!="B4") %>% group_by(Time, treatment) %>% 
  summarise(mean_fluoro= mean(fluorescence),
            sd_fluoro= sd(fluorescence)) %>% drop_na() %>%  
  filter(treatment %in% c("Plasmin only","No plasmin", "AXT high", "AXT low")) %>% 
  ggplot(., 
         aes(y=mean_fluoro, x=Time, fill=treatment))+
  geom_line(aes(colour=treatment))+
  geom_ribbon(aes(
    ymax=mean_fluoro+sd_fluoro, 
    ymin=mean_fluoro-sd_fluoro, fill=treatment),alpha=0.3)
AXT
    
# edaravone
newdrughit1 %>% filter(well!="B4") %>% group_by(Time, treatment) %>% 
  summarise(mean_fluoro= mean(fluorescence),
            sd_fluoro= sd(fluorescence)) %>% drop_na() %>%  
  filter(treatment %in% c("Plasmin only","No plasmin", "Edav high", "Edav low")) %>% 
  ggplot(., 
         aes(y=mean_fluoro, x=Time, fill=treatment))+
  geom_line(aes(colour=treatment))+
  geom_ribbon(aes(
    ymax=mean_fluoro+sd_fluoro, 
    ymin=mean_fluoro-sd_fluoro, fill=treatment),alpha=0.3)


# LiCl
newdrughit1 %>% filter(well!="B4") %>% group_by(Time, treatment) %>% 
  summarise(mean_fluoro= mean(fluorescence),
            sd_fluoro= sd(fluorescence)) %>% drop_na() %>%  
  filter(treatment %in% c("Plasmin only","No plasmin", "LiCl high", "LiCl low")) %>% 
  ggplot(., 
         aes(y=mean_fluoro, x=Time, fill=treatment))+
  geom_line(aes(colour=treatment))+
  geom_ribbon(aes(
    ymax=mean_fluoro+sd_fluoro, 
    ymin=mean_fluoro-sd_fluoro, fill=treatment),alpha=0.3)
                          

# VPA
newdrughit1 %>% filter(well!="B4") %>% group_by(Time, treatment) %>% 
  summarise(mean_fluoro= mean(fluorescence),
            sd_fluoro= sd(fluorescence)) %>% drop_na() %>%  
  filter(treatment %in% c("Plasmin only","No plasmin", "VPA high", "VPA low")) %>% 
  ggplot(., 
         aes(y=mean_fluoro, x=Time, fill=treatment))+
  geom_line(aes(colour=treatment))+
  geom_ribbon(aes(
    ymax=mean_fluoro+sd_fluoro, 
    ymin=mean_fluoro-sd_fluoro, fill=treatment),alpha=0.3)



  
