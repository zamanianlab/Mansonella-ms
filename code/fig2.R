# data wrangling
library(tidyverse)
library(tidyr)
library(janitor)
library(dplyr)

# other plotting
library(ggbeeswarm)
library(ggpubr)
library(cowplot)
library(survminer)
library(gridExtra)
library(grid)
library(ggtext)
library(ZamanianLabThemes)

# misc
library(lubridate)
library(conflicted)
library(magrittr)
library(Hmisc)
library(broom)

#PDF import
library(magick)
library(pdftools)
library(grImport2)


library(here)


conflict_prefer("filter", "dplyr")

#set working directory-------------------------------- 
setwd<-here() 

#set theme 
aesthetic <- 
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),  axis.line = ggplot2::element_line(size = 0.25, colour = "black"),
    legend.text = element_text(size = 10),
    axis.title.x=element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1,face='plain', size=8),
    axis.text.y = element_text(face = "plain", size = 8),
    strip.text.y = element_text(size=12),        
    axis.title.y = element_text(size = 12, angle=90,face = "plain"),
    panel.border = element_blank(),
    strip.text = ggplot2::element_text(face = "plain", size = 10))+
  theme(panel.border = element_rect(color="black", fill=NA)) 

#read in data 
demographic_data<- read_csv("../data/demographics.csv")
micro_data<- read_csv("../data/microscopy.csv")
LAMP_data<- read_csv("../data/LAMP_quant.csv")

#tidy data and rename survey 1 data 

Lamp<- LAMP_data%>%
  pivot_longer(cols= c(8:13,19,20,21),names_to = "rep", values_to = "result")%>%
  mutate( result = case_when(
    result == 'Empty_well'~ 'NA', 
    result == 'Empty_well_P'~ 'NA', 
    result == 'INC'~ 'NA', 
    result == 'I'~ 'NA', 
    result == 'N'~ 'Negative', 
    result == 'negative'~ 'Negative', 
    result == 'P'~ 'Positive', 
    result == 'positive'~ 'Positive', 
    result == 'no_reulst'~ 'NA', 
    result == 'NA'~ 'NA' ))%>%
  filter(result != 'NA')%>%
  mutate_at("ID", str_replace, "AMF-", "")

Lamp$ID<-(as.numeric(as.character(Lamp$ID)))

Lamp$rep <- factor (Lamp$rep , levels = c('plasma1',"UW1","UW2","UW3","UW4","NEB1","NEB23","consensus",'plasma2'), 
                    labels=c('Plasma','Rep0','Rep1','Rep2','Rep3','Rep4','Rep5','Consensus','Plasma2'))
         
blood<-Lamp%>%
  filter(!rep %in% c('Rep3','Rep0','Plasma2', 'Plasma','Consensus'))%>%
  filter(ID != 4)

consensus<-Lamp%>%
  filter(rep == 'Consensus')%>%
  filter(ID != 4)

consensus%>% count(result)
61+54
54/115

plasma<-Lamp%>%
  filter(rep == 'Plasma')%>%
  filter(ID != 4)

plasma%>% count(result)  
31/115

#plot for Plasma lamps 

plasma.plot<-plasma%>%
  ggplot(aes(x= ID, y= rep, fill= result))+
  geom_tile(colour="white",size=0.25)+
  scale_fill_manual(values = c( '#EDEDED','#f4c33a',"#FFCFA4"))+
  labs(x = "", y = expression ("L"[p]), size=10) +
  scale_x_continuous(limits= c(0,118), breaks= seq(1,117,5),expand = c(0, 0))+
  scale_y_discrete(position= 'right',expand=c(0,0))+
  theme_zlab() +
  aesthetic +
  theme (
    axis.text.y = element_blank(),
    axis.ticks.y= element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1,face='plain', size=6),
  )
plasma.plot

#plot for Blood lamps 
consensus.plot<-consensus%>%
  ggplot(aes(x= ID, y= rep, fill= result))+
  geom_tile(colour="white",size=0.25)+
  scale_fill_manual(values = c( '#EDEDED',"#c26a7a","#FFCFA4"))+
  labs(x = "",y = expression ("L"[b]), size=10) +
  geom_hline(yintercept= c(5.5, 1.5))+
  scale_x_continuous( limits= c(0,118), breaks= seq(1,117),expand = c(0, 0))+
  scale_y_discrete(position= 'right', expand=c(0,0))+
  theme_zlab() +
  aesthetic +
  theme (
    axis.text.y = element_blank(),
    axis.ticks.y= element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())
consensus.plot

#merge dataframes

data<-left_join(demographic_data, LAMP_data)%>%
  left_join(., micro_data)

#Demographic analysis

demo<- data%>%
  mutate_at("ID", str_replace, "AMF-", "")

demo$ID<-(as.numeric(as.character(demo$ID)))

#microscopy data

demo<-demo%>%
  filter(FILARIA_MICROSCOPY_enrollment %in% c('Positive','Negative'))

#smears for mf

demo%>%filter(ID <118)%>%
count(FILARIA_MICROSCOPY_enrollment)
88+16
16/104
demo%>%count(consensus,FILARIA_MICROSCOPY_enrollment)

smear.plot<-demo%>%
  ggplot(aes(x= ID, y= 'Micro', fill= FILARIA_MICROSCOPY_enrollment))+
  geom_tile(colour="white",size=0.25)+
  scale_fill_manual(values = c( '#EDEDED','#B5F7E0'))+
  labs(x = "", y = "M", size=10) +
  scale_x_continuous(limits= c(0,118), breaks= seq(1,117), expand=c(0,0))+
  scale_y_discrete(position= 'right',expand=c(0,0))+
  theme_zlab() +
   aesthetic +
     theme (axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(), 
            axis.ticks.y = element_blank()
  )
smear.plot

#####################
#survey 2 and 3 data 
#####################

#read in raw data 
demographic_data<- read_csv("../data/demographics.csv")
micro_data<- read_csv("../data/microscopy.csv")
LAMP_data<- read_csv("../data/LAMP_quant.csv")

#tidy data and rename 

Lamp<- LAMP_data%>%
  pivot_longer(cols= c(8:13,19,20,21),names_to = "rep", values_to = "result")%>%
  mutate( result = case_when(
    result == 'Empty_well'~ 'NA', 
    result == 'Empty_well_P'~ 'NA', 
    result == 'INC'~ 'NA', 
    result == 'I'~ 'NA', 
    result == 'N'~ 'Negative', 
    result == 'negative'~ 'Negative', 
    result == 'P'~ 'Positive', 
    result == 'positive'~ 'Positive', 
    result == 'no_reulst'~ 'NA', 
    result == 'NA'~ 'NA' ))%>%
  filter(result != 'NA')%>%
  mutate_at("ID", str_replace, "AMF-", "")

Lamp$ID<-(as.numeric(as.character(Lamp$ID)))

Lamp$rep <- factor (Lamp$rep , levels = c('plasma1','plasma2',"UW1","UW2","UW3","UW4","NEB1","NEB23","consensus"), 
                    labels=c('Plasma1','Plasma2','Rep0','Rep1','Rep2','Rep3','Rep4','Rep5','Consensus'))

blood<-Lamp%>%
  filter(!rep %in% c('Plasma1','Plasma2','Rep0','Rep1','Rep2','Rep4','Rep5','Consensus'))%>%
  filter(!ID %in% c(126,129,150,172))

#plot for Blood lamp 

blood.plot2<-blood%>%
  ggplot(aes(x= ID, y= rep, fill= result))+
  geom_tile(colour="white",size=0.25)+
  scale_fill_manual(values = c( '#EDEDED',"#c26a7a","#FFCFA4"))+
  labs(x = "", y = expression ("L"[b]), size=10) +
  geom_hline(yintercept= c(5.5, 1.5))+
  scale_x_continuous(limits= c(117,245), breaks= seq(118,244),expand = c(0, 0))+
  scale_y_discrete(position='right',expand=c(0,0))+
  theme_zlab() +
  aesthetic +
  theme (
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y= element_blank(), 
    axis.ticks.x=element_blank())

blood.plot2

blood%>%count(result)
80+40
40/120
plasma<-Lamp%>%
  filter(rep == 'Plasma2')%>%
  filter(!ID %in% c(126,129,150,172))

#plot for Plasma lamp 

plasma.plot2<-plasma%>%
  ggplot(aes(x= ID, y= rep, fill= result))+
  geom_tile(colour="white",size=0.25)+
  scale_fill_manual(values = c( '#EDEDED','#f4c33a',"#FFCFA4"))+
  labs(x = "", y = expression ("L"[p]), size=10) +
  geom_hline(yintercept= c(5.5, 1.5))+
  scale_x_continuous(limits= c(117,245), breaks= seq(118,244,5),expand = c(0, 0))+
  scale_y_discrete(position='right',expand=c(0,0))+
  theme_zlab() +
  aesthetic +
  theme (
    axis.text.x = element_text(angle = 45, hjust = 1,face='plain', size=6),
    axis.text.y = element_blank(),
    axis.ticks.y= element_blank() )

plasma.plot2

plasma%>%count(result)
19+104
123

#quantitative micro

#merge dataframes
data<-left_join(demographic_data, LAMP_data)%>%
  left_join(., micro_data)

demo<- data%>%
  mutate_at("ID", str_replace, "AMF-", "")

demo$ID<-(as.numeric(as.character(demo$ID)))

quant<-demo%>%
  select(1,94)

quant$FILARIA_per_40ul<-(as.numeric(as.character(quant$FILARIA_per_40ul)))

quant<-quant%>%
  filter(!ID %in% c(126,129,150,172))%>%
  filter( ID >117)

heatmap <- quant%>%
  ggplot( scale="row",aes( x= ID, y= "", fill= FILARIA_per_40ul))+
  geom_tile(colour="white",size=0.25) +
  scale_fill_gradientn(colors= c( '#EDEDED', '#E6D7D7', '#E0C2C2', '#D9ACAC', '#D39797', '#CC8181', '#C66C6C', '#BF5656', '#B94141', '#B22B2B', '#AC1616', '#A50000'),na.value ='white', limits= c(0,11), breaks =seq(0,11,3))+
  scale_x_continuous(limits= c(117,245), breaks= seq(118,244,5),expand = c(0, 0))+
  scale_y_discrete(position= 'right',expand = c(0, 0)) +
  labs(title= "",x = "", y = "qM", fill = "Count") +
  theme_zlab() +
  aesthetic+
  theme(
    panel.grid.major.y = element_blank(),
    legend.position="right",
    legend.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1,face='plain', size=6),
    axis.text.y = element_blank(),
    axis.ticks.y= element_blank())
heatmap

quant%>%filter (ID>117)%>%
  filter(!ID %in% c(126,129,150,172))%>%
  count(FILARIA_per_40ul)
7+4+4+3+2+1+1+1
23+99
23/122

#microscopy smear data
demo<-demo%>%
  filter(FILARIA_MICROSCOPY_enrollment %in% c('Positive','Negative'))

#smears for mf

smear.plot2<-demo%>%
  filter(!ID %in% c(126,129,150,172))%>%
  ggplot(aes(x= ID, y= 'Micro', fill= FILARIA_MICROSCOPY_enrollment))+
  geom_tile(colour="white",size=0.25)+
  scale_fill_manual(values = c( '#EDEDED','#B5F7E0'))+
  labs(x = "", y = "M", size=10) +
  scale_x_continuous(limits= c(117,245), breaks= seq(118,244), expand=c(0,0))+
  scale_y_discrete(position= 'right',expand=c(0,0))+
  theme_zlab() +
  aesthetic +
  theme (axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.x = element_blank(), 
         axis.ticks.y = element_blank()
  )

smear.plot2

demo%>%filter(ID>117)%>%
  filter(!ID %in% c(126,129,150,172))%>%
  count(FILARIA_MICROSCOPY_enrollment)
103+14
14/117

# qunat micro vs LAMP, color= smear
lamp_micro<- data%>%
  select(1,85,89,81, 91,92,94)

lamp_micro<-lamp_micro%>%
  mutate (combine= case_when(
    UW4 == 'N' & plasma2 == 'N' ~'NN',
    UW4 == 'P' & plasma2 == 'N' ~'PN',
    UW4 == 'P' & plasma2 == 'P' ~'PP',
    UW4 == 'N' & plasma2 == 'P' ~'NP',
    UW4 == 'I' & plasma2 == 'N' ~'IN'))

lamp_micro<-lamp_micro%>%
  mutate_at("ID", str_replace, "AMF-", "")

lamp_micro$ID<- (as.numeric(as.character(lamp_micro$ID)))
lamp_micro<-lamp_micro%>%   
  filter(ID > 117)%>%
  filter(!ID %in% c(126,129,150,172))

lamp_micro$combine <- factor (lamp_micro$combine , levels = c('NN','IN',"PN","PP","NP")) 

lamp_micro$FILARIA_per_40ul<-(as.numeric(as.character(lamp_micro$FILARIA_per_40ul)))

lamp_micro<-lamp_micro%>%
  drop_na(combine)

lamp_micro<-lamp_micro%>%
  mutate(FILARIA_MICROSCOPY_enrollment = case_when(
    FILARIA_MICROSCOPY_enrollment == 'Negative'~ 'Negative',
    FILARIA_MICROSCOPY_enrollment == 'Positive'~ 'Positive',
    FILARIA_MICROSCOPY_enrollment == 'Not tested - no blood smear'~ 'Not tested'))

comb<- lamp_micro %>%
  ggplot(aes(x= combine, y= FILARIA_per_40ul))+
  geom_quasirandom(aes(color=FILARIA_MICROSCOPY_enrollment))+
  scale_color_manual(values = c( 'black','#EDEDED','#B5F7E0'),  name= 'Thin Smear')+
  labs(x = "LAMP Results", y = "Counts", size=10) +
  theme_zlab() +
  aesthetic +
  theme(
    legend.position = 'right',
    legend.title = element_text(size = 10),
    axis.title.x=element_text(size= 10),
    axis.text.x = element_text(angle = 0, hjust = .5,face='plain'),
    panel.border = element_blank()
  )

comb


cowplot<-plot_grid(NULL,smear.plot, NULL, consensus.plot, NULL, plasma.plot,NULL,smear.plot2,NULL,blood.plot2,NULL,plasma.plot2,NULL,heatmap,NULL,comb, labels=c('A','','','','','','B','','','','','','C','','D','',''), ncol=1, align ='v', axis = "lr", rel_heights= c(0.3,1,-0.12,1,-0.12,1.2,0.3,1,-0.12,1,-0.12,1.2,-.12,1.5,0.3,1.8))
cowplot

save_plot("../plots/fig2.pdf", cowplot, base_height = 8, base_width = 10)

