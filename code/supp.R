# data wrangling
library(tidyverse)
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


library(ggcorrplot)
library(car)
library(leaflet)
library(htmlwidgets)
library(mapproj)
library(maps)

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

#plot for positive lamp 
blood.plot<-blood%>%
  ggplot(aes(x= ID, y= rep, fill= result))+
  geom_tile(colour="white",size=0.25)+
  #facet_wrap(assay_type~sample_type , ncol=1, scales = "free_y") +
  scale_fill_manual(values = c( '#EDEDED','#f4c33a',"#FFCFA4"))+
  labs(x = "", y = expression ("L"[b]), size=10) +
  scale_x_continuous(limits= c(0,118), breaks= seq(1,117,5),expand = c(0, 0))+
  scale_y_discrete(position= 'right',expand=c(0,0))+
  theme_zlab() +
  aesthetic +
  theme(
    axis.text.y= element_blank(),
    axis.ticks.y= element_blank(),
    legend.title = element_text(size = 10),
    axis.title.x=element_text(size= 10),
    axis.text.x = element_text(angle = 45, hjust = .5,face='plain')
  )
blood.plot

######
#SQ-LAMP heatmap (hp)
#######
Lamphm<- Lamp%>%
  mutate(NEB1_cq = na_if(NEB1_cq,0))%>%
  mutate(NEB1_cq = ifelse(is.na(NEB1_cq), 150,NEB1_cq))%>%
  filter(ID != 4)%>%
  mutate(Tt= 21.1*NEB1_cq)%>%
  #filter contaminated DNA based on consensus and replicate sq_LAMP data
  mutate(Tt=ifelse (NEB1_cq == 18.80, 52.75, Tt))%>%
  mutate(Tt=ifelse (NEB1_cq == 24.02, ((21.1*116.35)), Tt))%>%
  mutate(Tt=ifelse (NEB1_cq == 69.93, ((21.1*145.24)), Tt))

#sq-LAMP 
heatmap <- Lamphm%>%
  ggplot( scale="row",aes( x= ID, y= "", fill= Tt/60))+
  geom_tile(size=0.25) + 
  scale_fill_gradient2(high= "#4D6291",mid= '#b2abd2', low = "#FDA50F", midpoint =((21.1*75)/60))+
  scale_x_continuous(limits= c(0,118), breaks= seq(1,117,5),expand = c(0, 0))+
  scale_y_discrete(position= 'right',expand = c(0, 0)) + 
  labs(x = "",  y = expression ("L"[sq]), fill = "Tt (mins)") +
  theme_zlab() +
  aesthetic+
  theme(
    legend.position = 'right',
    legend.title = element_text(size = 10),
    axis.title.x=element_text(size= 10),
    axis.text.x = element_text(angle = 45, hjust = .5,face='plain'),
  )
heatmap


# LAMP blood and plasma results vs sq LAMP
data<-left_join(demographic_data, LAMP_data)%>%
  left_join(., micro_data)

lamp_micro<- data%>%
  select(1,85,86,89, 90,92)

lamp_micro<-lamp_micro%>%
  mutate (combine= case_when(
    consensus == 'negative' & plasma1 == 'N' ~'NN',
    consensus == 'positive' & plasma1 == 'N' ~'PN',
    consensus == 'positive' & plasma1 == 'P' ~'PP',
    consensus == 'negative' & plasma1 == 'P' ~'NP'))%>%
  mutate(Tt= (21.1*NEB1_cq)/60)  %>%
  #filter contaminated DNA based on consensus and replicate sq_LAMP data
  mutate(Tt=ifelse (NEB1_cq == 18.80, 0, Tt))%>%
  mutate(Tt=ifelse (NEB1_cq == 24.02, ((21.1*116.35)/60), Tt))%>%
  mutate(Tt=ifelse (NEB1_cq == 69.93, ((21.1*145.24)/60), Tt))%>%
  drop_na(combine)

lamp_micro<-lamp_micro%>%
  mutate_at("ID", str_replace, "AMF-", "")

lamp_micro$ID<- (as.numeric(as.character(lamp_micro$ID)))
lamp_micro<-lamp_micro%>%  
  filter(ID != 4)%>%
  filter(ID < 118)

lamp_micro$combine <- factor (lamp_micro$combine , levels = c('NN',"PN","PP","NP")) 

lamp_micro<-lamp_micro%>%
  mutate(FILARIA_MICROSCOPY_enrollment = case_when(
    FILARIA_MICROSCOPY_enrollment == 'Negative'~ 'Negative',
    FILARIA_MICROSCOPY_enrollment == 'Positive'~ 'Positive',
    FILARIA_MICROSCOPY_enrollment == 'Not tested - no blood smear'~ 'Not tested'))

#Plot sq_LAMP by LAMP results, color=smear 
comb<- lamp_micro %>%
  ggplot(aes(x= combine, y= as.numeric(Tt)))+
  geom_quasirandom(aes(color=FILARIA_MICROSCOPY_enrollment))+
  scale_y_continuous(limits= c(-2,60), breaks= seq(0,60,20), expand= c(0,0))+
  scale_color_manual(values = c( 'black','#EDEDED','#B5F7E0'),  name= 'Smear')+
  labs(x = "LAMP Results", y = "Tt (mins)", size=10) +
  geom_hline(yintercept=c(1,30), linetype="dashed")+
  theme_zlab() +
  aesthetic +
  theme( 
    legend.position = 'right',
    legend.title = element_text(size = 10),
    axis.title.x=element_text(size= 10),
    axis.text.x = element_text(angle = 0, hjust=0.5)
  )

comb

#figure plot

cowplot<-plot_grid(NULL,blood.plot, NULL,heatmap, NULL, comb, labels=c('A','','B','','C'), ncol=1, align ='v', axis = "lr", rel_heights= c(0.3,1,0.3,1,0.3,2))
cowplot


save_plot("../plots/sup_fig1.pdf", cowplot, base_height = 6, base_width = 8)

