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
    strip.text = ggplot2::element_text(face = "plain", size = 10))
  
#read in data 
demographic_data<- read_csv("../data/demographics.csv")
micro_data<- read_csv("../data/microscopy.csv")
LAMP_data<- read_csv("../data/LAMP_quant.csv")
location<- read_csv("../data/table_location.csv")

###############################
# prep blood LAMP for all individuals 
###############################

Lamp<- LAMP_data%>%
  pivot_longer(cols= c(8:13,19),names_to = "rep", values_to = "result")%>%
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

Lamp<-Lamp%>%
  filter(!rep %in% c('Plasma1','Plasma2','Rep0','Rep1','Rep2','Rep4','Rep5'))%>%
  filter(!ID %in% c(4,126,129,150,172))


#merge dataframes

data<-left_join(demographic_data, LAMP_data)%>%
  left_join(., micro_data)

######################
#Demographic analysis
######################
#tidy and rename data to match across diagnostics 
demo<- data%>%
  mutate_at("ID", str_replace, "AMF-", "")

demo$ID<-(as.numeric(as.character(demo$ID)))

demo<-demo%>%
  pivot_longer(cols= c(78:83,89),names_to = "rep", values_to = "result")%>%
filter(! rep %in% c( 'plasma1','plasma2',"UW1","UW2","UW3","NEB1","NEB23")) 

 demo<-demo%>%
   mutate( result = case_when(
     result == 'I'~ 'NA', 
result == 'N'~ 'Negative', 
result == 'negative'~ 'Negative', 
result == 'P'~ 'Positive', 
result == 'positive'~ 'Positive', 
result == 'no_reulst'~ 'NA', 
result == 'NA'~ 'NA' ))%>%
  filter(result != 'NA')%>%
   filter(!ID %in% c(4,126,129,150,172))
 
#Tables-------------- 
 demo$ID<-(as.numeric(as.character(data$ID))) 
 demo%>%count(FILARIA_MICROSCOPY_enrollment)
 demo%>%count(result)
 
#LAMP 94/(141+94)
 
#micro 30/(186+30+19)

#counts
test.count<-demo%>% group_by(Case_origin ,as.numeric(ID))%>%
  count(result,FILARIA_MICROSCOPY_enrollment)

age_cat%>%group_by(Sex)%>%
  summarise (av_age= mean(Age))
#total 36.7
#F-35
#M-40

######################
#plot for age,sex, mf+

tbl1<-demo%>%
  select(1,6,7,11,83:85,91)

tbl1$Age<-(as.numeric(as.character(tbl1$Age)))

look<-tbl1%>%count(Case_origin,result)
look%>%summarise(sum(n))

#set groups for age ranges
age_cat<-tbl1%>%
  mutate(AGE= case_when(
    Age <33 ~ 'Young_Adult',
    Age < 55 ~ 'Adult',
    Age > 54 ~ 'Mature_Adult'))%>%
  filter(!is.na(AGE))

age_cat$result<-factor(age_cat$result, levels = c("Negative",'Positive'))
age_cat$Sex<-factor(age_cat$Sex, levels = c("Female" , "Male"),
                           labels = c("F" ,"M"))
#plot for LAMP results by age/sex 
demo.plot<-age_cat%>%
  ggplot(aes(x= Sex, y= as.numeric(Age) , color= 'black'))+
  facet_grid(cols=vars(result))+  
  geom_beeswarm(size=1)+
  stat_summary(aes(y = as.numeric(Age)), fun.data=mean_se, colour="magenta", geom="errorbar")+
  #stat_compare_means(method = "t.test")+
  scale_color_manual(values = c("Black")) +
  labs(x = "", y = "Age") +
 theme_zlab() +
  aesthetic+
  theme(
        axis.text.x = element_text(angle = 0, hjust=.5),
        legend.title = element_blank(),
  )
demo.plot

#t-test assumptions 'normality' 
model <- lm(Age ~ Sex, data=age_cat)
lattice::densityplot(~residuals(model), group=Sex, data=age_cat, auto.key=TRUE)

with(age_cat, shapiro.test(Age[Sex == "M"]))
with(age_cat, shapiro.test(Age[Sex == "F"]))
#data does not follow a normal distribution, need to use nonparametric test 

compare_means(Age ~ result, age_cat, group.by = "Sex", method = 'wilcox')
#female pval .026, male pval .07 

#plot histogram
age_cat2<-age_cat%>%
  group_by(Age,Sex,AGE)%>%
  count(Age,result)%>%
  pivot_wider(names_from = result, values_from = n)%>%
  mutate(Positive = ifelse(is.na(Positive), 0,Positive))%>%
  mutate(Negative = ifelse(is.na(Negative), 0,Negative))%>%
  mutate(total= Negative+ Positive)%>%
  mutate(percent= Positive/total)

#histogram of %+LAMP by age/sex
demo.plot2<-age_cat2%>%
  ggplot(aes(x= as.numeric(Age), y= percent*100 , color= 'black'))+
  facet_grid(cols=vars(Sex))+  
  geom_col()+
  stat_summary(aes(y = percent), fun.data=mean_se, colour="red", geom="errorbar")+
  scale_color_manual(values = c("Black")) +
  labs(x = "Age", y = "% Positive") +
  theme_zlab() +
  aesthetic+
  theme(
    axis.title.x=element_text(size= 12),
    axis.text.x = element_text(angle = 0, hjust=.5),
    #legend.position = 'bottom',
    legend.title = element_blank(),
  )

demo.plot2

#grid plot
age_sex_result<-plot_grid(demo.plot,demo.plot2, labels=c('',''),nrow=1,ncol=2, align = 'hv' , rel_widths = c(.8,1))

###############################
#Symptoms
###############################

#filter and rename data
symp.corr2<-demo%>%
  select(1,2,6,7,10,11,14:33,60:68,90,91)%>%
  filter(result == 'Positive')%>%
  pivot_longer(cols= c(9,11:26), names_to="symptoms", values_to = "present")%>%
  filter(present != 'Not reported')%>%
  mutate(present = ifelse(present=="YES" | present== "Positive", 1,0))

symp.corr2$ID<-(as.numeric(as.character( symp.corr2$ID)))

#format data as binary, get % reported 
symp.corr3<-symp.corr2%>%
  filter(ID>117)%>%
  group_by(present)%>%
  count(symptoms)%>%
  pivot_wider(names_from = present, values_from = n)%>%
  rename(Positive='1')%>%
  rename(Negative='0')%>%
  mutate(Positive = ifelse(is.na(Positive), 0,Positive))%>%
  mutate(total= Negative+Positive)%>%
  mutate(percent= Positive/total*100)%>%
  filter(percent != '0')

#set levels and rename 
symp.corr3$symptoms<-factor(symp.corr3$symptoms, levels = c("Fever" , "Weightloss","Jaundice" ,"Headache","Muscle_body_pain","Joint_body pain" ,"Retro_orbital_pain",
                                                          "Chills" ,"Abdominal_pain","Weakness","Skin_rash","Dizziness" ,        
                                                          "Vomit" ,"Diarrhea","Cough","Red_eyes","Anosmia_ageusia" ,  
                                                          "result"),
                           labels = c("Fever" ,"Weightloss","Jaundice" ,"Headache","Muscle pain","Joint pain" ,"Eye pain",
                                      "Chills" ,"Abdominal pain","Weakness","Skin rash","Dizziness" ,        
                                      "Vomit" ,"Diarrhea","Cough","Red eyes","Anosmia/ageusia" ,  
                                      "M. ozzardi"))
#prep data for plotting 
symp.corr3$ymax = cumsum(symp.corr3$percent)
symp.corr3$ymin = c(0, head(symp.corr3$ymax, n=-1)) 
symp.corr3$labelPosition <- (symp.corr3$ymax + symp.corr3$ymin) / 2
symp.corr3$label <- paste0(symp.corr3$symptoms, symp.corr3$percent,"% ")

#plot for positive symptoms +LAMP
donut<-ggplot(symp.corr3, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=symptoms)) +
  geom_rect() +
  coord_polar(theta="y") + 
  scale_fill_manual(name ='% of LAMP positive',labels= c("Fever 18.2% " ,"Headache 25.6% ","Muscle pain 10% ","Joint pain 5.1% " ,
                             "Eye pain 5% ","Chills 7.5% " ,"Abdominal pain 12.5% ",
                             "Weakness 12.5% " ,"Dizziness 7.5% ", "Cough 10% ","Red eyes 2.5% ","Anosmia/ageusia 5% "),
                    values = c( 'grey','#537380','#889889','#cde5f9','#88a2b9','#677e8e',
                                                '#81a9ad', '#33454e','#5A5A5A','#E1D9D1','#BCB099','#B2BEB5'))+
  xlim(c(-1, 4)) +
  aesthetic+
  theme_void() +
  theme(legend.title= element_text(),
    legend.position = "right",
    legend.key.size = unit(.5,"line"))+
    guides(fill = guide_legend(ncol = 2))
    

donut
######################
#Negative symptoms  
######################
symp.corr2.neg<-demo%>%
  select(1,2,6,7,10,11,14:33,60:68,90,91)%>%
  filter(result == 'Negative')%>%
  pivot_longer(cols= c(9,11:26), names_to="symptoms", values_to = "present")%>%
  filter(present != 'Not reported')%>%
  mutate(present = ifelse(present=="YES" | present== "Positive", 1,0))

symp.corr2.neg$ID<-(as.numeric(as.character( symp.corr2.neg$ID)))

#format data as binary, get % not reported 
symp.corr4<-symp.corr2.neg%>%
  filter(ID>117)%>%
  group_by(present)%>%
  count(symptoms)%>%
  pivot_wider(names_from = present, values_from = n)%>%
  rename(Positive='1')%>%
  rename(Negative='0')%>%
  mutate(Positive = ifelse(is.na(Positive), 0,Positive))%>%
  mutate(total= Negative+Positive)%>%
  mutate(percent= Positive/total*100)%>%
  filter(percent != '0')

#set levels and rename 
symp.corr4$symptoms<-factor(symp.corr4$symptoms, levels = c("Fever" , "Weightloss","Jaundice" ,"Headache","Muscle_body_pain","Joint_body pain" ,"Retro_orbital_pain",
                                                            "Chills" ,"Abdominal_pain","Weakness","Skin_rash","Dizziness" ,        
                                                            "Vomit" ,"Diarrhea","Cough","Red_eyes","Anosmia_ageusia" ,  
                                                            "result"),
                            labels = c("Fever" ,"Weightloss","Jaundice" ,"Headache","Muscle pain","Joint pain" ,"Eye pain",
                                       "Chills" ,"Abdominal pain","Weakness","Skin rash","Dizziness" ,        
                                       "Vomit" ,"Diarrhea","Cough","Red eyes","Anosmia/ageusia" ,  
                                       "M. ozzardi"))
#filter for list in positive LAMP plot
symp.corr4<-symp.corr4%>%
  filter (!symptoms %in% c('Weightloss','Skin rash','Jaundice'))

#prep data for plottnig 
symp.corr4$ymax = cumsum(symp.corr4$percent)
symp.corr4$ymin = c(0, head(symp.corr4$ymax, n=-1)) 
symp.corr4$labelPosition <- (symp.corr4$ymax + symp.corr4$ymin) / 2
symp.corr4$label <- paste0(symp.corr4$symptoms, symp.corr4$percent,"% ")

#plot for negative symptoms +LAMP
donut.neg<-ggplot(symp.corr4, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=symptoms)) +
  geom_rect() +
  coord_polar(theta="y") + 
  scale_fill_manual(name ='% of LAMP negative',labels= c("Fever 11.1% " ,"Headache 27.5% ","Muscle pain 20.2% " , 
                                                         "Joint pain 15% ","Eye pain 7.7% ","Chills 6.3% " , "Abdominal pain 8.8% ",
                              "Weakness 11.3% " ,"Dizziness 5.1% ","Cough 8.8% ","Red eyes 8.9% ","Anosmia/ageusia 5% "), 
                    values = c( 'grey','#537380','#889889','#cde5f9','#88a2b9','#677e8e',
                                      '#81a9ad', '#33454e','#5A5A5A','#E1D9D1','#BCB099','#B2BEB5'))+
  xlim(c(-1, 4)) +
  aesthetic+
  theme_void() +
  theme(legend.title= element_text(),
        legend.position = "right",
        legend.key.size = unit(.5,"line"))+
  guides(fill = guide_legend(ncol = 2))

donut.neg

#plot grid 
symp.donuts<-plot_grid(donut,donut.neg)

########################################
#Report LAMP/Coinfections/Symptoms
######################################## 
 corr.dis<- demo%>%
   select(1,2,6,7,10,11,14:33,60:68,90,91)

 corr.test<-corr.dis%>%
   pivot_wider(names_from = Diagnosis, values_from = `Diagnosed_dengue_malaria,_yellowfever_chikunguña_zika_COVID-19`)

 corr.test= unite(corr.test, malaria,c(37,38,40,41,45,46))
 corr.test= unite(corr.test, dengue,c(38,40,41,43:45,48,49))
 corr.test= unite(corr.test, covid,c(39,42,48,49))

 corr.test2<-corr.test%>%
   select(1,37:39)

 corr.test2$ID<-(as.numeric(as.character(corr.test2$ID)))

 #make binary 
 corr.test2<-corr.test2%>%
  mutate(malaria = ifelse(grepl("YES", corr.test2$malaria), 1,0))%>%
  mutate(dengue = ifelse(grepl("YES", corr.test2$dengue), 1,0))%>%
  mutate(covid = ifelse(grepl("YES", corr.test2$covid), 1,0))%>%
   filter(ID>186)
 
######

 demo$ID<-(as.numeric(as.character(demo$ID)))
 
  diseases<-demo%>%
   select(1,60:68,90,91)%>%
   pivot_longer(cols=c(2:10,12), names_to = 'symptoms', values_to= 'present')%>%
   select(1,3,4)
 
 diseases<- diseases%>%
   filter(ID>117)%>%
   filter(!present %in% c("Not tested - no blood smear" ,"not apply - no sample" ,
                          "no apply","insufficient additional sample - not tested" ,"not apply"))%>%
   mutate(present = ifelse(present=="Positivo" | present== "Positive", 1,0))%>%
   mutate(type='Serology')

#set levels and rename 
diseases$symptoms<-factor(diseases$symptoms, levels = c("MALARIA_MICROSCOPY","HIV_Ag/Ab","ANTI_HBS_Ab","HCV_Ab" ,"HBS_Ag",
                                                             "COV_IgM" ,'malaria','IgM_Dengue_RDT','IgG_Dengue_RDT', 'result'),
                             labels = c('Malaria micro',"HIV Ag/Ab","Hep B Ab", 'Hep C Ab','Hep B Ag', 'COVID IgM', 
                                        'Malaria sr', 'Dengue IgM','Dengue IgG','M. ozzardi'))

symp.corr<- demo%>%
   select(1,2,6,7,10,11,14:33,60:68,90,91)%>%
   pivot_longer(cols= c(9,11:26,37), names_to="symptoms", values_to = "present")

#set levels and rename 
symp.corr$symptoms<-factor(symp.corr$symptoms, levels = c("Fever" , "Weightloss","Jaundice" ,"Headache","Muscle_body_pain","Joint_body pain" ,"Retro_orbital_pain",
                                                            "Chills" ,"Abdominal_pain","Weakness","Skin_rash","Dizziness" ,        
                                                            "Vomit" ,"Diarrhea","Cough","Red_eyes","Anosmia_ageusia" ,  
                                                            "result"),
                            labels = c("Fever" ,"Weightloss","Jaundice" ,"Headache","Muscle pain","Joint pain" ,"Eye pain",
                                       "Chills" ,"Abdominal pain","Weakness","Skin rash","Dizziness" ,        
                                       "Vomit" ,"Diarrhea","Cough","Red eyes","Anosmia/ageusia" ,  
                                       "M. ozzardi"))

#prep data for plotting 
symp.corr$ID<-(as.numeric(as.character( symp.corr$ID)))

symp.corr<-symp.corr%>%
   filter(present != 'Not reported')%>%
   mutate(present = ifelse(present=="YES" | present== "Positive", 1,0))%>%
   filter(ID>117)%>%
  select(1, 20,21)%>%
  mutate(type='Symptom')

combined<-symp.corr%>%
    rbind(.,diseases)%>%
    mutate(type = ifelse(symptoms=="M. ozzardi",  'LAMP', type))
  
#plot +/- for symptoms and serology, blood LAMP 
comb.plot<- combined%>%
  filter(symptoms !='result')%>%
   ggplot(aes(x= symptoms, y= as.character(ID), fill= factor(present)))+
   facet_grid(~type,scales='free_x',space='free')+
   geom_tile(width=.9,height=.9)+
   scale_fill_manual(values = c( '#f7f7f7',"#FFCFA4"))+
   scale_y_discrete( breaks= seq(118,244,126),expand = c(0, 0)) +
   ylab('ID')+
   aesthetic +
     theme(  
       strip.background = element_rect(color='white', fill='white'),
       axis.text.x = element_text(angle = 65, hjust = 1,face='plain'),
       axis.text.y = element_text(face = "plain"),
       legend.position = "",
       panel.border = element_rect(color="black", fill=NA))
       # panel.background = element_rect(color = 'white'))
comb.plot 

##################### 
#infections 
#####################

dis.corr<-demo%>%
  select(1,60:68,90,91)%>%
   filter(result == 'Positive')%>%
   pivot_longer(cols= c(2:10), names_to="disease", values_to = "present")%>%
   filter(present != 'Not reported')%>%
   mutate(present = ifelse(present=="YES" | present== "Positive", 1,0))
 
 symp.corr2$ID<-(as.numeric(as.character( symp.corr2$ID)))

 #format data as binary, get % reported  
dis.corr3<-dis.corr%>%
   filter(ID>117)%>%
   group_by(present)%>%
   count(disease)%>%
   pivot_wider(names_from = present, values_from = n)%>%
   rename(Positive='1')%>%
   rename(Negative='0')%>%
   mutate(Positive = ifelse(is.na(Positive), 0,Positive))%>%
   mutate(total= Negative+Positive)%>%
   mutate(percent= Positive/total*100)%>%
   filter(percent != '0')

#set levels and rename 
dis.corr3$disease<-factor(dis.corr3$disease, levels = c("MALARIA_MICROSCOPY","HIV_Ag/Ab","ANTI_HBS_Ab","HCV_Ab" ,"HBS_Ag",
                                                        "COV_IgM" ,'malaria','IgM_Dengue_RDT','IgG_Dengue_RDT','result'),
                          labels = c('Malaria micro',"HIV Ag/Ab","Hep B Ab", 'Hep C Ab','Hep B Ag', 'COVID IgM', 
                                     'Malaria sr', 'Dengue IgM','Dengue IgG','M. ozzardi'))
#prep data for plotting 
 dis.corr3$ymax = cumsum(dis.corr3$percent)
 dis.corr3$ymin = c(0, head(dis.corr3$ymax, n=-1)) 
 dis.corr3$labelPosition <- (dis.corr3$ymax + dis.corr3$ymin) / 2
 dis.corr3$label <- paste0(dis.corr3$disease, dis.corr3$percent,"% ")
 
 #plot of positive serology results for +LAMP 
 dis.donut<-ggplot(dis.corr3, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=disease)) +
   geom_rect() +
   coord_polar(theta="y") + 
   scale_fill_manual(name ='% of LAMP positive',labels=c("Hep B Ab 47.5% ","Hep C Ab 2.5% " ,"Hep B Ag 2.5% ","COVID IgM 10% " , "Dengue IgM 20%","Dengue IgG 37.5% " ),
                     values = c( 'grey','#889889','#cde5f9','#88a2b9','#677e8e',
                                 '#81a9ad', '#33454e', '#537380','#5A5A5A','#36454F','#536878'))+
    xlim(c(-1, 4)) +
   aesthetic+
   theme_void() +
   theme( legend.title = element_text(),
     legend.position = "left",
     legend.key.size = unit(.5,"line"))
 
 dis.donut
################# 
#negative cases 
 
 dis.corr.neg<-demo%>%
   select(1,60:68,90,91)%>%
   filter(result == 'Negative')%>%
   pivot_longer(cols= c(2:10), names_to="disease", values_to = "present")%>%
   filter(present != 'Not reported')%>%
   mutate(present = ifelse(present=="YES" | present== "Positive", 1,0))
 
 symp.corr2$ID<-(as.numeric(as.character( symp.corr2$ID)))
 
 #format data as binary, get % reported   
 dis.corr4<-dis.corr.neg%>%
   filter(ID>117)%>%
   group_by(present)%>%
   count(disease)%>%
   pivot_wider(names_from = present, values_from = n)%>%
   rename(Positive='1')%>%
   rename(Negative='0')%>%
   mutate(Positive = ifelse(is.na(Positive), 0,Positive))%>%
   mutate(total= Negative+Positive)%>%
   mutate(percent= Positive/total*100)%>%
   filter(percent != '0')
 
 #set levels and rename 
 dis.corr4$disease<-factor(dis.corr4$disease, levels = c("MALARIA_MICROSCOPY","HIV_Ag/Ab","ANTI_HBS_Ab","HCV_Ab" ,"HBS_Ag",
                                                         "COV_IgM" ,'malaria','IgM_Dengue_RDT','IgG_Dengue_RDT','result'),
                           labels = c('Malaria micro',"HIV Ag/Ab","Hep B Ab", 'Hep C Ab','Hep B Ag', 'COVID IgM', 
                                      'Malaria sr', 'Dengue IgM','Dengue IgG','M. ozzardi'))

 #prep data for plotting 
 dis.corr4$ymax = cumsum(dis.corr4$percent)
 dis.corr4$ymin = c(0, head(dis.corr4$ymax, n=-1)) 
 dis.corr4$labelPosition <- (dis.corr4$ymax + dis.corr4$ymin) / 2
 dis.corr4$label <- paste0(dis.corr4$disease, dis.corr4$percent,"% ")
 
#plot of negative serology results for +LAMP 
 dis.donut.neg<-ggplot(dis.corr4, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=disease)) +
   geom_rect() +
   coord_polar(theta="y") + 
   scale_fill_manual(name ='% of LAMP negative',labels=c("Hep B Ab 37.5% ","Hep C Ab 6.25% " ,"Hep B Ag 3.75% ",  "COVID IgM 7.5%","Dengue IgM 15% ","Dengue IgG 38.75% "),
                     values = c( 'grey','#889889','#cde5f9','#88a2b9','#677e8e',
                    '#81a9ad', '#33454e', '#537380','#5A5A5A','#36454F','#536878'))+

   xlim(c(-1, 4)) +
   aesthetic+
   theme_void() +
   theme( legend.title = element_text(),
          legend.position = "left",
          legend.key.size = unit(.5,"line"))
 
 dis.donut.neg

 dis.donuts<-plot_grid(dis.donut,dis.donut.neg)

###################
 #GLM infection/symptoms 
##################
 
#get frequence for pos/neg LAMP by symptom
symp.Lpos<-symp.corr3%>%
   select(1,5)%>%
   mutate(percent = percent/100)%>%
   pivot_wider(names_from = symptoms, values_from = percent)%>%
   mutate(LAMP= 'positive')

symp.Lneg<-symp.corr4%>%
  select(1,5)%>%
  mutate(percent = percent/100)%>%
  pivot_wider(names_from = symptoms, values_from = percent)%>%
  mutate(LAMP= 'negative')


#GLM for symtpoms that differ in freq 

symp.corr<- demo%>%
  select(1,2,6,7,10,11,14:33,60:68,90,91)%>%
  pivot_longer(cols= c(9,11:26,37), names_to="symptoms", values_to = "present")

symp.corr$ID<-(as.numeric(as.character( symp.corr$ID)))

symp.corr<-symp.corr%>%
  filter(present != 'Not reported')%>%
  mutate(present = ifelse(present=="YES" | present== "Positive", 1,0))%>%
  filter(ID>117)%>%
  select(1:4, 20,21)%>%
  mutate(type='symptom')

symp.corr.glm<-symp.corr%>%
  pivot_wider(names_from = symptoms, values_from = present)

colnames(symp.corr.glm) <- gsub(" ", ".", colnames(symp.corr.glm))

symp.corr$Age<-(as.numeric(as.character( symp.corr$Age)))

#creat data frame needed for GLM 
symp.GLM<-symp.corr.glm%>%
  mutate(A=NA)%>%
  mutate(V=NA)%>%
  mutate(J=NA)%>%
  mutate(M=NA)%>%
  mutate(W=NA)%>%
  filter(!Age %in% c('Not reported', NA))%>%
  mutate(AGE= case_when(
    Age <33 ~ 'Young_Adult',
    Age < 55 ~ 'Adult',
    Age > 54 ~ 'Mature_Adult'))%>%
  mutate(result = case_when(
    result == 1~ 'positive',
    result == 0~ 'negative' 
  ))%>%
  mutate( opposite.f = case_when(
    Fever == 1~ 0, 
    Fever== 0~ 1))%>%
  mutate(V=cbind(Fever,opposite.f))%>%
  mutate( opposite.j = case_when(
    Joint_body.pain == 1~ 0, 
    Joint_body.pain== 0~ 1))%>%
  mutate(J=cbind(Joint_body.pain,opposite.j))%>%
  mutate( opposite.a = case_when(
    Abdominal_pain == 1~ 0, 
    Abdominal_pain== 0~ 1))%>%
  mutate(A=cbind(Abdominal_pain,opposite.a))%>%
  mutate( opposite.m = case_when(
    Muscle_body_pain == 1~ 0, 
    Muscle_body_pain== 0~ 1))%>%
  mutate(M=cbind(Muscle_body_pain,opposite.m))

symp.GLM$Age<-(as.numeric(as.character( symp.GLM$Age)))

#set factored list
symp.GLM$AGE <- factor(symp.GLM$AGE, levels = c("Young_Adult","Adult","Mature_Adult"))
symp.GLM$Sex <- factor(symp.GLM$Sex, levels = c("Male","Female"))

#GLMs for variables A J V M 

#abdominal pain A
(symp.corr.glm <- glm(A ~ result+AGE+Sex, binomial,
                  data = symp.GLM))  

(summary(symp.corr.glm ))


anova(symp.corr.glm , test = "Chi")

#Joint pain J
(symp.corr.glmJ <- glm(J ~ result+AGE+Sex, binomial,
                      data = symp.GLM))  

(summary(symp.corr.glmJ ))

anova(symp.corr.glmJ , test = "Chi")
Anova(symp.corr.glmJ, type = 3)


#Fever V
(symp.corr.glmV <- glm(V ~ result+AGE+Sex, binomial,
                      data = symp.GLM))  

(summary(symp.corr.glmV ))

anova(symp.corr.glmV , test = "Chi")

#muscle M
(symp.corr.glmM <- glm(M ~ result+AGE+Sex, binomial,
                      data = symp.GLM))  

(summary(symp.corr.glmM ))

anova(symp.corr.glmM , test = "Chi")
Anova(symp.corr.glmM, type = 3)

#count reported symptoms by sex to get subset sample sizes
symp.GLM%>%count(Joint_body.pain,Sex)
symp.GLM%>%count(Muscle_body_pain,Sex)
symp.GLM%>%count(Sex)

######################################
#GLM for serology that differ in freq 
dis.corr<-demo%>%
  select(1,6,7,60:68,90,91)%>%
  pivot_longer(cols= c(4:12), names_to="disease", values_to = "present")%>%
  filter(present != 'Not reported')%>%
  filter(ID>117)%>%
  mutate(present = ifelse(present=="YES" | present== "Positive", 1,0))

dis.corr.glm<-dis.corr%>%
  pivot_wider(names_from = disease, values_from = present)

colnames(dis.corr.glm) <- gsub(" ", ".", colnames(dis.corr.glm))

dis.corr.glm$Age<-(as.numeric(as.character( dis.corr.glm$Age)))

#create data frame needed for GLM 
dis.GLM<-dis.corr.glm%>%
  mutate(B=NA)%>%
  mutate(C=NA)%>%
  filter(!Age %in% c('Not reported', NA))%>%
  mutate(AGE= case_when(
    Age <33 ~ 'Young_Adult',
    Age < 55 ~ 'Adult',
    Age > 54 ~ 'Mature_Adult'))%>%
  mutate( opposite.B = case_when(
    ANTI_HBS_Ab == 1~ 0, 
    ANTI_HBS_Ab == 0~ 1))%>%
  mutate(B=cbind(ANTI_HBS_Ab ,opposite.B))%>%
  mutate(opposite.C = case_when(
    HCV_Ab == 1~ 0, 
    HCV_Ab == 0~ 1))%>%
  mutate(C=cbind(HCV_Ab ,opposite.C))

dis.GLM$Age<-(as.numeric(as.character( dis.GLM$Age)))

#set factored list
dis.GLM$AGE <- factor(dis.GLM$AGE, levels = c("Young_Adult","Adult","Mature_Adult"))
dis.GLM$Sex <- factor(dis.GLM$Sex, levels = c("Male","Female"))

#variable B C 

#Hep B ab
(dis.corr.glmB <- glm(B ~ result+AGE+Sex, binomial,
                      data = dis.GLM))  

(summary(dis.corr.glmB))

anova(dis.corr.glmB , test = "Chi")
Anova(dis.corr.glmB, type = 3)

#Hep C ab 
(dis.corr.glmC <- glm(C ~ result+AGE+Sex, binomial,
                      data = dis.GLM))  

(summary(dis.corr.glmC))

anova(dis.corr.glmC , test = "Chi")
Anova(dis.corr.glmC, type = 3)

################
# map--------------------------------------------------- 
###############

 library(tidyverse)
 library(osmdata)
 library(sf)
 library(ggmap)
 library(cowplot)
 library(viridis)
 library(magick)
 library(pdftools)
 library(grImport2)
 library(here)
 
 let_bb <- c(
   left = -70.4,
   bottom = -4.5,
   right = -69.6,
   top = -3.8
 )
 
 #borders <- getbb("Leticia Colombia")%>%
 borders <- let_bb %>%
   opq()%>%
   add_osm_feature(key = "admin_level", 
                   value = "2") %>%
   osmdata_sf()
 
 #countries <- getbb("Leticia Colombia")%>%
 countries <- let_bb %>%
   opq()%>%
   add_osm_feature(key = "place", 
                   value = c("country")) %>%
   osmdata_sf()
 
 #cities <- getbb("Leticia Colombia")%>%
 cities <- let_bb %>%
   opq()%>%
   add_osm_feature(key = "place", 
                   value = c("city")) %>%
   osmdata_sf()
 
 #towns <- getbb("Leticia Colombia")%>%
 towns <- let_bb%>%
   opq()%>%
   add_osm_feature(key = "place", 
                   value = c("town")) %>%
   osmdata_sf()
 
 #villages <- getbb("Leticia Colombia")%>%
 villages <- let_bb%>%
   opq()%>%
   add_osm_feature(key = "place", 
                   value = c("village")) %>%
   osmdata_sf()
 
 #streets <- getbb("Leticia Colombia") %>%
 streets <- let_bb %>%
   opq() %>%
   add_osm_feature(key = "highway", 
                   value = c("motorway", "primary", 
                             "secondary", "tertiary")) %>%
   osmdata_sf()
 
 #small_streets <- getbb("Leticia Colombia")%>%
 small_streets <- let_bb%>%
   opq()%>%
   add_osm_feature(key = "highway", 
                   value = c("residential", "living_street",
                             "unclassified",
                             "service", "footway")) %>%
   osmdata_sf()
 
 #river <- getbb("Leticia Colombia")%>%
 river <- let_bb%>%
   opq()%>%
   add_osm_feature(key = "waterway", value = "river") %>%
   osmdata_sf()
 
 #lakes <- getbb("Leticia Colombia")%>%
 lakes <- let_bb%>%
   opq()%>%
   add_osm_feature(key = "natural", value = "water") %>%
   osmdata_sf()
 
 zoom_to <- c(-4.21, -69.95)  # Leticia
 
 zoom_level <- 9
 
 lon_span <- 360 / 2^zoom_level
 lat_span <- 360 / 2^zoom_level
 
 lon_bounds <- c(zoom_to[1] - lon_span / 2, zoom_to[1] + lon_span / 2)
 lat_bounds <- c(zoom_to[2] - lat_span / 2, zoom_to[2] + lat_span / 2)
 
 
 #load in survey location data 
 setwd<-here()
 location<- read_csv("../data/table_location.csv")
 
 #read in data 
 demographic_data<- read_csv("../data/demographics.csv")
 micro_data<- read_csv("../data/microscopy.csv")
 LAMP_data<- read_csv("../data/LAMP_quant.csv")
 location<- read_csv("../data/table_location.csv")
 
 #tidy data and rename 
 
 Lamp<- LAMP_data%>%
   pivot_longer(cols= c(8:13,19),names_to = "rep", values_to = "result")%>%
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
 
 Lamp<-Lamp%>%
   filter(!rep %in% c('Plasma1','Plasma2','Rep0','Rep1','Rep2','Rep4','Rep5'))
 
 Lamp%>% count(result)
 
 
 #map features -----------------------------------------------------
 
 pop_data<- left_join(demographic_data,location)%>%
   filter(GPS!="Not Report")
 
 pop_data<-pop_data%>%
   select(1,2,11,72)%>%
   mutate_at("ID", str_replace, "AMF-", "")
 
 pop_data$ID<- (as.numeric(as.character(pop_data$ID)))
 
 
 data_bubble<-left_join(pop_data,Lamp)%>%
   select(1:4, 18,19)%>%
   separate(col=GPS, into= c('lat', 'long'), sep= ',')
 
 tbl<-data_bubble%>% group_by(Case_origin)%>%
   count(result)
 
 table<-data_bubble%>% count(Case_origin)%>%
   rename (total=n)
 
 table<- left_join(tbl, table)
 
 table <- table %>%
   filter(result== "Positive")%>%
   mutate(pos_rate= n/total)
 
 table <- table %>%
   select(1,4,5)
 
 data_bubble<-left_join(data_bubble,table)
 
 data_bubble$lat<-(as.numeric(as.character( data_bubble$lat)))
 data_bubble$long<-(as.numeric(as.character(data_bubble$long)))
 
 data_bubble<- data_bubble%>%
   mutate(total = ifelse(is.na(total), 1,total))%>%
   mutate(pos_rate = ifelse(is.na(pos_rate),0,pos_rate))
 
 data_bubble<-data_bubble%>%
   select(3:5,8,9)%>%
   distinct()
 
 #plot 
 map <- data_bubble%>%
   ggplot() +
   geom_sf(data = borders$osm_lines,
           inherit.aes = FALSE,
           color = "black",
           size = 1,
           alpha = 1) +
   geom_sf(data = lakes$osm_multipolygons,
           inherit.aes = FALSE,
           fill = "#0fb0f5",
           color = "#0fb0f5",
           size = 0.51,
           alpha = .5) +
   geom_sf(data = river$osm_lines,
           inherit.aes = FALSE,
           fill = "#0fb0f5",
           color = "#0fb0f5",
           size = 0.15,
           alpha = .5) +
 annotate("text", x=-70.2, y=-3.5, label = "Colombia", size = 4) +
   annotate("text", x=-70.5, y=-4, label = "Peru", size = 4) +
   annotate("text", x=-69.8, y=-4.45, label = "Brazil", size = 4) +
   coord_sf(
     xlim = c(-70.85,-69.35), #lat_bounds,
     ylim = c(-4.65,-2.65), #lon_bounds,
     expand = TRUE) +
   geom_point(data=data_bubble,aes(x=long, y=lat, size=total, fill= pos_rate), shape=21)+
   scale_shape_identity()+
   scale_fill_gradient2(low='#fdc78d', mid='#e45563',high='#2c1160', midpoint = .6)+
   scale_size_continuous(breaks = c(5,25,50,100))+
   guides(alpha=FALSE)+
   labs(fill="Prevalence", size= "Sample Size")+
   aesthetic+
   theme(
     panel.border = element_blank(),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     panel.background = element_rect(colour = "black", size=1),
     axis.title.y = element_blank(),
     axis.text.x = element_text(angle = 65, hjust = 1,face='plain' ),
     legend.key = element_rect(fill = "white"),
     plot.background = element_rect(fill = "#FFFFFF"),
     axis.title = element_blank()
   ) +
   
   NULL
 map
 
top<-plot_grid(map,age_sex_result, labels=c('A','B'),nrow=1,ncol=2, axis = 'tb', rel_widths = c(1,1))

bottom<-plot_grid(dis.donut,donut,dis.donut.neg,donut.neg, labels=c('D',''),vjust= -.25,nrow=2,ncol=2, axis = 'tb', rel_widths = c(.72,1,.72,1))

topfig3<-plot_grid(top,comb.plot,labels=c('','C'),nrow=2,ncol=1,rel_widths = c(1,2), rel_heigths = c(1.2,1))

fig3<-plot_grid(topfig3,bottom,nrow=2,ncol=1, axis= 'r',rel_heights  = c(2,1), rel_widths = c(1,1))

save_plot("../plots/fig3.pdf", fig3, base_height = 10.2, base_width = 8.8)
 
