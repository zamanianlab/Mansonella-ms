library(tidyverse)
library(grid)
library(mgcv)
library(lubridate)
library(conflicted)
library(ggbeeswarm)
library(ggpubr)
library(ggtext)
library(dplyr)
library(cowplot)
library(magick)
library(pdftools)
library(grImport2)
library(broom)
library(ggcorrplot)
library(here)
library (betareg)
library(performance)
library(ZamanianLabThemes)

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
#Prep daata blood LAMP for all individuals 
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



data<-left_join(demographic_data, LAMP_data)%>%
  left_join(., micro_data)

######################
#Demographic analysis
######################

demo<- data%>%
  #filter(consensus == "positive")%>%
  mutate_at("ID", str_replace, "AMF-", "")

demo$ID<-(as.numeric(as.character(demo$ID)))

demo<-demo%>%
  mutate_at("ID", str_replace, "AMF-", "")

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

#############
#GLM demographics 
#############
GLM.data<- demo%>%
  select(1,2,6:11,16,18:33,60:68,87,90,91)%>%
  mutate(y=NA)

GLM.data$ID<-(as.numeric(as.character(GLM.data$ID)))
GLM.data$FILARIA_per_40ul<-(as.numeric(as.character(GLM.data$FILARIA_per_40ul)))
GLM.data$Age<-(as.numeric(as.character(GLM.data$Age)))

GLM.data<-GLM.data%>%
  mutate(result = ifelse(result=="Positive", 1,0))%>%
  filter(ID>117)%>%
  filter(Age!= 'Not reported')

#make data frame for GLMs 
glm.data<-GLM.data%>%
  mutate( opposite = case_when(
    result == 1~ 0, 
    result == 0~ 1))%>%
  filter(!FILARIA_per_40ul%in%c('No sample','No sample for AMF-110'))%>%
  filter(!Age%in%c('Not reported'))%>%
  mutate(y=cbind(result,opposite))%>%
  mutate(AGE= case_when(
    Age <33 ~ 'Young_Adult',
    Age < 55 ~ 'Adult',
    Age > 54 ~ 'Mature_Adult'))%>%
  mutate(titer= case_when (
    FILARIA_per_40ul == 0 ~ "none",
    FILARIA_per_40ul < 3 ~'Low',
    FILARIA_per_40ul < 6 ~ 'Med',
    FILARIA_per_40ul > 5 ~ 'High'
  ))%>%
  filter(!is.na(AGE))%>%
  drop_na()

#set factored lists
glm.data$AGE <- factor(glm.data$AGE, levels = c("Young_Adult","Adult","Mature_Adult"))
glm.data$Sex <- factor(glm.data$Sex, levels = c("Male","Female"))
glm.data$titer <- factor(glm.data$titer, levels = c("none","Low","Med","High"))
glm.data$Etnhnicity <- factor(glm.data$Etnhnicity, levels = c("TICUNA","Not reported","SENU","BORA", "COCAMA","HALF BLOOD"))
glm.data$Education <- factor(glm.data$Education, levels = c("PRIMARY","STUDENT","HIGH SCHOOL","TECHNICAL","UNIVERYESTY","MASTERY","Not reported" ))
glm.data$Case_origin <- factor(glm.data$Case_origin, levels = c("Puerto Nariño","PALMERAS","Not reported","COMUNIDAD DE SAN FRANCISCO","COMUNIDAD SANTA TERESITA","COMUNIDAD 12 DE OCTUBRE","SAN PEDRO DE TIPISCA"))


#Full Model
(lamp.corr <- glm(y ~ AGE*Sex+Education+Etnhnicity+Case_origin, binomial,
                        data = glm.data))

(summary(lamp.corr))

plot(lamp.corr)

anova(lamp.corr, test = "Chi")

# Remove the interactions

(lamp.corr.p1 <- glm(y ~ AGE+Sex+Education+Etnhnicity+Case_origin, binomial,
                  data = glm.data))

(summary(lamp.corr.p1))

#plot(lamp.corr.p1)

anova(lamp.corr.p1, test = "Chi")


# Remove the non-significant terms

(lamp.corr.p2 <- glm(y ~ AGE+Sex+Education+Case_origin, binomial,
                     data = glm.data))

(summary(lamp.corr.p2))

#plot(lamp.corr.p2)

anova(lamp.corr.p2, test = "Chi")

# Remove the non-significant terms

(lamp.corr.p3 <- glm(y ~AGE+Sex+Etnhnicity+Case_origin, binomial,
                     data = glm.data))

(summary(lamp.corr.p3))

  #plot(lamp.corr.p3)

anova(lamp.corr.p3, test = "Chi")

# Remove the non-significant terms

(lamp.corr.p4 <- glm(y ~AGE+Education+Etnhnicity+Case_origin, binomial,
                     data = glm.data))

(summary(lamp.corr.p4))

  #plot(lamp.corr.p4)

anova(lamp.corr.p4, test = "Chi")

# Remove the non-significant terms

(lamp.corr.p5 <- glm(y ~Sex+Education+Etnhnicity+Case_origin, binomial,
                     data = glm.data))

(summary(lamp.corr.p5))

#plot(lamp.corr.p5)

anova(lamp.corr.p5, test = "Chi")

# Remove the non-significant terms

(lamp.corr.p6 <- glm(y ~AGE+Sex+Case_origin, binomial,
                     data = glm.data))

(summary(lamp.corr.p6))

#plot(lamp.corr.p6)

anova(lamp.corr.p6, test = "Chi")

# Remove the non-significant terms

(lamp.corr.p7 <- glm(y ~ AGE+Education+Case_origin, binomial,
                     data = glm.data))

(summary(lamp.corr.p7))

#plot(lamp.corr.p7)

anova(lamp.corr.p7, test = "Chi")

# Remove the non-significant terms
(lamp.corr.p8 <- glm(y ~ Sex+Education+Case_origin, binomial,
                     data = glm.data))

(summary(lamp.corr.p8))

#plot(lamp.corr.p8)

anova(lamp.corr.p8, test = "Chi")
library(car)
Anova(lamp.corr.p8, type = 3)


# Remove the non-significant terms
(lamp.corr.p9 <- glm(y ~  Sex+Etnhnicity+Case_origin, binomial,
                     data = glm.data))

(summary(lamp.corr.p9))

#plot(lamp.corr.9)

anova(lamp.corr.p9, test = "Chi")

# Remove the non-significant terms
(lamp.corr.p10 <- glm(y ~ AGE+Etnhnicity+Case_origin, binomial,
                     data = glm.data))

(summary(lamp.corr.p10))

  #plot(lamp.corr.p10)

anova(lamp.corr.p10, test = "Chi")

# Remove the non-significant terms
(lamp.corr.p11 <- glm(y ~ Education+Etnhnicity+Case_origin, binomial,
                      data = glm.data))

(summary(lamp.corr.p11))

#plot(lamp.corr.p10)

anova(lamp.corr.p11, test = "Chi")

# model selection with AIC
#want the model with the lowest AIC
#backwards step-wise deletion and used AIC for model selection. 

AIC(lamp.corr,lamp.corr.p1,lamp.corr.p2, lamp.corr.p3,
    lamp.corr.p4,lamp.corr.p5, lamp.corr.p6,
    lamp.corr.p7,lamp.corr.p8,lamp.corr.p9,lamp.corr.p10,lamp.corr.p11)


#drop low sample size to see if its driving results 
#qualitatively similar (based on p values ) 

library(AICcmodavg)
library(xtable)
Cand.models <-list('global' = lamp.corr,'m1' = lamp.corr.p1,'m2' = lamp.corr.p2,'m3' =  lamp.corr.p3,
                   'm4' =  lamp.corr.p4, 'm5' = lamp.corr.p5, 'm6' = lamp.corr.p6,
                   'm7' = lamp.corr.p7,'m8' = lamp.corr.p8,'m9' = lamp.corr.p9,
                   'm10' = lamp.corr.p10,'m11' = lamp.corr.p11)

Selectiontable<-aictab(cand.set = Cand.models, second.ord = FALSE)

print(xtable(Selectiontable, caption = 'Model selection table on Prevalance of infection',
             label='tab:selection'), include.rowanmes = FALSE, caption.placement= 'top')

# Attempt at Post-hoc analysis  

education_counts<-glm.data%>%count(result,Education)

education_counts<-education_counts%>%
   pivot_wider(names_from =result , values_from = n)

(education_counts[is.na(education_counts)] = 0)

education_counts<-education_counts%>%
   rename ('neg' = '0')%>%
   rename ('pos' = '1')%>%
  mutate(total= pos+neg)%>%
  mutate(percent = pos/total)

edu.plot<-education_counts%>%
  ggplot(aes(x= Education, y= percent,color= 'black'))+
  geom_col()+
  stat_summary(aes(y = percent), fun.data=mean_se, colour="magenta", geom="errorbar")+
  scale_color_manual(values = c("Black")) +
  labs(x = "", y = "percent") +
  theme_zlab() +
  aesthetic+
  theme(
    axis.text.x = element_text(angle = 90, hjust=.5),
    legend.title = element_blank(),
  )
  
edu.plot

#Location
location_counts<-glm.data%>%count(result,Case_origin)

location_counts<-location_counts%>%
  pivot_wider(names_from =result , values_from = n)

(location_counts[is.na(location_counts)] = 0)

location_counts<-location_counts%>%
  rename ('neg' = '0')%>%
  rename ('pos' = '1')%>%
  mutate(total= pos+neg)%>%
  mutate(percent = pos/total)

location.plot<-location_counts%>%
  ggplot(aes(x= Case_origin, y= percent,color= 'black'))+
  geom_col()+
  stat_summary(aes(y = percent), fun.data=mean_se, colour="magenta", geom="errorbar")+
  scale_color_manual(values = c("Black")) +
  labs(x = "", y = "percent") +
  theme_zlab() +
  aesthetic+
  theme(
    axis.text.x = element_text(angle = 90, hjust=.5),
    legend.title = element_blank(),
  )

location.plot

library(multcomp)

Treat.comp <- glht(lamp.corr.p8)
 summary(Treat.comp)
#filter out low sample size. rerun all models (re-select model-posthoc GLHT)

#re-run model with removal of low Sample size - remove ethnicity (only one reported)
glm.data<-glm.data%>%
  filter(!Education %in% c("STUDENT","UNIVERYESTY","MASTERY","Not reported" ))

glm.data<-glm.data%>%
  filter(!Case_origin %in% c("PALMERAS","COMUNIDAD SANTA TERESITA","COMUNIDAD DE SAN FRANCISCO","Not reported" ))

#Full Model
(lamp.corr.filt <- glm(y ~ AGE*Sex+Education+Case_origin, binomial,
                  data = glm.data))

(summary(lamp.corr.filt))

plot(lamp.corr.filt)

anova(lamp.corr.filt, test = "Chi")

# Remove the interactions

(lamp.corr.filt.p1 <- glm(y ~ AGE+Sex+Education+Case_origin, binomial,
                     data = glm.data))

(summary(lamp.corr.filt.p1))

#plot(lamp.corr.p1)

anova(lamp.corr.filt.p1, test = "Chi")


# Remove the non-significant terms

(lamp.corr.filt.p2 <- glm(y ~ AGE+Sex+Case_origin, binomial,
                     data = glm.data))

(summary(lamp.corr.filt.p2))

#plot(lamp.corr.p2)

anova(lamp.corr.filt.p2, test = "Chi")

# Remove the non-significant terms

(lamp.corr.filt.p3 <- glm(y ~AGE+Education+Case_origin, binomial,
                     data = glm.data))

(summary(lamp.corr.filt.p3))

#plot(lamp.corr.p4)

anova(lamp.corr.filt.p3, test = "Chi")

# Remove the non-significant terms

(lamp.corr.filt.p4 <- glm(y ~Sex+Education+Case_origin, binomial,
                     data = glm.data))

(summary(lamp.corr.filt.p4))

#plot(lamp.corr.p5)

anova(lamp.corr.filt.p4, test = "Chi")

#AIC selection

AIC(lamp.corr.filt,lamp.corr.filt.p1,lamp.corr.filt.p2,
    lamp.corr.filt.p3,lamp.corr.filt.p4)
#drop low sample size to see if its driving results 
#qualitatively similar (based on p values ) 

