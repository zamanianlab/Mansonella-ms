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


  #theme(panel.border = element_rect(color="black", fill=NA)) 

#read in data 
flow<- read_csv("../data/flow_tidy_cryo.csv")

#regressed flow vaalue to 0hr movement 
flow_worm<- flow%>%
  mutate(worm_number = c(2,2,1,3))%>%
  mutate(con_norm=(optical_flow/worm_number-(3198002/2)))
  #mutate(flow= (optical_flow/con))

#set levels 
flow_worm$other<-factor(flow$other, levels=c ('0hr','6hr','24hr','48hr'))

#plot movement over time
flow_plot<-flow_worm%>%
  ggplot(aes(x=other, y=con_norm))+
  geom_line(color='black',group='other')+
  geom_point()+
  labs(x = "Time", y = expression("Δ "* "Movement (optical flow units)"))+
  aesthetic +
  theme( axis.text.x = element_text(angle = 0,hjust = .5)
)

flow_plot

#load images of flowmaps 
illust.hb <- image_read_pdf("../images/cryo_grid.pdf", density = 250)
FigIll<- ggdraw() +draw_image(illust.hb, scale = 1)

AB<- plot_grid(flow_plot,FigIll, labels=c('A', 'B'), ncol=1, rel_heights = c(.9,.8),align ='v', axis = "lr" )
AB

save_plot("../plots/fig5lr.pdf",AB, base_height = 6, base_width = 8,device = cairo_pdf)


