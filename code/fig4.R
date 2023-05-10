library(tidyverse)
library(ggpubr)
library(ggtext)
library(dplyr)
library(cowplot)
library(magrittr)
library(Hmisc)
library(gridExtra)
library(DescTools)
library(magick)
library(pdftools)
library(grImport2)
library(broom)
library(ggcorrplot)
library(here)
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

#load in PDF
illust.hb <- image_read_pdf("../plots/pdf/Figure-5.v1.pdf", density = 600)
FigIll<- ggdraw() +draw_image(illust.hb, scale = 1)

A<- plot_grid(FigIll, labels=c(''), ncol=1)
A

#save_plot("../plots/fig4.pdf",A, base_height = 4, base_width = 6,device = cairo_pdf)

#load in PDF for supplemental figure 

illust.suppA <- image_read_pdf("../plots/pdf/supp_fig2A.pdf", density = 600)
FigIllSuppA<- ggdraw() +draw_image(illust.suppA, scale = 1)

illust.suppB <- image_read_pdf("../plots/pdf/supp_fig2B.pdf", density = 600)
FigIllSuppB<- ggdraw() +draw_image(illust.suppB, scale = 1)

supp2<- plot_grid(FigIllSuppA,FigIllSuppB, labels=c('A','B'), ncol=1)
supp2

save_plot("../plots/sup_fig2.pdf",supp2, base_height = 8, base_width = 8,device = cairo_pdf)
