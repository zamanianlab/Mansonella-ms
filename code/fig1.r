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

# data wrangling/plotting
library(tidyverse)
library(janitor)

# stats
library(broom)

# other plotting
library(cowplot)
library(ggtext)
library(ggbeeswarm)
library(ZamanianLabThemes)
library(ggrepel)

# misc
library(conflicted)
library(here)

conflict_prefer("filter", "dplyr")

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

#map code 

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
     
   data_bubble<-data_bubble%>%
     filter(Case_origin =='Puerto Nariño')

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
geom_point(data = cities$osm_points,aes(geometry = geometry), color= "#d95821", stat = "sf_coordinates",size=2)+
ggrepel::geom_label_repel(data = cities$osm_points,
                          aes(label = cities$osm_points$name, geometry = geometry),
                          fill = "#d95821",
                          stat = "sf_coordinates",
                          min.segment.length = 0,
                          nudge_y = 0.05,
                          label.padding = unit(0.2, "lines"),
                          segment.color = "#d95821",
                          color = "white",
                          size = 4.5,
                          label.size = 0.5,
                          alpha = 0.8) +
  annotate("text", x=-70, y=-3.0, label = "Colombia", size = 4) +
  annotate("text", x=-70.5, y=-4, label = "Peru", size = 4) +
  annotate("text", x=-69.8, y=-4.45, label = "Brazil", size = 4) +
  coord_sf(
    xlim = c(-70.85,-69.35), #lat_bounds,
    ylim = c(-4.65,-2.65), #lon_bounds,
    expand = TRUE) +
 geom_point(data=data_bubble,aes(x=long, y=lat), color= "#800000", size=2)+
  ggrepel::geom_label_repel(
    data = data_bubble, aes(x = long, y = lat, label = Case_origin),
    fill = "#800000",
    min.segment.length = 0,
    nudge_y = 0.05,
    label.padding = unit(0.2, "lines"),
    segment.color = "#800000",
    color = "white",
    size = 4.5,
    label.size = 0.5,
    alpha = 0.8) +
  guides(alpha=FALSE)+
  labs(fill="", size= "")+
  aesthetic+
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(colour = "black", size=2),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 65, hjust = 1,face='plain'),
    legend.position = '',
    plot.background = element_rect(fill = "#FFFFFF"),
    axis.title = element_blank()
  ) +

  NULL
map

#Plot

illust.hb <- image_read_pdf("../images/Mansonella.pdf", density = 600)
FigIll<- ggdraw() +draw_image(illust.hb, scale = 1)

AB<- plot_grid(map,FigIll,labels=c('A','B'), rel_heights=c(1.8,1), ncol=2, align='v' )
AB

save_plot("../plots/fig1.pdf", AB, base_height = 5, base_width = 10)

