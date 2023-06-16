# install.packages("remotes")
#remotes::install_github("coolbutuseless/ggpattern")

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(sf)
library(tidyverse)

river<-sf::st_read("Major_Rivers_dd83/MajorRivers_dd83.shp")
texas3<-sf::st_read("Texas_State_Boundary_Detailed-shp/State_Boundary.shp")
major_bays<-sf::st_read("major-bays/MajorBays/MajorBays.shp")
bays_location <- openxlsx::read.xlsx("bays_location.xlsx")

## 
texas <- subset(map_data("state"), region %in% c("texas"))
tx_county <- subset(map_data("county"), region %in% c("texas"))
louisiana<- subset(map_data("state"), region %in% c("louisiana"))

## Get the cordinate system of river in SF Object
CRS<-st_crs(river)

## Convert the latitude longitude data.frame into SF object and convert it to Polygon 
texas2<-st_as_sf(texas, coords = c("long", "lat"),crs=CRS) %>% 
  group_by(group) %>%  
  summarise(geometry = st_combine(geometry)) %>% 
  st_cast("POLYGON")

louisiana<-st_as_sf(louisiana, coords = c("long", "lat"),crs=CRS) %>% 
  group_by(group) %>%  
  summarise(geometry = st_combine(geometry)) %>% 
  st_cast("POLYGON")

tx_county2<-st_as_sf(tx_county, coords = c("long", "lat"),crs=CRS)%>% 
  group_by(group) %>%  
  summarise(geometry = st_combine(geometry)) %>% 
  st_cast("POLYGON")

major_bays$COLOR<-c("1","5","2","6","3","7","4","8")

bays_location <- openxlsx::read.xlsx("bays_location.xlsx")
## PLOT State, bays, and Rivers
tx_base <-
  ggplot(data=texas3) + 
  geom_sf()+
  geom_sf(data=louisiana)+
  geom_sf(data=river) +  
  geom_sf(data=major_bays,aes(fill = COLOR), show.legend = FALSE)+
  scale_fill_grey()+
  coord_sf(xlim = c(-98, -93.5),  ylim = c(26, 30))+
   geom_text(data=bays_location,aes(x=long+0.12,y=lat,label=NAMES),size=8)+
  xlab("longitude")+ylab('latitude')+
  theme (panel.background  = element_blank(),axis.title=element_text(size=rel(1.5)),axis.text=element_text(size=rel(1.5))) +
  ggspatial::annotation_scale(aes(location="bl"),pad_x=unit(2,"in")) +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", pad_y = unit(3, "in")) 
# print(tx_base)

world <- ne_countries(scale = "medium", returnclass = "sf")
CRS<-st_crs(world)

GOM_base<-world %>%
  ggplot() +
  geom_sf() +
  coord_sf(xlim = c(-98.5, -85), ylim = c(23, 31), expand = FALSE)+
  geom_rect(aes(xmin=-98,xmax=-93.5,ymin=26,ymax=30),color = "black", fill=NA,size=2)+
  geom_text(data=tibble(x=-91,y=25),aes(x,y), label="Gulf of Mexico",size=10, inherit.aes=FALSE)+
  theme_nothing()+theme(panel.border=element_rect(color="black"))
  
MAP<- cowplot::ggdraw()+
  cowplot::draw_plot(tx_base) +
  cowplot::draw_plot(GOM_base, x=0.57,  y=0.15, width=0.4, height=0.3)

tiff (filename = "figure_map.tif", width = 900, height = 900, units ="px")
print(MAP)
dev.off()



