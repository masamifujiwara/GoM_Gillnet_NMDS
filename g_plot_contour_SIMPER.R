library("tidyverse")
library("ggpubr")
library("readxl") 

rm(list=ls())  # Clear saved variables

load('RESULTS_f_SIMPER_ANALYSIS.Rdata')

theme_set(theme_bw(base_size = 12)+
            theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank()))

species<- results$species_code 

## SP2,DATAG
load('DATA_G.Rdata')

data <- DATAG %>%
  gather(species_code,catch,-c(major_area,year,season,salinity,temperature,diss_oxygen,turbidity)) %>%
  mutate(species_code=as.numeric(species_code)) 

plot_contour <-function(species,data,SP){
  
  data <- data %>% filter(species_code == species) %>%
    group_by(major_area,year) %>%
    summarize(catch=sum(catch)) %>%
    ungroup()

  SP<-SP %>%
    filter(species_code==species) %>%
    select(common_name)
  
  mcount <- max(data$catch, na.rm = TRUE)
  
  figure1 <- data %>%
    ggplot(aes(x=major_area,y=year))+
    geom_raster(aes(fill=catch))+
    scale_fill_gradientn(colours = c("white", "blue","rosybrown2","red"), values = c(0, 0.0001, 0.333, 1), limits=c(0,mcount),na.value="black")+
    labs(title = paste(SP),x="Bay",y="Year",fill="Count")+scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8),labels=c("SL", "GB", "MB","SB","AB","CC","UL","LL"))
  
  return(figure1)
}

figures <- list()
for (k in c(1:length(species))){
  figures[[k]] <-plot_contour(species[[k]],data,SP2)
}

## This should work, but it does not! 
## figures<-map(species,~plot_contour(.x,data,SP2))

figures6<-ggarrange(figures[[1]], figures[[2]],figures[[3]],figures[[4]],
                    figures[[5]], figures[[6]],figures[[7]],figures[[8]],
                    labels = c("(A)","(B)","(C)","(D)",
                               "(E)","(F)","(G)","(H)"),
                    align=c("hv"),
                    ncol = 2, nrow = 4)

tiff (filename = "figure_contour1.tif", width = 900, height = 900, units ="px")
print(figures6)
dev.off()

figures7<-ggarrange(figures[[9]], figures[[10]],figures[[11]],figures[[12]],
                    figures[[13]],figures[[15]],figures[[18]],figures[[19]],
                    labels = c("(A)","(B)","(C)","(D)",
                               "(E)","(F)","(G)","(H)"),
                    align=c("hv"),
                    ncol = 2, nrow = 4)

tiff (filename = "figure_contour2.tif", width = 900, height = 900, units ="px")
print(figures7)
dev.off()

figures8<-ggarrange(figures[[14]], figures[[16]],
                    figures[[17]], figures[[20]],
                    labels = c("(A)","(B)","(C)","(D)"),
                    align=c("hv"),
                    ncol = 2, nrow = 2)

tiff (filename = "figure_contour3.tif", width = 900, height = 450, units ="px")
print(figures8)
dev.off()



# pdf("Contour.pdf",height=11,width=8.5)
# 
# for (k in 1:length(figure)){
#   print(figure[[k]])
# }
# dev.off()

load('RESULTS_d_BC_ANALYSIS_Bays.Rdata')
rm(list=setdiff(ls(), c("sol","species","SP2")))

BAYS <-c("Sabine Lake", "Galveston Bay", "Matagorda Bay","San Antonio Bay","Aransas Bay","Corpus Christi Bay","Upper Laguna Madre","Lower Laguna Madre") 

NMDS_Plot <- function(k,sol,species, BAYS){
  sites <- vegan::scores(sol[[k]])$sites %>% as_tibble(rownames="link") %>%
  separate(col=link,into=c("major_area","year","season"),remove=FALSE) 
  
  species2 <- vegan::scores(sol[[k]])$species %>% as_tibble(rownames="species_code") %>%
    mutate(species_code=as.numeric(species_code))%>%
    filter(species_code %in% species) %>%
    left_join(SP2)
  
  figure2 <- sites %>%
    ggplot(aes(x=NMDS1, y=NMDS2)) +
    geom_point() +
    geom_segment(data=species2, aes(x=0,y=0, xend=NMDS1, yend=NMDS2))+
    ggrepel::geom_label_repel(data=species2, aes(x=NMDS1, y=NMDS2, label=common_name))+#,position=position_jitter(width=0.05,height=0.05))+
    geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
    geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
    coord_fixed(ratio = 1)+
    xlab("NMDS B1")+
    ylab("NMDS B2")+
    ggtitle(BAYS[k])
  return(figure2)
}

figure2 <- list()
for (k in c(1:8)){
  figure2[[k]]<-NMDS_Plot(k,sol, species, BAYS)
}

## This should work, and it was working, but now it does not! 
## figure2<-map(c(1:8),~NMDS_Plot(.x,sol, species))

figures5<-ggarrange(figure2[[1]], figure2[[2]],figure2[[3]],figure2[[4]],
                    labels = c("(A)","(B)","(C)","(D)"),
                    align=c("hv"),
                    ncol = 2, nrow = 2)

tiff (filename = "figure_NMDS_fish1.tif", width = 900, height = 850, units ="px")
print(figures5)
dev.off()

figures6<-ggarrange(figure2[[5]], figure2[[6]],figure2[[7]],figure2[[8]],
                    labels = c("(A)","(B)","(C)","(D)"),
                    align=c("hv"),
                    ncol = 2, nrow = 2)

tiff (filename = "figure_NMDS_fish2.tif", width = 900, height = 850, units ="px")
print(figures6)
dev.off()
  
  