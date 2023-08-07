library("tidyverse")
library("ggrepel") 
library("ggpubr")
library("vegan")

load("RESULTS_d_BC_ANALYSIS_Bays.Rdata")
rm(list=setdiff(ls(), c("sol","result","Env")))

load('DATA_G.Rdata') # SP2,DATAG

## Set the default theme for figures
theme_set(theme_bw(base_size = 12))

SEASON <-data.frame(season=factor(c(1:2)),Season=factor(c("Spring","Fall"), level=c("Spring","Fall"))) 
BAYS <-c("Sabine Lake", "Galveston Bay", "Matagorda Bay","San Antonio Bay","Aransas Bay","Corpus Christi Bay","Upper Laguna Madre","Lower Laguna Madre") 

figure1 <- figure2 <- list()

for (k in c(1:8)){
  
  sites <- scores(sol[[k]])$sites %>% as_tibble(rownames="link") %>%
    separate(col=link,into=c("major_area","year","lseason"),remove=FALSE)
  
  species2 <- scores(sol[[k]])$species %>%
    as_tibble(rownames="species_code")
  
  figure1[[k]] <- sites %>%
    select(link, NMDS1, NMDS2, NMDS3) %>%
    left_join(Env[[k]],by = c("link")) %>%
    ggplot(aes(x=NMDS1, y=NMDS2,color=year)) +
    scale_color_gradientn(colours = colorRamps::blue2green2red(n=10),  na.value="black")+
    geom_point(alpha=0.5) +
    geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
    geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
    coord_fixed(ratio = 1) +
    ggtitle(BAYS[k])+
    xlab("NMDS B1")+
    ylab("NMDS B2")+
    labs(color="Year")
  
  if (k==7){sites$NMDS2 = -sites$NMDS2}
  
    figure2[[k]] <- sites %>%
    select(link, NMDS1, NMDS2, NMDS3) %>%
    left_join(Env[[k]],by = c("link")) %>%
    left_join(SEASON, by=c("season")) %>%
    ggplot(aes(x=NMDS1, y=NMDS2, color=Season,shape=Season)) +
    geom_point(size=3,alpha=0.5) +
    geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
    geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
    coord_fixed(ratio = 1)+
    xlab("NMDS B1")+
    ylab("NMDS B2")+
    ggtitle(BAYS[k])
  
}

figures3<-ggarrange(figure1[[1]], figure1[[2]],figure1[[3]],figure1[[4]],
                    figure1[[5]], figure1[[6]],figure1[[7]],figure1[[8]],
                    labels = c("(A)","(B)","(C)","(D)","(E)","(F)","(G)","(H)"),
                    align=c("hv"),
                    ncol = 2, nrow = 4)

figures4<-ggarrange(figure2[[1]], figure2[[2]],figure2[[3]],figure2[[4]],
                    figure2[[5]], figure2[[6]],figure2[[7]],figure2[[8]],
                    labels = c("(A)","(B)","(C)","(D)","(E)","(F)","(G)","(H)"),
                    align=c("hv"),
                    ncol = 2, nrow = 4)

tiff (filename = "figure_year_by_bay.tif", width = 550, height = 960, units ="px")
print(figures3)
dev.off()

tiff (filename = "figure_season_by_bay.tif", width = 550, height = 960, units ="px")
print(figures4)
dev.off()
