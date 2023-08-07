library("tidyverse")
library("ggpubr")
library("vegan")
library("colorRamps")

load('RESULTS_b_BC_ANALYSIS.Rdata')
rm(list=setdiff(ls(), c("sol","result","Env","ComData")))

load('DATA_G.Rdata') # SP2,DATAG

## Set the default theme for figures
theme_set(theme_bw(base_size = 12))

sites <- scores(sol)$sites %>% as_tibble(rownames="link") %>%
  separate(col=link,into=c("major_area","year","lseason"),remove=FALSE) 

#species2 <- scores(sol)$species %>% 
#  as_tibble(rownames="species_code") %>%
#  mutate(species_code=as.numeric(species_code)) %>%
#  filter(species_code %in% SP2$species_code)

BAYS <-data.frame(major_area=c(1:8),Bays=factor(c("SL", "GB", "MB","SB","AB","CC","UM","LL"), level=c("SL", "GB", "MB","SB","AB","CC","UM","LL"))) 
  
figure1 <- sites %>%
  select(link, NMDS1, NMDS2, NMDS3) %>%
  left_join(Env,by = c("link")) %>%
  left_join(BAYS, by=c("major_area")) %>%
  ggplot(aes(x=NMDS1, y=NMDS2, color=Bays,shape=Bays)) +
  scale_shape_manual(values=1:8) +
  geom_point(size=3) +
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("NMDS A1")+
  ylab("NMDS A2")+
  coord_fixed(ratio = 1)

SEASON <-data.frame(season=factor(c(1:2)),Season=factor(c("Spring","Fall"), level=c("Spring","Fall"))) 

figure2 <- sites %>%
  select(link, NMDS1, NMDS2, NMDS3) %>%
  left_join(Env,by = c("link")) %>%
  left_join(SEASON, by=c("season")) %>%
  ggplot(aes(x=NMDS1, y=NMDS3, color=Season,shape=Season)) +
  geom_point(size=3,alpha=0.5) +
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  xlab("NMDS A1")+
  ylab("NMDS A3")+
  coord_fixed(ratio = 1)

figure3 <- sites %>%
  select(link, NMDS1, NMDS2, NMDS3) %>%
  left_join(Env,by = c("link")) %>%
  ggplot(aes(x=NMDS3, y=NMDS2, color=year)) +
  scale_color_gradientn(colours = colorRamps::blue2green2red(n=10),  na.value="black")+
  geom_point(alpha=0.5) +
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  coord_fixed(ratio = 1) +
  xlab("NMDS A3")+
  ylab("NMDS A2")+
  labs(color="Year")

# Print this so that the height is 3 times greater than width
figures1<-ggarrange(figure1, figure3,figure2,
          labels = c("(A)","(B)","(C)"),
          align=c("hv"),
          ncol = 1, nrow = 3)

SP2 <- SP2 %>% 
  mutate(holo=if_else(taxa=="Holostei", 1, 0), elasm=if_else(taxa=="Elasmobranchii",1,0))

#### 
DATAG2 <- as.data.frame(DATAG) %>%
  mutate(link=str_c(major_area,"_",year,"_",season)) %>%
  select(-c(major_area, year, season, salinity,temperature,diss_oxygen,turbidity)) %>%
  gather(species_code,catch,-link) %>%
  mutate(species_code=as.numeric(species_code)) %>%
  left_join(SP2) %>%
  mutate(f_catch=fresh.water*catch,m_catch=marine*catch,e_catch=elasm*catch,h_catch=holo*catch) %>%
  group_by(link) %>%
  summarize(prop_fresh=sum(f_catch)/sum(catch),prop_marine=sum(m_catch)/sum(catch),prop_elasm=sum(e_catch)/sum(catch),prop_holo=sum(h_catch)/sum(catch))

figure4 <-sites %>%
  select(link, NMDS1, NMDS2, NMDS3) %>%
  left_join(Env,by = c("link")) %>%
  ggplot(aes(x=NMDS1, y=NMDS3, color=salinity)) +
  geom_point(alpha=0.5) +
  scale_color_gradientn(colours = colorRamps::blue2green2red(n=10),  na.value="black")+
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  coord_fixed(ratio = 1) +
  xlab("NMDS A1")+
  ylab("NMDS A3")+
  labs(color="Salinity")

figure5 <-sites %>%
  left_join(DATAG2,by = c("link")) %>%
  ggplot(aes(x=NMDS1, y=NMDS3, color=prop_fresh)) +
  geom_point(alpha=0.5) +
  scale_color_gradientn(colours = colorRamps::blue2green2red(n=10),  na.value="black")+
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  coord_fixed(ratio = 1) +
  xlab("NMDS A1")+
  ylab("NMDS A3")+
  labs(color="% Fresh SP")

figure6 <- sites %>%
  left_join(DATAG2,by = c("link")) %>%
  ggplot(aes(x=NMDS1, y=NMDS3, color=prop_elasm)) +
  geom_point(alpha=0.5) +
  scale_color_gradientn(colours = colorRamps::blue2green2red(n=10),  na.value="black")+
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  coord_fixed(ratio = 1) +
  xlab("NMDS A1")+
  ylab("NMDS A3")+
  labs(color="% Elasmo.")

figure7 <- sites %>%
  left_join(DATAG2,by = c("link")) %>%
  ggplot(aes(x=NMDS1, y=NMDS3, color=prop_holo)) +
  geom_point(alpha=0.5) +
  scale_color_gradientn(colours = colorRamps::blue2green2red(n=10),  na.value="black")+
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
  coord_fixed(ratio = 1) +
  xlab("NMDS A1")+
  ylab("NMDS A3")+
  labs(color="% Gar")

figures2<-ggarrange(figure4,figure5,figure7,
                    labels = c("(A)","(B)","(C)"),
                    align=c("hv"),
                    ncol = 1, nrow = 3)

tiff (filename = "figure_NMDS.tif", width = 320, height = 650, units ="px")
print(figures1)
dev.off()

tiff (filename = "figure_env.tif", width = 330, height = 580, units ="px")
print(figures2)
dev.off()