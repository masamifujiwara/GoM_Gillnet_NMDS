library("vegan")
library("ggrepel") 
library("ggpubr")
library("tidyverse")
library("parallel")

rm(list=ls())  # Clear saved variables

theme_set(theme_bw(base_size = 12))

load('DATA_G.Rdata') # SP2,DATAG

DATAG<-as.data.frame(DATAG) %>% 
  mutate(link=str_c(major_area,"_",year,"_",season)) %>% 
  relocate(link,.before =major_area) 

results3 <- results2 <- results1 <- Env <-list()

for (k in c(1:8)){
  DATAG2 <- DATAG %>%
    filter(major_area==k)%>%
    select(where(~ any(. != 0)))  # eliminate columns of all zeros
  
  ## Create Environmental DATA
  
  Env<- DATAG2 %>%
    select(c(link,major_area, year, season)) %>%
    mutate(year=ifelse(year %in% c(1982:2000), "Pre 2000", "Post 2000")) %>%
    mutate(season=ifelse(season ==1, "Spring","Fall"))%>%
    mutate(season=as.factor(season),major_area=as.factor(major_area), year=as.factor(year))
  
  ## Keep only fish data
  ComData <-DATAG2 %>% 
    select(-any_of( c("link","major_area", "year", "season", "salinity","temperature","diss_oxygen","turbidity"))) 
  
  ## SIMPER ANALYSIS -- Association with Season
  results1[[k]]<-summary(with(Env, simper(ComData, season , permutations = 999)))[[1]] %>%
    as.data.frame() %>%
    mutate(species_code=as.numeric(row.names(.))) %>%
    filter(p <= 0.01) %>% select(species_code,average) %>%
    arrange(desc(average))
  
  ## SIMPER ANALYSIS -- Association with Year
  results2[[k]]<-summary(with(Env, simper(ComData, year , permutations = 999)))[[1]] %>%
    as.data.frame() %>%
    mutate(species_code=as.numeric(row.names(.))) %>%
    filter(p <= 0.01) %>% select(species_code,average) %>%
    arrange(desc(average))
}

## Combine all results (from all bays)
## Rank based on how many bays the species significantly contribute
results<-bind_rows(results2) %>%
  group_by(species_code) %>%
  summarise(contributions=n()) %>%
  ungroup() %>%
  filter(contributions>3) %>%
  arrange(desc(contributions)) %>%
  left_join(SP2)

row.names(results) <- NULL

save (results2,results1,results,file = 'RESULTS_f_SIMPER_ANALYSIS.Rdata')