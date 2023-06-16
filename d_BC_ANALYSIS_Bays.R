library("vegan")
library("tidyverse")
library("parallel")

rm(list=ls())  # Clear saved variables

## SP2,DATAG
load('DATA_G.Rdata')

DATAG<-as.data.frame(DATAG) %>% 
  mutate(link=str_c(major_area,"_",year,"_",season)) %>% 
  relocate(link,.before =major_area) 

sol <- result <- Env <-list()

set.seed(5)

for (k in c(1:8)){
  DATAG2 <- DATAG %>%
    filter(major_area==k)
  
  ## Create Environmental DATA
  
  Env[[k]]<- DATAG2 %>%
    select(c(link,major_area, year, season, salinity,temperature,diss_oxygen,turbidity)) %>%
    mutate(season=as.factor(season))
  
  ## Create row names using the "link" column  
  ComData<-DATAG2
  row.names(ComData) <- ComData$link
  
  ## Keep only fish data and eliminate the species never observed
  ComData <-ComData %>% 
    select(-c(link,major_area, year, season, salinity,temperature,diss_oxygen,turbidity)) %>%
    select(where(~ any(. != 0)))
  
  ## Convert the data into matrix
  ComData <- as.matrix(ComData)
  
  ## Calculate dissimilarity matrix
  BC_Dis <- vegdist(ComData, method="bray")
  
  ## Non-metric multidimensional scaling
  sol2<-metaMDS(ComData,distance = "bray", k = 3, try = 400, trymax = 400, 
               autotransform = TRUE, parallel=8)
  
  # ## https://search.r-project.org/CRAN/refmans/vegan/html/MDSrotate.html <Factor Rotation>
  vec <- Env[[k]] %>% select(year)
  sol[[k]]<-MDSrotate(sol2, vec, na.rm = TRUE)
  
  Env2 <- Env[[k]] %>% select(-link, -major_area)
  result[[k]]<-envfit(sol[[k]], Env2,choices=c(1,2,3),na.rm = TRUE)

}

save (sol,result,Env,file = 'RESULTS_d_BC_ANALYSIS_Bays.Rdata')
