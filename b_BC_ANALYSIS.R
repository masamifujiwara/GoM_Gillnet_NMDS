library("vegan")
library("tidyverse")
library("parallel")

rm(list=ls())  # Clear saved variables

## SP2,DATAG
load('DATA_G.Rdata')

DATAG <- as.data.frame(DATAG)

DATAG <- DATAG %>% mutate(link=str_c(major_area,"_",year,"_",season)) %>% 
  relocate(link,.before =major_area) 

## Create Environmental DATA
Env<- DATAG %>%
  select(c(link,major_area, year, season, salinity,temperature,diss_oxygen,turbidity)) %>%
  mutate(season=as.factor(season))

## Create row names using the "link" column  
ComData<-DATAG 
row.names(ComData) <- ComData$link

## Keep only fish data
ComData <-ComData %>% 
  select(-c(link,major_area, year, season, salinity,temperature,diss_oxygen,turbidity)) 

## Convert the data into matrix
ComData <- as.matrix(ComData)

## Calculate dissimilarity matrix
BC_Dis <- vegdist(ComData, method="bray")

## Non-metric multidimensional scaling
set.seed(5)
sol<-metaMDS(ComData,distance = "bray", k = 3, try = 200, trymax = 200, 
             autotransform = TRUE, parallel=8)

# If we want, we can rotate the NMDS axes 
# ## https://search.r-project.org/CRAN/refmans/vegan/html/MDSrotate.html <Factor Rotation>
# vec <- Env %>% select(salinity)
# sol<-MDSrotate(sol, vec, na.rm = TRUE)

# sites <- scores(sol)$sites %>% as_tibble(rownames="link") %>%
#   separate(col=link,into=c("major_area","year","lseason"),remove=FALSE) 
# 
# species2 <- scores(sol)$species %>% as_tibble(rownames="SP") 

## Associate the environmental variables. 
##*****************************************************************
### https://jkzorz.github.io/2020/04/04/NMDS-extras.html
##****************************************************************
Env2 <- Env %>% select(-link)

result<-envfit(sol, Env2,choices=c(1,2,3),na.rm = TRUE)

save (Env,sol,result,ComData,file = 'RESULTS_b_BC_ANALYSIS.Rdata')

