## This code depends on the files in other directories (original data). This is just for reference. 
## This code will save the formatted data as "DATA_G.Rdata", which will be used for subsequent analysis. 

suppressMessages(library("tidyverse"))
suppressMessages(library("openxlsx")) # read.xlsx(FILE)  write.xlsx (Variable, FILE)
                ## I also use the readxl package. Sorry for not being consistent. 

# Set working directory to the current folder location (only works under Windows)
# setwd(dirname(parent.frame(2)$ofile))  

# Clear saved variables
rm(list=ls())  

# Load Gillnet Data
DATA_G1<-read.xlsx("../../../2017 08 31 TPWD Data/Occupancy/TPWD Gill net 82-16 all spp.xlsx")%>% rename_all(tolower) %>% rename (station_id=sample_id)
DATA_G2<-read.xlsx("../../../2017 08 31 TPWD Data/Occupancy/TPWD Gill Net 17-19 all spp.xlsx")%>% rename_all(tolower)
DATAG<- as.data.frame(bind_rows(DATA_G1,DATA_G2) %>% filter(major_area %in% c(1:8))) # only use major areas 1-8

# Convert season to 1 and 2
DATAG[,"month"]<-as.numeric(DATAG[,"month"]>7)+1    # 1: Spring, 2: Fall

# ELIMINATE "NA" SPCODE and rename the variable month to season
DATAG <- DATAG[which(!is.na(DATAG[,'species_code'])),] %>% rename(season=month) 

# Species 615 and Species 212 are now Species 333 (TPWD combined the two species because identification is ambiguous)
DATAG[DATAG[,'species_code']==615,'species_code']<-333
DATAG[DATAG[,'species_code']==212,'species_code']<-333

## OBTAIN the SUMMARY of STATION DATA (take the median of environmental variables)
STATION_G <-distinct(DATAG,station_id, .keep_all = TRUE) %>% 
  select(c(major_area,year,season,temperature,salinity,diss_oxygen,turbidity))  %>% 
  group_by(major_area,year,season)%>%
  summarize(salinity=median(salinity,na.rm = TRUE),temperature=median(temperature,na.rm = TRUE),diss_oxygen=median(diss_oxygen,na.rm = TRUE),turbidity=median(turbidity,na.rm = TRUE)) %>%
  ungroup()

## Load SPECIES CODE (this includes all species observed in all samples in the TPWD data)
SPCODE<-read.csv("../../../2017 08 31 TPWD Data/OccupancyCSV/TPWD_Spp_codes_MF.csv") %>% 
  rename(taxa = names(.)[4]) %>%
  select(species_code,taxa,common_name=contains("common"),sci_name=contains('latin')) %>%
  mutate(species_code=as.numeric(species_code)) %>%
  filter(taxa %in% c(1))  %>%  # Select 1: fish only
  arrange(species_code)

## Additional species information and Select Teleostei Elasmobranchii Holostei   
SP2<-readxl::read_excel("habitat_relevant_species_taxa.xlsx") %>%
  mutate(brakish= suppressWarnings(as.numeric(brakish)),
         fresh.water=suppressWarnings(as.numeric(fresh.water)),
         marine=suppressWarnings(as.numeric(marine)),
         taxa=as.factor(taxa)) %>%
  mutate(common_name=str_trim(common_name,side="right")) %>%
  filter(taxa %in% c("Teleostei","Elasmobranchii","Holostei")) 

## Select the species code appears in SPCODE and SP2
SPCODE <- SPCODE %>% filter(species_code %in% SP2$species_code)

# Take the total catch in one sampling period (i.e. season)
# Eliminate species that do not appear in SPCODE and SP2
# Eliminate the species that were not observed in more than 10 sampling periods
DATAG<-DATAG %>%
  select(c(major_area,year,season,catch,species_code)) %>% 
  group_by(major_area,year,season,species_code) %>%
  summarize(catch=sum(catch)) %>%
  filter(species_code %in% SPCODE[,'species_code']) %>%
  ungroup() %>%
  group_by (species_code) %>%
  mutate(n=n()) %>%
  filter(n>10) %>%
  ungroup() %>%
  select(-n)
  
# Eliminate species code that do no appear in sample
# There is a smarter way of doing this in the previous block of code. 
SPCODE <- SPCODE %>%
  filter(species_code %in% DATAG$species_code) 

## Data are "spread" to create community matrix
## NA is entered when no observation was made during a sampling period so it is replaced with 0
## Environmental variables are
DATAG<-spread(DATAG, species_code, catch) %>% 
  replace(is.na(.),0) %>%
  left_join(STATION_G) %>% 
  relocate(any_of(c("salinity","temperature","diss_oxygen","turbidity")),.after =season) 

SP2 <- SP2 %>%
  filter(species_code %in% SPCODE$species_code) %>%
  droplevels()

save(SP2,DATAG,file = 'DATA_G.Rdata')