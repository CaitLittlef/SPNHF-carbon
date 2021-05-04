#################################################################
#################################################################
## Model SPNHF numbers off of MA CFI data (oakp) and SCG (nhw) ##
#################################################################
#################################################################


# Load SPNHF data, remove forested wetland (don't have good carbon numbers), and compute vol for each parcel
spnhf <- read.csv("data/extrapolated_SPNHF.csv")
head(spnhf)
boo <- spnhf %>%
  dplyr::select(type, area_acres, sawvol_bf_ac, pulp_crd_ac, oakp4_nhw9) %>%
  filter(!type == "forested wetland") %>% 
  mutate(zone = ifelse(oakp4_nhw9 == 9, "nhw", "oakp")) %>%
  mutate(ttl_bf = sawvol_bf_ac * area_acres,
         ttl_crd = pulp_crd_ac * area_acres)


## Create model of cords to carbon and bf to carbon for each zone and type within zone

foo <- 