#################################################################
#################################################################
## Model SPNHF numbers off of MA CFI data (oakp) and SCG (nhw) ##
#################################################################
#################################################################


# Load SPNHF data, remove forested wetland (don't have good carbon numbers), and compute vol for each parcel
spnhf <- read.csv("data/extrapolated_SPNHF.csv")
head(spnhf)
spnhf <- spnhf %>%
  dplyr::select(type, area_acres, sawvol_bf_ac, pulp_crd_ac, oakp4_nhw9) %>%
  filter(!type == "forested wetland") %>% 
  mutate(zone = ifelse(oakp4_nhw9 == 9, "nhw", "oakp")) %>%
  mutate(ttl_bf = sawvol_bf_ac * area_acres,
         ttl_crd = pulp_crd_ac * area_acres)


# Get avg of cords to carbon and bf to carbon for each zone and type within zone
head(nhw)
head(oakp)
lu_C <- rbind(nhw, oakp)
# Convert cubic feet into bf
lu_C$mt_C_per_merch_bf <- lu_C$mt_C_per_merch_cuft/12

  
lu_C <- lu_C %>%
  group_by(zone, type) %>%
  summarize(mt_C_per_merch_cord = mean(mt_C_per_merch_cord),
            mt_C_per_merch_bf = mean(mt_C_per_merch_bf),
            mt_C_per_merch_cuft = mean(mt_C_per_merch_cuft)) %>%
  ungroup()


# Apply those carbon conversions to total bf and crds in SPNHF data
spnhf <- spnhf %>%
  left_join(lu_C, by = c("zone"="zone", "type"="type"))
spnhf$mt_C_merch_volbf = spnhf$ttl_bf*spnhf$mt_C_per_merch_bf   
spnhf$mt_C_merch_volcord = spnhf$ttl_crd*spnhf$mt_C_per_merch_cord
spnhf$mt_C_merch = spnhf$mt_C_merch_volbf + spnhf$mt_C_merch_volcord
spnhf$mt_C_merch_per_acre = spnhf$mt_C_merch/spnhf$area_acres

