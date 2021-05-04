################################################################################
################################################################################
## Create volume-carbon look-ups from SCG data and MA CFI data (run thru FVS) ##
################################################################################
################################################################################

################################################################################
## SCG data applied to SPNHF properties in northern hardwood region
# Data import
area <- read.csv("data/SCG_2018_Covertype_FVS_plot_Base.csv")
carbon <- read.csv("data/SCG_FVS_Carbon_11182020.csv")
vol <- read.csv("data/SCG_FVS_Summary2_East_v2.csv")


# Confirm no missing covertypes/stands
# (temp <- anti_join(area, carbon, by = c("Covertype" = "StandID")))
# (temp <- anti_join(area, vol, by = c("Covertype" = "StandID")))


# What's total area? and prop of each cover type
(scg_area_ac <- sum(area$acres))
area$prop_ttl_area_ac <- area$acres/scg_area_ac
sum(area$prop_ttl_area_ac)


# Convert all tons carbon to metric tons carbon
carbon[,3:11] <- carbon[,3:11]*0.907185


# Join data, select/rename variables, remove empty year
data <- vol %>% dplyr::select(StandID, Year, Tpa, BA, MCuFt) %>%
  left_join(carbon, by = c("StandID" = "StandID", "Year" = "Year")) %>%
  left_join(area, by = c("StandID" = "Covertype")) %>%
  dplyr::select(covertype = StandID, year = Year, tpa = Tpa, ba = BA,
                merch_cuft = MCuFt,
                ag_ttl_l_mt_ac_C = Aboveground_Total_Live,
                ag_merch_l_mt_ac_C = Aboveground_Merch_Live,
                bg_l_mt_ac_C = Belowground_Live,
                bg_d_mt_ac_C = Belowground_Dead,
                ag_d_mt_ac_C = Standing_Dead,
                cwd_mt_ac_C = Forest_Down_Dead_Wood,
                floor_mt_ac_C = Forest_Floor,
                shrub_herb_mt_ac_C = Forest_Shrub_Herb,
                ttl_mt_ac_C = Total_Stand_Carbon,
                area_ac = acres, prop_ttl_area_ac) %>%
  # filter(year < 2118) %>%
  ungroup()

rm(carbon, area, vol)

# Add cords & carbon conversion
# 1 cord = 128 cuft; unclear if FVS is green or dry
# 1 cord tpyically ~ 2000-5000 green lbs biomass (1-2.5 tons biomass); 1/2 that for C
data$merch_cords <- data$merch_cuft / 128
(data$mt_C_per_cord <- data$ag_merch_l_mt_ac_C/data$merch_cords) 
# Some hw cover types >1 meaning each cord has > 1 t C.
# Most cover types < 1 meaning each cord has < 1 t C.
# Ratios highest for hardwoods, lowest for softwoods -- as expected (lower C density)
(data$mt_C_per_merch_cuft = data$ag_merch_l_mt_ac_C/data$merch_cuft)


# # Do ratios persist over time?
# temp <- data %>%
#   group_by(covertype, year) %>%
#   summarise(avg = mean(mt_C_per_cord)) %>%
#   ungroup() %>%
# view()
# # The ratios differ subtly over time, even within covertypes. NBD.
# rm(temp)


# Tidy and keep bare minimum and only fist year in FVS run
data$mt_C_per_merch_cord <- data$mt_C_per_cord ; data$mt_C_per_cord <- NULL # be clear this is merch cords
data <- data %>%
  filter(year == 2018, !covertype == "Other") %>%
  mutate(type = ifelse(left(covertype, 1) == "H", "hw",
                       ifelse(left(covertype, 1) == "S", "sw", "mixed"))) %>%
  dplyr::select(type, merch_cuft, merch_cords, ag_merch_l_mt_ac_C, mt_C_per_merch_cuft, mt_C_per_merch_cord)


# Add zone
data$zone <- as.factor(paste0("nhw"))

# Set this SCG data to nhw zone
nhw <- data


################################################################################
## Load FVS output data for MA CFI -- calling this oakpine

## MA CFI data applied to SPNHF properties in oake pine zone

data <- read.csv("data/MA_CFI_FVS_out.csv")

# Convert all tons carbon to metric tons carbon
data[,3:6] <- data[,3:6]*0.907185


# Select and filter
data <- data %>%
  dplyr::select(type = TYPE, year = Year, #tpa = Tpa, ba = BA,
                merch_cuft = MCuFt,
                ag_ttl_l_mt_ac_C = Aboveground_Total_Live,
                ag_merch_l_mt_ac_C = Aboveground_Merch_Live,
                bg_l_mt_ac_C = Belowground_Live,
                # bg_d_mt_ac_C = Belowground_Dead,
                # ag_d_mt_ac_C = Standing_Dead,
                # cwd_mt_ac_C = Forest_Down_Dead_Wood,
                # floor_mt_ac_C = Forest_Floor,
                # shrub_herb_mt_ac_C = Forest_Shrub_Herb,
                ttl_mt_ac_C = Total_Stand_Carbon) %>%
  filter(!is.na(type)) %>%
  mutate(type = ifelse(type == "Hardwood", "hw",
                       ifelse(type == "Softwood", "sw", "mixed"))) %>%
  filter(year == 2020) 


# Add cords & carbon conversion
# 1 cord = 128 cuft; unclear if FVS is green or dry
# 1 cord tpyically ~ 2000-5000 green lbs biomass (1-2.5 tons biomass); 1/2 that for C
data$merch_cords <- data$merch_cuft / 128
(data$mt_C_per_merch_cord <- data$ag_merch_l_mt_ac_C/data$merch_cords) 
# Some hw cover types >1 meaning each cord has > 1 t C.
# Most cover types < 1 meaning each cord has < 1 t C.
# Ratios highest for hardwoods, lowest for softwoods -- as expected (lower C density)
(data$mt_C_per_merch_cuft <- data$ag_merch_l_mt_ac_C/data$merch_cuft)

data <- data %>%
  dplyr::select(type, merch_cuft, merch_cords, ag_merch_l_mt_ac_C, mt_C_per_merch_cuft, mt_C_per_merch_cord)

# Assign zone: 
data$zone <- as.factor(paste("oakp"))

# Set this MA CFI data to oakp
oakp <- data
