
library(tidyverse)
library(lubridate)

# set working directory based on active script
# https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio
setwd(dirname(rstudioapi::getActiveDocumentContext() %>% .$path))

df_storms <- readRDS("data/storm_data_episodes.rds")
df_gdp <- readRDS("data/gdp_bea.rds")

# summarize events on county/year/event type level
df_storms_year <- df_storms %>% 
  mutate(year = year(strt_dt)) %>% 
  group_by(STATE, CZ_NAME, STATE_FIPS, CZ_FIPS, year,
           EVENT_TYPE) %>% 
  summarize(injuries_direct = sum(injuries_direct),
            injuries_indirect = sum(injuries_indirect),
            deaths_direct = sum(deaths_direct),
            deaths_indirect = sum(deaths_indirect),
            dmg_property = sum(dmg_property),
            dmg_crops = sum(dmg_crops),
            episode_cnt = n())

# create a dataset w/ data from major weather types

unique(df_storms_year$EVENT_TYPE) %>%

case_when(EVENT_TYPE %in% c("Flood", "Flash Flood", "Coastal Flood", "Lakeshore Flood"))

# create indicators for each type of damage
df_storms_ind <- df_storms %>% 
  mutate(flood_ind = EVENT_TYPE %in% c("Flood", "Flash Flood", "Coastal Flood", "Lakeshore Flood"),
         tornado_wind_ind = EVENT_TYPE %in% c("Thunderstorm Wind", "Tornado", "Strong Wind", "High Wind"),
         hurricane_ind = EVENT_TYPE %in% c("Tropical Depression", "Tropical Storm", "Hurricane", "Hurricane (Typhoon)"),
         wildfire_ind = EVENT_TYPE %in% c("Wildfire"))

df_storms_dmg <- df_storms_ind %>% 
  group_by(STATE_FIPS, CZ_FIPS, EPISODE_ID) %>% 
  mutate(flood_dmg = dmg_property * flood_ind,
         tornado_wind_dmg = dmg_property * tornado_wind_ind,
         wildfire_dmg = dmg_property * wildfire_ind,
         hurricane_dmg = dmg_property * max(hurricane_ind)) %>% 
  group_by(STATE_FIPS, CZ_FIPS) %>% 
  summarize(across(flood_dmg:hurricane_dmg, sum)) %>% 
  mutate(geofips = paste0(str_pad(STATE_FIPS, 2, "left", "0"), str_pad(CZ_FIPS, 3, "left", "0")))

flood_dmg <- df_storms_dmg$flood_dmg
tornado_wind_dmg <- df_storms_dmg$tornado_wind_dmg
hurricane_dmg <- df_storms_dmg$hurricane_dmg
wildfire_dmg <- df_storms_dmg$wildfire_dmg

flood_dist <- ecdf(flood_dmg)(flood_dmg)
tornado_wind_dist <- ecdf(tornado_wind_dmg)(tornado_wind_dmg)
hurricane_dist <- ecdf(hurricane_dmg)(hurricane_dmg)
wildfire_dist <- ecdf(wildfire_dmg)(wildfire_dmg)

df_storms_dmg <- df_storms_dmg %>% 
  ungroup() %>% 
  mutate(flood_dist = flood_dist,
         tornado_wind_dist = tornado_wind_dist,
         hurricane_dist = hurricane_dist,
         wildfire_dist = wildfire_dist)

  
write_csv(df_storms_dmg, "storm_dmg_cnty.csv")

wldfr_dmg <- df_storms_dmg %>% 
  filter(wildfire_dmg > 0) %>% 
  .$wildfire_dmg

quantile(wldfr_dmg, c(0, 0.2, 0.4, 0.6, 0.8))


df_storms %>% 
  filter(year(strt_dt) == 2020,
         month(strt_dt) == 9,
         STATE == "CALIFORNIA") %>% 
  View(.)
