

# processing spatial data

library(dplyr)
library(sf)
library(stringr)

cfa_tfb <- st_read('shp/cfa_tfb_district.shp', stringsAsFactors = FALSE) 

# town list
towns_nm <- st_read('shp/VIC_TOWN_shp.dbf', stringsAsFactors = FALSE) 
towns <- st_read('shp/VIC_TOWN_POINT_shp.shp', stringsAsFactors = FALSE) %>%
  left_join(towns_nm, by = 'TOWN_PID') %>%
  st_join(cfa_tfb) %>%
  select(town_name = TOWN_NAME,
         cfa_tfb = TFB_DIST) %>%
  mutate(cfa_tfb = tools::toTitleCase(tolower(cfa_tfb)),
         town_val = gsub(' | - ', '-', tolower(town_name))) %>%
  arrange(town_name) %>%
  .[!duplicated(.$town_name),] # distinct did not seen to work

  

# CFA stations

stn <- st_read('shp/geomark_point.shp', stringsAsFactors = FALSE) %>%
  filter(STATE == 'VIC',
         FEATSUBTYP == 'fire station',
         !grepl('SATELLITE|&|infrastructure', NAME, ignore.case = TRUE)) %>%
  select(town_name = NAME_LABEL) %>%
  mutate(town_name = str_trim(gsub('Fire Station|District', '', town_name), 'both'),
         town_name = gsub('\\.', '', town_name),
         town_val = tolower(gsub(' | - ', '-', town_name))) %>%
  st_join(cfa_tfb) %>%
  select(town_name,
         town_val,
         cfa_tfb = TFB_DIST) %>%
  mutate(cfa_tfb = tools::toTitleCase(tolower(cfa_tfb))) %>%
  st_cast('POINT')

#st_geometry(stn) <- NULL  

towns2 <- rbind(stn, towns) %>%
  .[!duplicated(.$town_name),] # distinct did not seen to work



saveRDS(towns2, 'fdr_fluid/towns.rds')

