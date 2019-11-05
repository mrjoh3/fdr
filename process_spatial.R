

# processing spatial data

library(sf)

cfa_tfb <- st_read('shp/cfa_tfb_district.shp', stringsAsFactors = FALSE) 

towns_nm <- st_read('shp/VIC_TOWN_shp.dbf', stringsAsFactors = FALSE) 
towns <- st_read('shp/VIC_TOWN_POINT_shp.shp', stringsAsFactors = FALSE) %>%
  left_join(towns_nm, by = 'TOWN_PID') %>%
  st_join(cfa_tfb) %>%
  select(town_name = TOWN_NAME,
         cfa_tfb = TFB_DIST) %>%
  rowwise() %>%
  mutate(cfa_tfb = tools::toTitleCase(tolower(cfa_tfb)),
         town_val = gsub(' | - ', '-', tolower(unique(town_name)))) %>%
  ungroup() %>%
  arrange(town_name)
  


saveRDS(towns, 'fdr_fluid/towns.rds')

