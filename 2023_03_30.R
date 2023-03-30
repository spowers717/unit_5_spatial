# SEP
# 2023-03-30
# Points, lines, polygons

library(sf)

USA_crti_hab = st_read(dsn="data/North_Atlantic_Right_Whale_Critical_Habitat/", 
                       layer = "North_Atlantic_Right_Whale_Critical_Habitat")

USA_crit_hab_sf = st_transform(USA_crti_hab, crs = 4326) #EPSG 44326 -> WGS 84
# 4269 -> NAD 83

CAN_crit_hab = read.csv("data/NARW_canadian_critical_habitat_2017.csv")
head(CAN_crit_hab)

CAN_crit_hab_sf = CAN_crit_hab %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) # turned it into points

CAN_crit_hab_sf 
# need to turn points into polygons

CAN_crit_hab_sf = CAN_crit_hab %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  dplyr::group_by(habitat, country) %>%
  dplyr::summarize(do_union=FALSE) # take these groups and then summarize them into an object
#keeps the order of the points the same
  
CAN_crit_hab_sf 
# geometry type is now multipoint
# turn each of those multipoints into a closed polygon

CAN_crit_hab_sf = CAN_crit_hab %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  dplyr::group_by(habitat, country) %>%
  dplyr::summarize(do_union=FALSE) %>%
  st_cast("POLYGON")
 
CAN_crit_hab_sf  

USA_crit_hab_sf$habitat = c("GOM", "SEUS")
USA_crit_hab_sf$country = "USA"

USA_crit_hab_sf= USA_crit_hab_sf %>%
  dplyr::select(country, habitat, geometry)

crit_hab = rbind(USA_crit_hab_sf, CAN_crit_hab_sf)
  
crit_hab
  
  