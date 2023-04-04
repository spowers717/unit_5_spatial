# SEP
# 2023-04-04
# Continued 

library(sf)
library(tidyverse)
library(raster)
library(marmap) 
library(mapdata)

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

carcass = read.csv("data/RW_carcasses_2017.csv")
head(carcass)

lon_bounds = c(-72, -53)
lat_bounds = c(39, 53)

world_map = map_data("worldHires", ylim = lat_bounds, xlim = lon_bounds)

crit_map = ggplot() +
  geom_polygon(data=world_map, aes(x=long, y = lat, group = group), fill= "black") +
  geom_sf(data= crit_hab, aes(fill = country)) +
  geom_point(data = carcass, aes(x = Longitude, y=Latitude, color= Carcass_position)) +
  coord_sf(1.3, xlim = lon_bounds, ylim = lat_bounds) +
  theme_bw()


bath_m_raw = marmap::getNOAA.bathy(lon1 = lon_bounds[1]-2, lon2 = lon_bounds[2]+2,
                                   lat1 = lat_bounds[1]-2, lat2 = lat_bounds[2]+2, resolution = 4) # resolution default: 4 minutes
# with bathymetry data 

bath_m_fortify = marmap::fortify.bathy(bath_m_raw) 
bath_m = bath_m_fortify %>%
  mutate(depth_m = ifelse(z>0, NA, z)) %>%
  dplyr::select(-z)

rw_map = ggplot()+
  geom_raster(data = bath_m , aes(x = x, y = y, fill = depth_m), alpha=0.75) + 
  scale_fill_gradientn(colors=c("black", "navy", "blue4","lightblue"), 
                       values = scales::rescale(c(-5000, -3000, -300, 0)), 
                       name="Depth (m)") +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black", color = NA) +
  geom_sf(data=crit_hab, alpha = 0.5, fill='yellow') +
  geom_point(data = carcass, aes(x = Longitude, y = Latitude, color = Carcass_position), size=2) + 
  coord_sf(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  ylab("Latitude") + xlab("Longitude") + theme_classic()

# ais data

library(lubridate)

lon_bounds = c(-82, -76)
lat_bounds = c(25, 34)

ais_day = read.csv("data/processed_ais/ais_2017-01-25.csv")
head(ais_day)

USA_crit_hab = st_read("data/North_Atlantic_Right_Whale_Critical_Habitat/", 
                       "North_Atlantic_Right_whale_Critical_Habitat")
USA_crit_hab

# it is a polygon and NAD83

world_map = map_data("worldHires", xlim= lon_bounds, ylim=lat_bounds)
head(world_map)


ais_map_pts = ggplot() +
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group)) +
  geom_sf(data=USA_crit_hab, fill = "yellow", alpha = 0.5) +
  geom_point(data = ais_day, aes(x=LON, y=LAT, color = CallSign), size = 0.5) +
  coord_sf(1.3, xlim = lon_bounds, ylim = lat_bounds) +
  guides(color = FALSE) +
  theme_classic()
# guide = do not print legend
# crop ais points by the habitat polygon
# group our lines by callsign
# get rid of data before further analyzing it 

ships_RW_intersect = ais_day %>%
  st_as_sf(coords=c("LON", "LAT"), crs=4269)
# Using NAD83
# turning spatial into special feature object
# tons of points want to shrink them to the critical habitat area
Sys.time()
ships_RW_intersect = ais_day %>%
  st_as_sf(coords=c("LON", "LAT"), crs=4269) %>%
  st_intersection(USA_crit_hab %>% dplyr::select(geometry))
Sys.time()
ships_RW_intersect


law_breakers = ships_RW_intersect %>%
  filter(Length >20) %>% # units in meters, law applies to boats > 65 ft in length
  filter(SOG>10) #in knots

dim(law_breakers)
length(unique(law_breakers$CallSign)) # 26 boats broke the law on this day which is the height of calving season
unique(law_breakers$VesselName) # names of the lawbreakers

#collapse points into lines
# how much distance have these vessels traveled while breaking the law
head(law_breakers)

illegal_paths = law_breakers %>% # make sure data is in order by time and lubridate package
  mutate(date_time = lubridate::ymd_hms(BaseDateTime))
# gives us the time variable in the year month day and hour minute second
# we want r to recognize it as a date

str(illegal_paths)
# official date time object type column now and will order it as we expect now
illegal_paths = law_breakers %>% 
  mutate(date_time = lubridate::ymd_hms(BaseDateTime)) %>%
  arrange(date_time) %>%
  group_by(CallSign) %>%
  summarize(do_union = FALSE) %>% #creates a multipoint
  st_cast("LINESTRING") %>%
  st_make_valid() # if it is not a valid linestring get rid of it
  
law_breaking_map = ggplot()+
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black", color = NA) + # add coastline
  geom_sf(data=USA_crit_hab, alpha = 0.5, color=NA, fill='yellow') +
  geom_sf(data=illegal_paths, aes(color=CallSign)) +
  coord_sf(1.3, xlim = lon_bounds, ylim = lat_bounds) + # Crop map edges
  guides(color="none") +
  ylab("Latitude") + xlab("Longitude") + theme_classic() 

law_breaking_map

illegal_path_lengths = illegal_paths %>%
  mutate(track_length_m = st_length(geometry))
head(illegal_path_lengths)

str(illegal_path_lengths)

class(illegal_path_lengths$track_length_m) # units class, double type; can convert to numeric with as.numeric()

tot_illegal_path = sum(illegal_path_lengths$track_length_m)
tot_illegal_path


# or convert the lengths from units class to numeric class and calculate total track length using dplyr:
tot_illegal_path = illegal_paths %>%
  mutate(track_length_m = as.numeric(st_length(geometry))) %>% 
  summarize(tot_track_length_m = sum(track_length_m)) %>%
  mutate(tot_track_length_km = tot_track_length_m/1000) %>%
  st_drop_geometry()

tot_illegal_path




















               





















