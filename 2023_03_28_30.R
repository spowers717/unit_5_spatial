# 2023-03-28
# SEP
# Bathymetry and chlorophyll rasters

# AquaMODIS_20020701_20220731.L3m.MC.CHL.chlor_a.9km.nc

# satellite mission, start date and end date, level 3 averaged monthly, monthly climatology(takes the dates and for every single july averaged over 20 years), standard chlorophyll product, 9km resolution (9 km by 9 km), net cdf file

library(tidyverse)
library(raster)
library(mapdata)
library(marmap) # getNOAA.bathy()

chl_raster = raster("data/AQUA_MODIS.20020701_20220731.L3m.MC.CHL.chlor_a.9km.nc")
chl_raster
names(chl_raster) = "chl_a"
names(chl_raster)

# change to data frame
chl_pts = rasterToPoints(chl_raster, spatial = TRUE)
head(chl_pts)
chl_df = data.frame(chl_pts)
head(chl_df)

hist(chl_df$chl_a)
# a lot of values around zero in mg/m^3
max(chl_df$chl_a)
# good candidate for transformation (a lot log transformations)
hist(log10(chl_df$chl_a))
# more interesting to look at and variability

# colors?
cols = rainbow(7, rev = TRUE)[-1] # indigo blue green orange red removed purple
global_chl_map = ggplot() +
  geom_raster(data = chl_df, aes(x=x, y=y, fill = log10(chl_a))) +
  scale_fill_gradientn(colors = cols, limits= c(-1.5, 0.75), name = "log_10(chl_a)") + theme_classic()
ggsave(global_chl_map, filename= "figures/global_chl_July2002_July2022.pdf", device = "pdf", height=5, width = 9)


# Gulf of Maine
lon_bounds = c(-72, -62)
lat_bounds = c(39, 47)

# crop
c(lon_bounds, lat_bounds)
chl_GOM_raster = raster::crop(chl_raster, extent(c(lon_bounds, lat_bounds)))
chl_GOM_raster

chl_GOM_df = data.frame(rasterToPoints(chl_GOM_raster, spatial = TRUE))
head(chl_GOM_df)

world_map = map_data("worldHires")
head(world_map)

GOM_chl_map = ggplot() +
  geom_raster(data = chl_GOM_df, aes(x=x, y=y, fill=log10(chl_a))) +
  geom_polygon(data=world_map, aes(x=long, y = lat, group = group), fill = "darkgrey") + scale_fill_gradientn(colors=cols, limits = c(-1, 1.75), ) + 
  theme_bw() +
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds, expand=FALSE)

bath_m_raw = marmap::getNOAA.bathy(lon1 = lon_bounds[1], 
                       lon2= lon_bounds[2],
                       lat1 = lat_bounds[1],
                       lat2 = lat_bounds[2],
                       resolution=4)
class(bath_m_raw)


# convert to dataframe

bath_m_df = marmap::fortify.bathy(bath_m_raw)
head(bath_m_df)
bath_m = bath_m_df %>%
  mutate(depth_m = ifelse(z>20, NA, z)) %>%
 dplyr::select(-z)
head(bath_m)  
tail(bath_m)



ggplot() +
  geom_raster(data = bath_m, aes(x=x, y=y, fill=depth_m)) +
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), fill = "black") +
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds, expand = FALSE) +
  scale_fill_gradientn(colors=c("black", "darkblue", "lightblue"),
                      values = scales::rescale(c(-6000, -300, 0)),
                      name = "Depth (m)")
# contour lines


ggplot() +
  geom_contour(data=bath_m, aes(x=x, y=y, z = depth_m), breaks = c(-100), linewidth = c(0.25), 
               colour = "grey") +
  geom_contour(data=bath_m, aes(x=x, y=y, z = depth_m), breaks = c(-200), linewidth = c(0.5), 
               colour = "grey") +
  geom_contour(data=bath_m, aes(x=x, y=y, z = depth_m), breaks = c(-300), linewidth = c(0.75), 
               colour = "grey") +
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group), fill = "black", color = NA) +
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds, expand = FALSE) + theme_bw()


GOM_bath_map_contours = ggplot()+
  geom_contour(data = bath_m, aes(x=x, y=y, z=depth_m), breaks=c(-100), linewidth=c(0.25), colour="grey") + # add 100m contour
  geom_contour(data = bath_m, aes(x=x, y=y, z=depth_m), breaks=c(-200), linewidth=c(0.5), colour="grey") + # add 250m contour
  geom_contour(data = bath_m, aes(x=x, y=y, z=depth_m), breaks=c(-500), linewidth=c(0.75), colour="grey") + # add 250m contour
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "black", color = NA) + # add coastline
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds, expand=FALSE) 

# rasterize the bathy object

bath_m_raster = marmap::as.raster(bath_m_raw)
bath_m_raster

#resample
bath_layer_chl_dims = raster::resample(bath_m_raster, chl_GOM_raster)

raster_stack = stack(chl_GOM_raster, bath_layer_chl_dims)

plot(raster_stack)

stack_df = data.frame(raster::rasterToPoints(raster_stack))
head(stack_df)


#O'reilly
oligo_chl_a = 0.1
eutro_chl_a = 1.67

stack_df = stack_df %>%
  mutate(trophic_index = case_when(chl_a < oligo_chl_a ~ "oligotrophic", 
        chl_a > oligo_chl_a & chl_a <eutro_chl_a ~ "mesotrophic", 
        chl_a >eutro_chl_a~ "eutrophic")) %>%
  mutate(trophic_index = as.factor(trophic_index))

summary(stack_df)

ggplot() +
  geom_raster(data=stack_df, aes(x=x, y=y, fill=trophic_index)) + 
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3, xlim = lon_bounds, ylim = lat_bounds, expand=FALSE)

stack_df %>%
  group_by(trophic_index) %>%
  summarize(n=n())


















