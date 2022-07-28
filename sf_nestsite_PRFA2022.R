
############################  NEST BUFFER SCRIPT  ##############################
#.......................       Using SF Package      ...........................

# load packages relevant to this script:

library(sf)
library(ggplot2)
library(ggmap)
library(dplyr)
# library(raster)

rm( list = ls() )

###########################  SPATIAL REQUIREMENTS  #############################
#........... Google API key for ggmap & function to plot google map   ..........

register_google(key = )
# Define a function to fix the bbox to be in EPSG:3857

ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  # Convert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

##############################  Load PRFA Data  ################################
#.............. Data Extracted From 'datadf' in Cleaning Script   ..............

load( "cleaningPRFA22.RData" )
load( "nest_site_bufferpts.RData" )

nest_site <- datadf %>%
  dplyr::group_by(territory) %>%
  dplyr::select(., y, x, territory) # keep location and territory columns (and all rows)

nest_site <- nest_site[!duplicated(nest_site$y),] # remove duplicates (keeping only unique rows)

#########################   Define CRS/Projection  #############################
#.....................      Reproject data into UTM    .........................

nest_sf <- st_as_sf(nest_site, coords = c("x", "y"), crs = crsdata) # convert from data.frame to sf object
data_sf <- st_as_sf(datadf, coords = c("lon", "lat"), crs = crsdata)

st_crs(data_sf) == st_crs(nest_sf) # check if projections are the same
st_crs(data_sf) # check projection # WGS 84
st_crs(nest_sf)
st_crs(crstracks) # UTM

nest_sf_reproj <- st_transform(nest_sf, crs = crstracks) # reproject from WGS 84 to UTM- for correctly measuring buffer
data_sf_reproj <- st_transform(data_sf, crs = crstracks) 

st_crs(data_sf_reproj) == st_crs(nest_sf_reproj) # check if reprojected objects are the same
st_crs(data_sf_reproj)
st_crs(nest_sf_reproj)

st_crs(NCA_Shape) # UTM

crsgoogle <- 3857 # EPSG 3857 (Pseudo-Mercator, what Google uses)

#########################     Plot All Nest Sites    ###########################
#................    Plot NCA and nest sites over base map    ..................

NCA_google <- get_map( location = c(lon = -116.1267, lat = 43.05984), 
                       maptype = "terrain", source = "google", zoom = 9 )
# plot map
ggmap(NCA_google)

# Use function to assign correct google map CRS for plotting with geom_sf( )
NCA_google_x <- ggmap_bbox(NCA_google)

NCA_Shape_r <- st_transform(NCA_Shape, crs = crsgoogle) # Transform to EPSG 3857
nest_r <- st_transform(nest_sf, crs = crsgoogle)

# Plot NCA Shape and nest locations over google map
ggmap(NCA_google_x) + 
  coord_sf(crs = crsgoogle) + # force the ggplot2 map to be in 3857
  geom_sf(data = NCA_Shape_r, size = 1.5, color = "black", fill = NA, inherit.aes = FALSE) +
  stat_sf_coordinates(data = nest_r, inherit.aes = FALSE)

#########################     Buffer Nest Sites    #############################
#...................     Identify points within buffer    ......................

nest_buffer <- st_buffer(nest_sf_reproj, 250) # place 200 m buffer around nest sites
nest_pts <- st_intersection(nest_buffer, data_sf_reproj) # create sf object of all points within buffer

# Plot NCA + nest buffers + points within each buffer
ggplot() + 
   geom_sf(data = NCA_Shape, size = 1.5, color = "black", fill = NA) +
   geom_sf(data = nest_buffer, size = 1, color = "black", fill = "cyan1") +
   stat_sf_coordinates(data = nest_pts) +
   ggtitle("NCA") + 
   coord_sf()

######################     Buffer 1 Individual Nest    #########################
#...........     Buffer and plot 1 nest for visual inspection    ...............

CRW_site <- datadf %>%
  dplyr::group_by(territory) %>%
  dplyr::select(., y, x, territory) %>% # keep id and territory columns (and all rows)
  dplyr::filter( territory == "CRW" )

CRW_site <- CRW_site[!duplicated(CRW_site$y),] # remove duplicates (keeping only unique rows)

CRW_site_sf <- st_as_sf(CRW_site, coords = c("x", "y"), crs = crsdata) # convert from data.frame to sf object

CRW_sf_reproj <- st_transform(CRW_site_sf, crs = crstracks) # reproject from WGS 84 to UTM- for correctly measuring buffer

CRW_buffer <- st_buffer(CRW_sf_reproj, 250) # buffer 250 m
CRW_pts <- st_intersection(CRW_buffer, data_sf_reproj) # create sf object of all points within buffer

# Plot nest buffer + points within each buffer
ggplot() + 
  geom_sf(data = NCA_Shape, size = 1.5, color = "black", fill = NA) +
  geom_sf(data = CRW_buffer, size = 1, color = "black", fill = "cyan1") +
  stat_sf_coordinates(data = CRW_pts) +
  ggtitle("CRW") + 
  coord_sf(xlim = c(591500, 593000), ylim = c(4782000, 4783000), expand = FALSE)

CRW_buff_reproj <- st_transform(CRW_buffer, crs = crsgoogle) # Transform to EPSG 3857 (Pseudo-Mercator, what Google uses)
CRW_pts_reproj <- st_transform(CRW_pts, crs = crsgoogle)
st_crs(CRW_buff_reproj)

# Get google map using CRW nest location
CRW_google <- get_map( location = c(lon = -115.8619, lat = 43.18981), 
                       maptype = "terrain", source = "google", zoom = 14 )
# Plot map
ggmap(CRW_google)

# Use function to assign correct google map CRS for plotting with geom_sf( )
CRW_google_x <- ggmap_bbox(CRW_google)

# Plot nest buffer over google map
ggmap(CRW_google_x) + 
  coord_sf(crs = crsgoogle) + # force the ggplot2 map to be in 3857
  geom_sf(data = CRW_buff_reproj, size = 1, color = "black", fill = NA, inherit.aes = FALSE) +
  stat_sf_coordinates(data = CRW_pts_reproj, inherit.aes = FALSE)



save.image("nest_site_bufferpts.RData")




