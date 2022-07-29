
############################  NEST BUFFER SCRIPT  ##############################
#.......................       Using SF Package      ...........................

# load packages relevant to this script:

library(sf)
library(ggplot2)
library(ggmap)
library(dplyr)
# library(raster)

rm( list = ls() )

#######################  SPATIAL-RELATED REQUIREMENTS  #########################
#........... Google API key for ggmap & function to plot google map   ..........

register_google(key = "key_goes_here", write = TRUE)
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

# load( "cleaningPRFA22.RData" )
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

#........................     Small home range    ..............................

CRW_site <- datadf %>%
  dplyr::group_by(territory) %>%
  dplyr::select(., y, x, territory) %>% # keep id and territory columns (and all rows)
  dplyr::filter( territory == "CRW" )

CRW_site <- CRW_site[!duplicated(CRW_site$y),] # remove duplicates (keeping only unique rows)

CRW_site_sf <- st_as_sf(CRW_site, coords = c("x", "y"), crs = crsdata) # convert from data.frame to sf object

CRW_sf_reproj <- st_transform(CRW_site_sf, crs = crstracks) # reproject from WGS 84 to UTM- for correctly measuring buffer

CRW_buffer <- st_buffer(CRW_sf_reproj, 250) # buffer 250 m
CRW_pts <- st_intersection(CRW_buffer, data_sf_reproj) # create sf object of all points within buffer
# check about data_sf_reproj

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

#........................     Large home range    ..............................

CFR_site <- datadf %>%
  dplyr::group_by(territory) %>%
  dplyr::select(., y, x, territory) %>% # keep id and territory columns (and all rows)
  dplyr::filter( territory == "CFR" )

CFR_site <- CFR_site[!duplicated(CFR_site$y),] # remove duplicates (keeping only unique rows)

CFR_site_sf <- st_as_sf(CFR_site, coords = c("x", "y"), crs = crsdata) # convert from data.frame to sf object

CFR_sf_reproj <- st_transform(CFR_site_sf, crs = crstracks) # reproject from WGS 84 to UTM- for correctly measuring buffer

CFR_buffer <- st_buffer(CFR_sf_reproj, 1000) # buffer # m
CFR_pts <- st_intersection(CFR_buffer, data_sf_reproj) # create sf object of all points within buffer
# check about data_sf_reproj
st_bbox(CFR_buffer) # bounding box for buffer

# Plot nest buffer + points within each buffer
ggplot() + 
  geom_sf(data = CFR_buffer, size = 1, color = "black", fill = "cyan1") +
  stat_sf_coordinates(data = CFR_pts) +
  ggtitle("CFR") + 
  coord_sf(xlim = c(593339.5, 593839.5), ylim = c(4757257.4, 4757757.4), expand = FALSE) # buffer bbox as x & y limits

CFR_buff_reproj <- st_transform(CFR_buffer, crs = crsgoogle) # Transform to EPSG 3857 (Pseudo-Mercator, what Google uses)
CFR_pts_reproj <- st_transform(CFR_pts, crs = crsgoogle)
st_crs(CFR_buff_reproj)
st_crs(CFR_pts_reproj)
st_crs(CFR_buff_reproj) == st_crs(CFR_pts_reproj)

# Get google map using CRW nest location
CFR_google <- get_map( location = c(lon = -115.8524, lat = 42.96446), 
                       maptype = "terrain", source = "google", zoom = 14 )
# Plot map
ggmap(CFR_google)

# Use function to assign correct google map CRS for plotting with geom_sf( )
CFR_google_x <- ggmap_bbox(CFR_google)

# Plot nest buffer over google map
ggmap(CFR_google_x) + 
  coord_sf(crs = crsgoogle) + # force the ggplot2 map to be in 3857
  geom_sf(data = CFR_buff_reproj, size = 1, color = "black", fill = NA, inherit.aes = FALSE) +
  stat_sf_coordinates(data = CFR_pts_reproj, inherit.aes = FALSE)

###########     Buffer and assign points IN or OUT of territory    #############
#..... Buffer 750m, keep pts outside of buffer, plot for visual inspection .....
#.......       Repeat for all individuals in entire dataframe       ............

# all CFR gps points: 
CFR_pts_all <- datadf %>%
  dplyr::filter( territory == "CFR" )

CFR_pts_all_sf <- st_as_sf(CFR_pts_all, coords = c("lon", "lat"), crs = crsdata)
CFR_all_reproj <- st_transform(CFR_pts_all_sf, crs = crstracks) # UTM
head(CFR_all_reproj)

CFR_nest <- datadf %>%
  dplyr::group_by(territory) %>%
  dplyr::select(., y, x, territory) %>% # keep id and territory columns (and all rows)
  dplyr::filter( territory == "CFR" )

CFR_nest <- CFR_nest[!duplicated(CFR_nest$y),] # remove duplicates (keeping only unique rows)
CFR_nest_sf <- st_as_sf(CFR_nest, coords = c("x", "y"), crs = crsdata) # convert from data.frame to sf object
CFR_nest_reproj <- st_transform(CFR_nest_sf, crs = crstracks) # reproject from WGS 84 to UTM- for correctly measuring buffer

CFR750m <- st_buffer(CFR_nest_reproj, 750) # 750 m buffer around nest site
# st_intersection(CFR_buffer, data_sf_reproj)
CFR2km <- st_buffer(CFR_nest_reproj, 2000) # 2000 m buffer around nest site, for plotting

# CFR_all_reproj$CFR_int_pts <- st_intersects(CFR_all_reproj, CFR750m)
# head(CFR_all_reproj)
# str(CFR_all_reproj)

int_buff <- lengths(st_intersects(CFR_all_reproj, CFR750m)) > 0
# NCA <- lengths(st_intersects(CFR_all_reproj, CFR750m)) < 1

# This gives us TRUE/FALSE vectors telling us if the corresponding pts element is in each set of polygons. 
# Logic then says:

terr <- int_buff
out <- !int_buff
# str(terr)
# str(out)

# Now we can plot subsets of the points in different colours over the polygon geometry:
  
# visualize and check:

st_bbox(CFR2km)

ggplot() + 
  geom_sf(data = CFR750m, size = 1, color = "black", fill = NA) +
  stat_sf_coordinates(data = CFR_all_reproj[terr,], color = "blue") +
  stat_sf_coordinates(data = CFR_all_reproj[out,], color = "red") +
  coord_sf(xlim = c( 591589.5 , 595589.5 ), ylim = c( 4755507.4 , 4759507.4 ), expand = FALSE) +
  ggtitle("CFR")

# Keep all rows with points outside of territory

CFR_pts_out <- CFR_all_reproj[out, ]
head(CFR_pts_out)

ggplot() + 
  geom_sf(data = CFR750m, size = 1, color = "black", fill = NA) +
  stat_sf_coordinates(data = CFR_pts_out, color = "blue") +
  coord_sf(xlim = c( 591589.5 , 595589.5 ), ylim = c( 4755507.4 , 4759507.4 ), expand = FALSE) +
  ggtitle("CFR")

# plot with basemap

# Get google map using CRW nest location
CFR_google_z13 <- get_map( location = c(lon = -115.8524, lat = 42.96446), 
                       maptype = "terrain", source = "google", zoom = 13 )
# Plot map
ggmap(CFR_google_z13)

# Use function to assign correct google map CRS for plotting with geom_sf( )
CFR_google_xz13 <- ggmap_bbox(CFR_google_z13)

CFR_750_pm <- st_transform(CFR750m, crs = crsgoogle) # Transform to EPSG 3857 (Pseudo-Mercator, what Google uses)
CFR_out_pm <- st_transform(CFR_pts_out, crs = crsgoogle)

# Plot nest buffer over google map
# note using google basemap with zoom=13 will remove all points outside of zoom extent
# to view more points with larger extent, decrease zoom

ggmap(CFR_google_xz13) + 
  coord_sf(crs = crsgoogle) + # force the ggplot2 map to be in 3857
  geom_sf(data = CFR_750_pm, size = 1, color = "black", fill = NA, inherit.aes = FALSE) +
  stat_sf_coordinates(data = CFR_out_pm, color = "blue", inherit.aes = FALSE) +
  ggtitle("CFR")

save.image("nest_site_bufferpts.RData")




