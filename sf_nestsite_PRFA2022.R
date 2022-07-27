
############################  NEST BUFFER SCRIPT  ##############################
#.......................       Using SF Package      ...........................

# load packages relevant to this script:

library(sf)

rm( list = ls() )
load( "nest_site_bufferpts.RData" )

##############################  Load PRFA Data  ################################
#.............. Data Extracted From 'datadf' in Cleaning Script   ..............

nest_site <- datadf %>%
  group_by(territory) %>%
  select(., y, x, territory) # keep id and territory columns (and all rows)

nest_site <- nest_site[!duplicated(nest_site$y),] # remove duplicates (keeping only unique rows)
class(nest_site)

#########################   Define CRS/Projection  #############################
#.....................      Reproject data into UTM    .........................

nest_sf <- st_as_sf(nest_site, coords = c("x", "y"), crs = crsdata) # convert from data.frame to sf object
data_sf <- st_as_sf(datadf, coords = c("lon", "lat"), crs = crsdata)

st_crs(data_sf) == st_crs(nest_sf) # check if projections are the same
st_crs(data_sf) # check projection
st_crs(nest_sf)
st_crs(crstracks)
class(crstracks)

nest_sf_reproj <- st_transform(nest_sf, crs = crstracks) # reproject from WGS 84 to UTM
data_sf_reproj <- st_transform(data_sf, crs = crstracks) 

st_crs(data_sf_reproj) == st_crs(nest_sf_reproj) # check if reprojected CRS are the same
st_crs(data_sf_reproj)
st_crs(nest_sf_reproj)

#########################     Buffer Nest Sites    #############################
#...................     Identify points within buffer    ......................

nest_buffer <- st_buffer(nest_sf_reproj, 200) # place 200 m buffer around nest sites
nest_pts <- st_intersection(nest_buffer, data_sf_reproj) # create sf object of all points within buffer

st_crs(NCA_Shape)
st_geometry_type(nest_pts)

# Plot NCA + nest buffers + points within each buffer
ggplot() + 
   geom_sf(data = NCA_Shape, size = 1.5, color = "black", fill = NA) +
   geom_sf(data = nest_buffer, size = 1, color = "black", fill = "cyan1") +
   stat_sf_coordinates(data = nest_pts) +
   ggtitle("NCA") + 
   coord_sf()


#...............................................................................


CRW_site <- datadf %>%
  group_by(territory) %>%
  select(., y, x, territory) %>% # keep id and territory columns (and all rows)
  filter( territory == "CRW" )
CRW_site <- CRW_site[!duplicated(CRW_site$y),] # remove duplicates (keeping only unique rows)
CRW_site_sf <- st_as_sf(CRW_site, coords = c("x", "y"), crs = crsdata)
CRW_sf_reproj <- st_transform(CRW_site_sf, crs = crstracks)
CRW_buffer <- st_buffer(CRW_sf_reproj, 200)
CRW_pts <- st_intersection(CRW_buffer, data_sf_reproj)
ggplot() + 
  geom_sf(data = CRW_buffer, size = 1, color = "black", fill = "cyan1") +
  stat_sf_coordinates(data = CRW_pts) +
  ggtitle("CRW") + 
  coord_sf()

save.image("nest_site_bufferpts.RData")




