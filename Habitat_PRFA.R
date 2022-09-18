


# load packages:
library( tidyverse ) 
library( raster )
library( terra )
library( rasterVis )
library( RColorBrewer )
library( sf )
library( lubridate )
library( weathermetrics )
library( corrplot )


# Clean your workspace to reset your R environment.

rm( list = ls() )

load("Habitat_PRFA_workspace.RData")
PRFA_alt_df <- read_rds("PRFA_alt_df")

# set path to data 

habpath <- "Z:/Common/QCLData/Habitat/NLCD_new/NCA_raster_summaries_300m/"

# load NCA shapefile 
NCA <- sf::st_read( "Z:/Common/QCLData/Habitat/NCA/GIS_NCA_IDARNGpgsSampling/BOPNCA_Boundary.shp", 
                    quiet = TRUE)

# load habitat change files

HabFiles <- list.files( habpath, full.names = TRUE )

# Percent cover in 2020

c20 <- raster::stack( paste0(habpath, "c20_agg.img" ) )

# Year of last change

lc <- raster::stack( paste0(habpath, "lc_agg.img" ) )

# Mode slope 1985-2020 at 300 m resolution

slps_mode <- raster::stack( paste0(habpath, "slps_mode_agg.img" ) )

# Mean slope 1985-2020 at 300 m resolution

slps_mean <- raster::stack( paste0(habpath, "slps_mean_agg.img" ) )

# years from 2020 when max change occurred (regardless of direction)

since_maxchange  <- raster::stack( paste0(habpath, "yrs_since_max_change_mean.img" ) )

# value of max change over time, most common at 300m

maxchange_val<- raster::stack( paste0(habpath, "max_change_value_mean.img" ) )

###### load Andrii Zaiats model estimates

# gr300 <- raster::raster( 'Z:/Common/QCLData/Habitat/NCA/Shrub_model/data/NCA_1985_2018_buff_sage_300m.tif' )

################## clean data ################################
##### extract habitat layers at site level #############################

# layer names not kept so have to rename
habnames <- c( "non_sagebrush", "sagebrush", "annual_herbaceous", 
               "perennial_herbaceous" )

names(c20) <- habnames 
c20
terra::plot(c20)
names(lc) <- habnames 
names(slps_mode ) <- habnames
names( slps_mean) <- habnames 
names( since_maxchange) <- habnames
names( maxchange_val ) <- habnames
terra::plot( since_maxchange )

# transform site locations to match crs of habitat rasters for extraction
# now transform to predictor crs:
crstracks <- st_crs(NCA)
PRFA_df_sf <- st_as_sf(PRFA_alt_df, crs = crstracks, coords = c("easting", "northing"))
class(PRFA_df_sf)
colnames(PRFA_df_sf)
head(PRFA_df_sf)

pts_trans <- sf::st_transform( PRFA_df_sf, st_crs( c20 ) )
st_crs(c20) == st_crs(pts_trans)

# transform NCA layer for plotting
# create NCA polygon for raster vis
NCA_trans <- sf::st_transform( NCA, st_crs( c20 ) ) 

# Need to turn sf objects to spatial objects:
NCA_poly <- as(st_geometry(NCA_trans), Class="Spatial")

# extract habitat variables for all sites:
cover <- raster::extract( x = c20, y = pts_trans, method = "simple" )
# view
head( cover )
colnames( cover ) <- paste( "cover", habnames, sep = "_" )

lastchange <- raster::extract( x = lc, y = pts_trans, method = "simple" )
colnames( lastchange ) <- paste( "lc", habnames, sep = "_" )

smodes <- raster::extract( x = slps_mode, y = pts_trans, method = "simple" )
colnames( smodes ) <- paste( "mode", habnames, sep = "_" )

smeans <- raster::extract( x = slps_mean, y = pts_trans, method = "simple" )
colnames( smeans ) <- paste( "mean", habnames, sep = "_" )

# now years since max change

# years from 2020 since max change occurred
since_mc <- raster::extract( x = since_maxchange, y = pts_trans, 
                             method = "simple" )
colnames( since_mc ) <- paste( "since_mc", habnames, sep = "_" )

# value of max change experienced at 300 m in a given year
mc <- raster::extract( x = maxchange_val, y = pts_trans,
                       method = "simple" )
colnames( mc) <- paste( "mc", habnames, sep = "_" )

# Plot habitat change for NCA
# mean slopes averaged at 300 m, and through time for nca: 
smeanp <- rasterVis::levelplot( slps_mean ) +
  latticeExtra::layer(sp.lines( NCA_poly, col = "grey70",
                                lwd = 3 ) )       

# mean cover at 300 m, and through time for nca: 
coverp <- rasterVis::levelplot( c20 ) +
  latticeExtra::layer(sp.lines( NCA_poly, col = "grey70",
                                lwd = 3 ) )       


# Years from 2020 since max change occurred at 300 m: 
yr_scp <- rasterVis::levelplot( since_maxchange ) +
  latticeExtra::layer(sp.lines( NCA_poly, col = "grey70",
                                lwd = 3 ) )       

# Years from 2020 since max change occurred at 300 m: 
mcp <- rasterVis::levelplot( maxchange_val ) +
  latticeExtra::layer(sp.lines( NCA_poly, col = "grey70",
                                lwd = 3 ) )       

# mode of year of last change at 300 m, and through time for NCA: 
rasterVis::levelplot( lc ) +
  latticeExtra::layer(sp.lines( NCA_poly, col = "grey70",
                                lwd = 3 ) )       

# plot histograms of habitat layers at NCA
par( mfrow = c(2,2))
# plot density plots
# for mean slopes 
histogram( slps_mean, xlim = c(-100,100) )
# for 2020 mean cover
histogram( c20, xlim = c(0,50) )
# for last year of change:
histogram( lc, xlim = c(1980,2022), breaks = 300 )
# mode of slopes
histogram( slps_mode, xlim = c(-100,100) )

save.image("Habitat_PRFA_workspace.RData")

































