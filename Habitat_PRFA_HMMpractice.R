

################ PRFA HMM ANALYSIS W/ Habitat TEST SCRIPT ######################
#.............   Testing HMM ~habitat with all individuals   ...................
#.........       Using momentuHMM package & habitat rasters        .............

# load packages:
library( tidyverse ) 
library( raster )
library( terra )
library( rasterVis )
library( RColorBrewer )
library( sf )
library( momentuHMM )

# Clean workspace

rm( list = ls() )

# Load data
load("Hab_PRFA_HMMpractice.RData")

PRFA_alt_df <- read_rds("PRFA_alt_df")

# load NCA shapefile 
NCA <- sf::st_read( "Z:/Common/QCLData/Habitat/NCA/GIS_NCA_IDARNGpgsSampling/BOPNCA_Boundary.shp", 
                    quiet = TRUE)
# set path to habitat data 
habpath <- "Z:/Common/QCLData/Habitat/NLCD_new/NCA_raster_summaries_300m/"
# load habitat change files
HabFiles <- list.files( habpath, full.names = TRUE )
# Percent cover in 2020
c20 <- raster::stack( paste0(habpath, "c20_agg.img" ) )
# raster stack? Is it more that 1 file?
# layer names not kept, so have to rename
habnames <- c( "non_sagebrush", "sagebrush", "annual_herbaceous", 
               "perennial_herbaceous" )
names(c20) <- habnames 
# view
c20

# separate raster layers in raster stack
unstack(c20) # view stack layers
non_sage_rast <- unstack(c20)[[1]]
crs(peren_herb_rast)
sage_rast <- unstack(c20)[[2]]
ann_herb_rast <- unstack(c20)[[3]]
peren_herb_rast <- unstack(c20)[[4]]

# Reproject site locations to crs of habitat rasters for extraction

crstracks <- st_crs(NCA) # create object of NCA shapefile crs
# convert PRFA dataframe to sf object and assign correct crs
PRFA_df_sf <- st_as_sf(PRFA_alt_df, crs = crstracks, coords = c("easting", "northing"))
class(PRFA_df_sf) # check 
colnames(PRFA_df_sf)
head(PRFA_df_sf)

# reproject habitat raster stack to UTM
c20_utm <- projectRaster(c20, crs = crs(NCA))
st_crs(c20_utm) == st_crs(PRFA_df_sf)
terra::plot(c20_utm)
class(c20_utm)

# reproject each habitat layer raster to UTM
nonsage_utm <- projectRaster(non_sage_rast, crs = crs(NCA))
sage_utm <- projectRaster(sage_rast, crs = crs(NCA))
annherb_utm <- projectRaster(ann_herb_rast, crs = crs(NCA))
perenherb_utm <- projectRaster(peren_herb_rast, crs = crs(NCA))
spatialCov <- list(nonsage = nonsage_utm, sage = sage_utm, annherb = annherb_utm, perenherb = perenherb_utm)

# extract habitat values at all points:
cover <- raster::extract( x = c20_utm, y = PRFA_df_sf, method = "simple" )
# view
head( cover )
colnames( cover ) <- paste( "cover", habnames, sep = "_" )
table(is.na(cover[]))

pts_geo_unlist <- PRFA_df_sf %>%
  mutate(., x = unlist(map(PRFA_df_sf$geometry,1)), y = unlist(map(PRFA_df_sf$geometry,2))) # separate geometry column
pts_dropgeo <- st_drop_geometry(pts_geo_unlist) # remove geometry column
head(pts_dropgeo)
class(pts_dropgeo)

PRFA_hab_df <- pts_dropgeo %>% 
  dplyr::select( ID, t_, x, y )

class(PRFA_hab_df)
head(PRFA_hab_df)

?prepData

PRFA_hab_prep <- prepData(PRFA_hab_df, type = "UTM", coordNames = c("x","y"),
                          spatialCovs = spatialCov)
# Error in prepData.default(PRFA_hab_df, type = "UTM", coordNames = c("x",  : 
#   missing values are not permitted in spatialCovs$nonsage
table(is.na(nonsage_utm[]))

head(PRFA_hab_prep)
summary( PRFA_hab_prep$angle )
summary( PRFA_hab_prep$step )

#########                     Fit Habitat Model                         ########
#.........           Using momentuHMM::fitHMM() function           .............
#.........     Hab predictor for states only, not transitions      .............


stateNames <- c("forage","travel")

#..........                        BREAKDOWN:                         ..........
#..........                   HMM Model Formula:                      ..........

# Model Components:

# States = 2

# Add predictors to the model:
# mean SL = int + slope*predictor
# sd SL = int + slope*predictor
# zeromass = int

# State1:

# Step_1 ~ gamma( mu_1, sd_1, zerommass_1)
# mu_1 = beta0 + beta1 * hab 
# sd_1 = beta0 + beta1 * hab 
# zeromass_1 = int

# Angle_1 ~ VonMises( mean_1, concentration_1)
# mean_1 = int 
# concentration_1 = beta0 + beta1 * hab 

# State2:

# Step_2 ~ gamma( mu_2, sd_2, zerommass_2)
# Mean_2 = beta0 + beta1 * hab
# sd_2 = beta0 + beta1 * hab
# zeromass_2 = int

# Angle_2 ~ VonMises( mean_2, concentration_2)
# mean_2 = int 
# concentration_2 = beta0 + beta1 * hab 

#####   Covariate Model m2_PRFA, Parameter Estimates for State Distribution  #####
#..........      Covariate Models, state distribution formula :        .........

# provide initial values for the coefficients in each state's STEP distribution
# ORDER: int_mean1, int_mean2, int_sd1,  int_sd2, int_zeromass1, int_zeromass2
stepPar0_hab <- c( 3.558848, 4.218419, 3.309423, 3.174193, -5.074119, -28.568315)
# values used were obtained from m1_PRFA null model output parameter estimates

# provide initial values for the coefficients in each state's ANGLE distribution
# ORDER: int_mean1, int_mean2, int_concentration1, beta1_concentration1, int_concentration2, beta1_concentration2)
anglePar0_hab <- c(9.254339e-03, 4.627725e-05, -6.153377e-01, 0, 2.834985e+00, 0)
# values used were obtained from m1_PRFA null model output parameter estimates
# 0 for beta1_concentration1 & beta1_concentration2 = no effect of alt

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx#

# required to define equations for the parameters in state distributions:
# since equations the same for each of two states, only need to list equation once:

# m2_PRFA model formula 2 states
# STEP ~ gamma( mu_int, sd_int, zerommass_int)
# ANGLE ~ VonMises( mean_int, concentration + scaled_alt )
stepDM_alt <- list( mean = ~1, sd = ~1, zeromass = ~1 )
angleDM_alt <- list( mean = ~1, concentration = ~1 + scaled_alt )

m2_PRFA <- momentuHMM::fitHMM(data = PRFA_alt_prep, nbStates = 2, dist = list( step = "gamma", angle = "vm" ),
                              Par0 = list(step = stepPar0_alt, angle = anglePar0_alt),
                              DM = list( step = stepDM_alt, angle = angleDM_alt  ), formula = ~1,
                              stationary = FALSE, estAngleMean = list( angle = TRUE ),
                              stateNames = stateNames)

plot(m2_PRFA)
print(m2_PRFA) # estimations
plotPR(m2_PRFA)


save.image("Hab_PRFA_HMMpractice.RData")








