

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

crstracks <- st_crs(NCA) # create object of NCA shapefile crs
# convert PRFA dataframe to sf object and assign correct crs (assigning has to happen before reprojecting, otherwise crs is NA)
PRFA_df_sf <- st_as_sf(PRFA_alt_df, crs = crstracks, coords = c("easting", "northing"))
class(PRFA_df_sf) # check 
colnames(PRFA_df_sf)
head(PRFA_df_sf)

# reproject habitat raster stack to UTM
# momentuHMM prepData() requires data to be in lat/lon or UTM
# to ensure data (in UTM) and crs of rasters all match, I re-projected raster stack to UTM
c20_utm <- projectRaster(c20, crs = crs(NCA))
st_crs(c20_utm) == st_crs(PRFA_df_sf)
terra::plot(c20_utm)
class(c20_utm)

# reproject each habitat layer raster to UTM
nonsage_utm <- projectRaster(non_sage_rast, crs = crs(NCA))
sage_utm <- projectRaster(sage_rast, crs = crs(NCA))
annherb_utm <- projectRaster(ann_herb_rast, crs = crs(NCA))
perenherb_utm <- projectRaster(peren_herb_rast, crs = crs(NCA))

# extract habitat values at all points:
# if we want to use raster layers with values already extracted as spatial covariates, then
# after extracting from raster stack, I'll need to unstack rasters and create spatialCovs list.
# cover <- raster::extract( x = c20_utm, y = PRFA_df_sf, method = "simple" )
# view
# head( cover )
# colnames( cover ) <- paste( "cover", habnames, sep = "_" )
# table(is.na(cover[]))

pts_geo_unlist <- PRFA_df_sf %>%
  mutate(., x = unlist(map(PRFA_df_sf$geometry,1)), y = unlist(map(PRFA_df_sf$geometry,2))) # separate geometry column
pts_dropgeo <- st_drop_geometry(pts_geo_unlist) # remove geometry column
head(pts_dropgeo)
class(pts_dropgeo)

# Only keep columns that are not covariates (keep: ID (each burst is individual), time, location)
# Since spatial covariates are added separately
# If including non-spatial covariates, we'd keep those in the dataframe and later identify the covariates in
# momentuHMM::prepData(covNames = x)
PRFA_hab_df <- pts_dropgeo %>% 
  dplyr::select( ID, t_, x, y )

class(PRFA_hab_df)
head(PRFA_hab_df)

# momentuHMM::prepData() does not all spatial covariates to contain any missing values

# replace NA's in rasters with mean values for each habitat type
nonsage_utm[is.na(nonsage_utm[])] <- cellStats(nonsage_utm, mean) # 5.012766
table(is.na(nonsage_utm[])) # check
sage_utm[is.na(sage_utm[])] <- cellStats(sage_utm, mean) # 7.088749
table(is.na(sage_utm[])) # check
annherb_utm[is.na(annherb_utm[])] <- cellStats(annherb_utm, mean) # 14.45989
table(is.na(annherb_utm[])) # check
perenherb_utm[is.na(perenherb_utm[])] <- cellStats(perenherb_utm, mean) # 14.13447
table(is.na(perenherb_utm[])) # check

# scale raster values to ease processing when running models
nonsage_scaled <- scale(nonsage_utm, center=TRUE, scale=TRUE)
sage_scaled <- scale(sage_utm, center=TRUE, scale=TRUE)
annherb_scaled <- scale(annherb_utm, center=TRUE, scale=TRUE)
perenherb_scaled <- scale(perenherb_utm, center=TRUE, scale=TRUE)
table(is.na(perenherb_scaled[])) # check

?prepData

# create a list of all spatial covariates (each habitat raster)
spatialCov <- list(nonsage = nonsage_utm, sage = sage_utm, annherb = annherb_utm, perenherb = perenherb_utm)

PRFA_hab_prep <- prepData(PRFA_hab_df, type = "UTM", coordNames = c("x","y"),
                          spatialCovs = spatialCov)

head( PRFA_hab_prep )
summary( PRFA_hab_prep$angle )
summary( PRFA_hab_prep$step )

spatialCovsc <- list(nonsage = nonsage_scaled, sage = sage_scaled, 
                     annherb = annherb_scaled, perenherb = perenherb_scaled)

PRFA_habsc_prep <- prepData(PRFA_hab_df, type = "UTM", coordNames = c("x","y"),
                          spatialCovs = spatialCovsc)

head( PRFA_habsc_prep )
summary( PRFA_habsc_prep$angle )
summary( PRFA_habsc_prep$step )

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
# 0 for beta1_concentration1 & beta1_concentration2 = no effect of habitat type

# required to define equations for the parameters in state distributions:
# since equations the same for each of two states, only need to list equation once:

# m2_PRFA model formula 2 states
# STEP ~ gamma( mu_int, sd_int, zerommass_int)
# ANGLE ~ VonMises( mean_int, concentration + sage )
stepDM_hab <- list( mean = ~1, sd = ~1, zeromass = ~1 )
angleDM_hab <- list( mean = ~1, concentration = ~1 + sage )

m2_PRFA <- momentuHMM::fitHMM(data = PRFA_hab_prep, nbStates = 2, dist = list( step = "gamma", angle = "vm" ),
                              Par0 = list(step = stepPar0_hab, angle = anglePar0_hab),
                              DM = list( step = stepDM_hab, angle = angleDM_hab  ), formula = ~1,
                              stationary = FALSE, estAngleMean = list( angle = TRUE ),
                              stateNames = stateNames)

plot(m2_PRFA)
print(m2_PRFA) # estimations
plotPR(m2_PRFA)

#####   Covariate Model m3_PRFA, Parameter Estimates for State Distribution  #####
#..........      Covariate Models, state distribution formula :        .........

# FULL MODEL:
# stepDM_sage <- list( mean = ~1 + sage, sd = ~1 + sage, zeromass = ~1 )
# int_mean1, beta1_mean1, int_mean2, beta1_mean2, int_sd1,  beta1_sd1, int_sd2, beta1_sd2, int_zeromass1, int_zeromass2
# angleDM_sage <- list( mean = ~1, concentration = ~1 + sage )
# int_mean1, int_mean2, int_concentration1, beta1_concentration1, int_concentration2, beta1_concentration2)


# provide initial values for the coefficients in each state's STEP distribution
# int_mean1, beta1_mean1, int_mean2, beta1_mean2, int_sd1,  beta1_sd1, int_sd2, beta1_sd2, int_zeromass1, int_zeromass2
stepPar0_sage_full <- c( 3.558848, 0, 4.218419, 0, 3.309423, 0, 3.174193, 0, -5.074119, -28.568315)
# values used were obtained from m1_PRFA null model output parameter estimates
# 0 for beta1_mean1, beta1_mean2, beta1_sd1, beta1_sd2 = no effect of sage

# provide initial values for the coefficients in each state's ANGLE distribution
# int_mean1, int_mean2, int_concentration1, beta1_concentration1, int_concentration2, beta1_concentration2
anglePar0_sage_full <- c(9.254339e-03, 4.627725e-05, -6.153377e-01, 0, 2.834985e+00, 0)
# values used were obtained from m1_PRFA null model output parameter estimates
# 0 for beta1_concentration1 & beta1_concentration2 = no effect of sage

# m3_SG FULL MODEL formula 2 states
# STEP ~ gamma( mu + sage, sd + sage, zerommass_int)
# ANGLE ~ VonMises( mean_int, concentration + sage )
stepDM_sage_full <- list( mean = ~1 + sage, sd = ~1 + sage, zeromass = ~1 )
angleDM_sage_full <- list( mean = ~1, concentration = ~1 + sage )

m3_PRFA <- momentuHMM::fitHMM(data = PRFA_hab_prep, nbStates = 2, dist = list( step = "gamma", angle = "vm" ),
                              Par0 = list(step = stepPar0_sage_full, angle = anglePar0_sage_full),
                              DM = list( step = stepDM_sage_full, angle = angleDM_sage_full  ), formula = ~1,
                              stationary = FALSE, estAngleMean = list( angle = TRUE ),
                              stateNames = stateNames)

plot(m3_PRFA)
print(m3_PRFA) # estimations
plotPR(m3_PRFA)


write_rds(PRFA_hab_prep, "PRFA_hab_prep")
write_rds(PRFA_habsc_prep, "PRFA_habsc_prep")

save.image("Hab_PRFA_HMMpractice.RData")








