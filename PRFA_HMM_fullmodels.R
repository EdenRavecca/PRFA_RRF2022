

############## PRFA HMM ANALYSIS with ALL Habitat Covariates ###################
#.........           HMM ~habitat with all individuals             .............
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
load("PRFA_HMM_Habfull.RData")

# PRFA_hab_prep <- read_rds("PRFA_hab_prep")
# PRFA_habsc_prep <- read_rds("PRFA_habsc_prep")
# m4_PRFA <- read_rds("m4_PRFA")

# load NCA shapefile 
NCA <- sf::st_read( "Z:/Common/QCLData/Habitat/NCA/GIS_NCA_IDARNGpgsSampling/BOPNCA_Boundary.shp", 
                    quiet = TRUE)

stateNames <- c("forage","travel")

# For Individual Random Effects
# add a new column to dataframe that identifies which individual each burst belongs to:
# unscaled predictors
head(PRFA_hab_prep)
library(stringr)
PRFA_hab_prep$Ind <-sapply(str_split(PRFA_hab_prep$ID, "_",  n = 2), `[`, 2)

PRFA_hab_prep$Ind <- sub("_.*", "", PRFA_hab_prep$ID) # Keep substring prior to "_" in ID column
unique(PRFA_hab_prep$Ind)
# scaled predictors
head(PRFA_habsc_prep)
PRFA_habsc_prep$Ind <- sub("_.*", "", PRFA_habsc_prep$ID)
unique(PRFA_habsc_prep$Ind)

table(PRFA_hab_prep$ID)
strunique(PRFA_hab_prep$Ind)

# Recall details from Habitat_PRFA_HMMpractice.R:
# spatialCovsc <- list(nonsage = nonsage_scaled, sage = sage_scaled, 
#                      annherb = annherb_scaled, perenherb = perenherb_scaled)
# PRFA_habsc_prep <- prepData(PRFA_hab_df, type = "UTM", coordNames = c("x","y"),
#                             spatialCovs = spatialCovsc)

#####   Covariate Model m4_PRFA, Parameter Estimates for State Distribution  #####
#..........      Covariate Models, state distribution formula :        .........
#..........                Non-scaled Habitat Rasters                  .........

# FULL MODEL:
# stepDM_hab_full <- list( mean = ~1 + sage + nonsage + annherb + perenherb, 
#                     sd = ~1 + sage + nonsage + annherb + perenherb, zeromass = ~1 )
# int_mean1, beta1_mean1, beta2_mean1, beta3_mean1, beta4_mean1, 
# int_mean2, beta1_mean2, beta2_mean2, beta3_mean2, beta4_mean2, 
# int_sd1,  beta1_sd1, beta2_sd1, beta3_sd1, beta4_sd1, 
# int_sd2, beta1_sd2, beta2_sd2, beta3_sd2, beta4_sd2,
# int_zeromass1, int_zeromass2

# angleDM_hab_full <- list( mean = ~1, concentration = ~1 + sage + nonsage + annherb + perenherb )
# int_mean1, int_mean2, 
# int_concentration1, beta1_concentration1, beta2_concentration1, beta3_concentration1, beta4_concentration1, 
# int_concentration2, beta1_concentration2, beta2_concentration2, beta3_concentration2, beta4_concentration2


# provide initial values for the coefficients in each state's STEP distribution
stepPar0_hab_full <- c( 3.558848, 0, 0, 0, 0, 4.218419, 0, 0, 0, 0, 
                        3.309423, 0, 0, 0, 0, 3.174193, 0, 0, 0, 0,
                        -5.074119, -28.568315)
# parameter starting values used were obtained from m1_PRFA null model output parameter estimates
# 0 for beta = no effect of habitat type

# provide initial values for the coefficients in each state's ANGLE distribution
anglePar0_hab_full <- c(9.254339e-03, 4.627725e-05, -6.153377e-01, 0, 0, 0, 0,
                        2.834985e+00, 0, 0, 0, 0)
# values used were obtained from m1_PRFA null model output parameter estimates
# 0 for beta = no effect of habitat type

# m3_SG FULL MODEL formula 2 states
# STEP ~ gamma( mu + sage + nonsage + annherb + perenherb, sd + sage + nonsage + annherb + perenherb, zerommass_int)
# ANGLE ~ VonMises( mean_int, concentration + sage + nonsage + annherb + perenherb )

stepDM_hab_full <- list( mean = ~1 + sage + nonsage + annherb + perenherb, 
                         sd = ~1 + sage + nonsage + annherb + perenherb, zeromass = ~1 )

angleDM_hab_full <- list( mean = ~1, concentration = ~1 + sage + nonsage + annherb + perenherb )

m4_PRFA <- momentuHMM::fitHMM(data = PRFA_hab_prep, nbStates = 2, dist = list( step = "gamma", angle = "vm" ),
                              Par0 = list(step = stepPar0_hab_full, angle = anglePar0_hab_full),
                              DM = list( step = stepDM_hab_full, angle = angleDM_hab_full  ), formula = ~1,
                              stationary = FALSE, estAngleMean = list( angle = TRUE ),
                              stateNames = stateNames)

plot(m4_PRFA)
print(m4_PRFA) # estimations
plotPR(m4_PRFA)

#####   Covariate Model m5_PRFA, Parameter Estimates for State Distribution  #####
#..........      Covariate Models, state distribution formula :        .........
#..........                  Scaled Habitat Rasters                    .........

# The only thing that's different here is momentuHMM::fitHMM(data = PRFA_habsc_prep)
# vs momentuHMM::fitHMM(data = PRFA_hab_prep)

# FULL MODEL:
# stepDM_hab_full <- list( mean = ~1 + sage + nonsage + annherb + perenherb, 
#                     sd = ~1 + sage + nonsage + annherb + perenherb, zeromass = ~1 )
# int_mean1, beta1_mean1, beta2_mean1, beta3_mean1, beta4_mean1, 
# int_mean2, beta1_mean2, beta2_mean2, beta3_mean2, beta4_mean2, 
# int_sd1,  beta1_sd1, beta2_sd1, beta3_sd1, beta4_sd1, 
# int_sd2, beta1_sd2, beta2_sd2, beta3_sd2, beta4_sd2,
# int_zeromass1, int_zeromass2

# angleDM_hab_full <- list( mean = ~1, concentration = ~1 + sage + nonsage + annherb + perenherb )
# int_mean1, int_mean2, 
# int_concentration1, beta1_concentration1, beta2_concentration1, beta3_concentration1, beta4_concentration1, 
# int_concentration2, beta1_concentration2, beta2_concentration2, beta3_concentration2, beta4_concentration2


# provide initial values for the coefficients in each state's STEP distribution
stepPar0_hab_full <- c( 3.558848, 0, 0, 0, 0, 4.218419, 0, 0, 0, 0, 
                        3.309423, 0, 0, 0, 0, 3.174193, 0, 0, 0, 0,
                        -5.074119, -28.568315)
# parameter starting values used were obtained from m1_PRFA null model output parameter estimates
# 0 for beta = no effect of habitat type

# provide initial values for the coefficients in each state's ANGLE distribution
anglePar0_hab_full <- c(9.254339e-03, 4.627725e-05, -6.153377e-01, 0, 0, 0, 0,
                        2.834985e+00, 0, 0, 0, 0)
# values used were obtained from m1_PRFA null model output parameter estimates
# 0 for beta = no effect of habitat type

# m3_SG FULL MODEL formula 2 states
# STEP ~ gamma( mu + sage + nonsage + annherb + perenherb, sd + sage + nonsage + annherb + perenherb, zerommass_int)
# ANGLE ~ VonMises( mean_int, concentration + sage + nonsage + annherb + perenherb )

stepDM_hab_full <- list( mean = ~1 + sage + nonsage + annherb + perenherb, 
                         sd = ~1 + sage + nonsage + annherb + perenherb, zeromass = ~1 )

angleDM_hab_full <- list( mean = ~1, concentration = ~1 + sage + nonsage + annherb + perenherb )

m5_PRFA <- momentuHMM::fitHMM(data = PRFA_habsc_prep, nbStates = 2, dist = list( step = "gamma", angle = "vm" ),
                              Par0 = list(step = stepPar0_hab_full, angle = anglePar0_hab_full),
                              DM = list( step = stepDM_hab_full, angle = angleDM_hab_full  ), formula = ~1,
                              stationary = FALSE, estAngleMean = list( angle = TRUE ),
                              stateNames = stateNames)

plot(m5_PRFA, plotTracks = FALSE)
print(m5_PRFA) # estimations
plotPR(m5_PRFA)

m6_PRFA <- momentuHMM::fitHMM(data = PRFA_habsc_prep, nbStates = 2, dist = list( step = "gamma", angle = "vm" ),
                              Par0 = list(step = stepPar0_hab_full, angle = anglePar0_hab_full),
                              DM = list( step = stepDM_hab_full, angle = angleDM_hab_full  ), 
                              formula = ~1 + sage + nonsage + annherb + perenherb,
                              stationary = FALSE, estAngleMean = list( angle = TRUE ),
                              stateNames = stateNames)

plot(m6_PRFA, plotTracks = FALSE)
print(m6_PRFA) # estimations
plotPR(m6_PRFA)

par(ask= FALSE)
plot(density( CircStats::rvm( n = 100, mean = 1.680747e+01, k = 0.08209443 )))

save.image("PRFA_HMM_Habfull.RData")
















