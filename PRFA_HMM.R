

###################### PRFA HMM ANALYSIS TEST SCRIPT ###########################
#...............   Testing HMM ~alt with all individuals   .....................
#...............          Using momentuHMM package         .....................

# load packages relevant to this script:

library( tidyverse ) #easy data manipulation
library( amt )
library( sf )
library( ggmap )
library( momentuHMM )
library( dplyr )

getwd()

# Load or create data
# Clean your workspace to reset your R environment. #
rm( list = ls() )

##################          Load & View Data          ##########################

load( "PRFA_HMM.RData" )

head( PRFA_alt_prep )
summary( PRFA_alt_prep ) # Max step length is  Max. :320
class( PRFA_alt_prep )
plot( PRFA_alt_prep )
par(ask = FALSE) # turns of Hit <Return> to see next plot
hist( PRFA_alt_prep$alt )
hist( PRFA_alt_prep$step, breaks = 100, ylim = c(0,200) )
hist( PRFA_alt_prep$angle, breaks = 100 )
summary( PRFA_alt_prep$angle )
summary( PRFA_alt_prep$step )

unique( PRFA_alt_prep$ID )

# check the autocorrelation of the step lengths

acf( PRFA_alt_prep$step[ !is.na( PRFA_alt_prep$step) ], lag.max = 5000 )
acf( PRFA_alt_prep$step[ !is.na( PRFA_alt_prep$step) ], lag.max = 100 )
# acf looks low

#######################          FIT HMM          ##############################
#...........    All individuals using momentuHMM::fitHMM    ....................

# Similar to the formula argument for state transition probability modelling, it is
# through the DM argument of fitHMM that models are specified for the state-dependent
# probability distribution parameters for each data stream. DM is a list argument containing
# an element for each data stream, but each element itself is also a list specifying the
# design matrix formulas for each parameter.

# Note that when DM is specified for a data stream, the initial parameter values (Par0)
# for that data stream now correspond to columns of the resulting design matrix and must
# be on the working scale instead of the natural scale. In this case, because the log link is
# used for the natural parameters of the gamma distribution, Par0$step was specified on
# the log scale. The functions getPar, getPar0, checkPar0, and getParDM are designed
# to assist users in the specification of design matrices and corresponding initial values on
# the working scale for any given model (see package documentation for further details).

# Model example:
# fitHMM(data = data,
## number of behavioral state to estimate
# nbStates = 2,
## prob distributions of data streams
## aka for step length and turning angle
# dist = list( step = "gamma", angle = "vm" ),
## Weibull dist not meant for data w/ SL = 0
## starting values for distribution parameters for each data stream
# Par0 = list(step = stepPar0, angle = anglePar0),
## design matrix formula for each parameter in state distribution
# DM = list( step = stepDM, angle = angleDM  ),
## regression formula for transition probability covariates
## ~1 = estimate intercept value only
# formula = ~1,
## FALSE if we include time-varying covariates (e.g. alt)
# stationary = FALSE,
## estimate angle mean for turning angle to use a circular angle mean
# estAngleMean = list( angle = TRUE ),
## setting it to TRUE is same of setting circularAngleMean to 1
## names can be given for each state in data
# stateNames = stateNames
# )

########   Initial Parameter Values for Gamma & VonMises Distributions  ########
#........                     Michelot et al. (2016)                     .......

# What is natural scale?

#..........                        BREAKDOWN:                         ..........
#..........                   HMM Model Formula:                      ..........

# Model Components:

# States = 2

# State 1:
# Step_1 ~ gamma( mu_1, sd_1, zerommass_1 )
# Angle_1 ~ VonMises( mean_1, concentration_1 )

# State 2:
# Step_2 ~ gamma( mu_2, sd_2, zerommass_2 )
# Angle_2 ~ VonMises( mean_2, concentration_2 )

#### Null model Initial Parameter Values for Gamma & VonMises Distributions ####
#..........               Null Model Initial Values:                  ..........

# State 1 and 2 STEP Distribution:
# Step_1 ~ gamma( mu_1, sd_1, zerommass_1 )     # mu_1 = int, sd_1 = int, zeromass_1 = int
# Step_2 ~ gamma( mu_2, sd_2, zerommass_2 )     # mu_2 = int, sd_2 = int, zeromass_2 = int

#..........                      m1_PRFA Model                           .........
# provide initial values for the coefficients in each state's STEP distribution
# ORDER:   int_mu1,  int_mu2,  int_sd1, int_sd2,  int_zeromass1, int_zeromass2:

stepPar0 <- c( 3.508092, 4.251077, 3.405863, 3.205991, -4.592400, -28.568315)
# values used were obtained from SG HMM null model output parameter estimates

# State 1 and 2 ANGLE Distribution:
# Angle_1 ~ VonMises( mu_1, concentration_1 )   # mean_1 = int, concentration_1 = int
# Angle_2 ~ VonMises( mu_2, concentration_2 )   # mean_2 = int, concentration_2 = int

# provide initial values for the coefficients in each state's ANGLE distribution
# ORDER:   int_mean1,  int_mean2,  int_concentration1, int_concentration2

anglePar0 <- c( 0.0023978079, 0.0001732104, -0.4206254134, 2.7229227455 )
# values used were obtained from SG HMM null model output parameter estimates

#########    Null Model, Parameter Estimates for State Distribution   ##########
#..........       state distribution formula (intercept only):        ..........

# required to define equations for the parameters in state distributions:
# since equations the same for each of two states, only need to list equation once:

# m1_PRFA model formula 2 states
# STEP ~ gamma( mu_int, sd_int, zerommass_int)
# ANGLE ~ VonMises( mean_int, concentration_int )
stepDM <- list( mean = ~1, sd = ~1, zeromass = ~1 ) # ~1 = estimates intercept value
angleDM <- list( mean = ~1, concentration = ~1 )

#########                      Fit Null Model                           ########
#.........           Using momentuHMM::fitHMM() function      ..................

m1_PRFA <- momentuHMM::fitHMM( data = PRFA_alt_prep, nbStates = 2, 
                             dist = list( step = "gamma", angle = "vm" ),
                             Par0 = list(step = stepPar0, angle = anglePar0),
                             DM = list( step = stepDM, angle = angleDM  ), formula = ~1,
                             stationary = FALSE, estAngleMean = list( angle = TRUE ) )

# view results
plot( m1_PRFA )
print( m1_PRFA ) # estimations
plotPR( m1_PRFA ) # checking residuals, but not sure what I'm looking at

#########                     Fit Altitude Models                       ########
#.........           Using momentuHMM::fitHMM() function           .............
#.........     Alt predictor for states only, not transitions      .............


stateNames <- c("forage","travel")

# Insert null model estimates into model with covariates using momentuHMM::getPar0()

Par0 <- momentuHMM::getPar0(model=m1_PRFA, nbStates=2,
                            DM=list(step = stepDM, angle= angleDM),
                            formula = ~1, stationary = FALSE, 
                            estAngleMean=list(angle=TRUE))

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
# mu_1 = beta0 + beta1 * alt 
# sd_1 = beta0 + beta1 * alt 
# zeromass_1 = int

# Angle_1 ~ VonMises( mean_1, concentration_1)
# mean_1 = int 
# concentration_1 = beta0 + beta1 * alt 

# State2:

# Step_2 ~ gamma( mu_2, sd_2, zerommass_2)
# Mean_2 = beta0 + beta1 * alt
# sd_2 = beta0 + beta1 * alt
# zeromass_2 = int

# Angle_2 ~ VonMises( mean_2, concentration_2)
# mean_2 = int 
# concentration_2 = beta0 + beta1 * alt 

#####   Covariate Model m2_PRFA, Parameter Estimates for State Distribution  #####
#..........      Covariate Models, state distribution formula :        .........

# provide initial values for the coefficients in each state's STEP distribution
# ORDER: int_mean1, int_mean2, int_sd1,  int_sd2, int_zeromass1, int_zeromass2
stepPar0_alt <- c( 3.558848, 4.218419, 3.309423, 3.174193, -5.074119, -28.568315)
# values used were obtained from m1_PRFA null model output parameter estimates

# provide initial values for the coefficients in each state's ANGLE distribution
# ORDER: int_mean1, int_mean2, int_concentration1, beta1_concentration1, int_concentration2, beta1_concentration2)
anglePar0_alt <- c(9.254339e-03, 4.627725e-05, -6.153377e-01, 0, 2.834985e+00, 0)
# values used were obtained from m1_PRFA null model output parameter estimates
# 0 for beta1_concentration1 & beta1_concentration2 = no effect of alt

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

#####   Covariate Model m3_PRFA, Parameter Estimates for State Distribution  #####
#..........      Covariate Models, state distribution formula :        .........

# FULL MODEL:
# stepDM_alt <- list( mean = ~1 + scaled_alt, sd = ~1 + scaled_alt, zeromass = ~1 )
# int_mean1, beta1_mean1, int_mean2, beta1_mean2, int_sd1,  beta1_sd1, int_sd2, beta1_sd2, int_zeromass1, int_zeromass2
# angleDM_alt <- list( mean = ~1, concentration = ~1 + scaled_alt )
# int_mean1, int_mean2, int_concentration1, beta1_concentration1, int_concentration2, beta1_concentration2)


# provide initial values for the coefficients in each state's STEP distribution
# int_mean1, beta1_mean1, int_mean2, beta1_mean2, int_sd1,  beta1_sd1, int_sd2, beta1_sd2, int_zeromass1, int_zeromass2
stepPar0_alt_full <- c( 3.558848, 0, 4.218419, 0, 3.309423, 0, 3.174193, 0, -5.074119, -28.568315)
# values used were obtained from m1_PRFA null model output parameter estimates
# 0 for beta1_mean1, beta1_mean2, beta1_sd1, beta1_sd2 = no effect of alt

# provide initial values for the coefficients in each state's ANGLE distribution
# int_mean1, int_mean2, int_concentration1, beta1_concentration1, int_concentration2, beta1_concentration2
anglePar0_alt_full <- c(9.254339e-03, 4.627725e-05, -6.153377e-01, 0, 2.834985e+00, 0)
# values used were obtained from m1_PRFA null model output parameter estimates
# 0 for beta1_concentration1 & beta1_concentration2 = no effect of alt

# m3_SG FULL MODEL formula 2 states
# STEP ~ gamma( mu + scaled_alt, sd + scaled_alt, zerommass_int)
# ANGLE ~ VonMises( mean_int, concentration + scaled_alt )
stepDM_alt_full <- list( mean = ~1 + scaled_alt, sd = ~1 + scaled_alt, zeromass = ~1 )
angleDM_alt_full <- list( mean = ~1, concentration = ~1 + scaled_alt )

m3_PRFA <- momentuHMM::fitHMM(data = PRFA_alt_prep, nbStates = 2, dist = list( step = "gamma", angle = "vm" ),
                            Par0 = list(step = stepPar0_alt_full, angle = anglePar0_alt_full),
                            DM = list( step = stepDM_alt_full, angle = angleDM_alt_full  ), formula = ~1,
                            stationary = FALSE, estAngleMean = list( angle = TRUE ),
                            stateNames = stateNames)

plot(m3_PRFA)
print(m3_PRFA) # estimations
plotPR(m3_PRFA)

########################         END OF SCRIPT       ###########################

save.image( "PRFA_HMM.RData" )


























