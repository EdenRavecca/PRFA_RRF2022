
#..............................................................................#
#   Script developed by Jen Cruz to calculate home ranges                      # 
#   We rely on amt vignette here:                                              #
#   https://cran.r-project.org/web/packages/amt/vignettes/p2_hr.html           #
#   as well as: Signer & Fieberg (2021) PeerJ9:e11031                          #
#   http://doi.org/10.7717/peerj.11031                                         #
#..............................................................................#

########################## DEFINING TRACKS SCRIPT ##############################
#.......................    For HR Estimation AMT  .............................

# load packages relevant to this script:

library( tidyverse ) #easy data manipulation
library( amt )
library( sp )
library( lubridate ) #easy date manipulation
library( sf )

# Load or create data
# Clean your workspace to reset your R environment. #
rm( list = ls() )

# Set working directory. This is the path to your Rstudio folder for this 
# project. If you are in your correct Rstudio project then it should be:
# getwd()
# if so then:
workdir <- getwd()

# load workspace 
load( "cleaningPRFA22.RData" )
load( "TracksWorkspace22.RData" )


# We are now ready to make tracks using atm package
# We first check sample size

table( datadf$territory )
table(datadf$yr)

data_breeders <- datadf %>%
  group_by( territory ) %>%
  filter( !territory == "Owyhee" ) %>%
  ungroup()

unique(data_breeders$territory)

# We can also get an idea of the data collection for each individual
# by plotting histograms

# sampling dates

ggplot( datadf, aes( x = jday, color = as.factor(yr) ) ) +
  theme_classic( base_size = 15 ) +
  geom_histogram( ) +
  facet_wrap( ~ territory )

# speeds traveled (in knots)

ggplot( datadf, aes( x = speed, group = territory ) ) +
  theme_classic( base_size = 15 ) +
  geom_histogram( ) +
  facet_wrap( ~ territory )

#..............................................................................#
#......                                                                  ......#
#......   Creating tracks, calculating step lengths and turning angles   ......#
#......             for all individuals at the same time:                ......#
#......                                                                  ......#
#..............................................................................#

########################       CREATING TRACKS      ############################
#.......................    For HR Estimation AMT   ...........................#

# amt requires us to turn data into tracks for further analyses.
head(datadf)
head(data_breeders)

trks <- data_breeders %>% 
  # make track. Note you can add additional columns to it.
  amt::make_track(.y = lat, .x = lon, .t = ts, 
                  # define columns that you want to keep, relabel if you need:
                  id = id, sex = Sex, mth = mth,jday = jday, speed = speed, 
                  alt = alt, territory = territory, capt_loc = capt_loc,
                  date = date,
                  # assign correct crs
                  crs = crsdata )

# Reproject to UTM to convert lat lon to easting northing:

st_crs(crstracks)

trks <- amt::transform_coords( trks, crs_to = crstracks )

# Turn into a tibble list by grouping and nest by individual IDs:

trks <- trks %>%  amt::nest( data = -"territory" )

# view

trks

# Remember we have multiple types of data including detailed data for flights:
# 3 times a week, 20min fixes during the day, then hourly fixes during migration.
# We start by focusing on data during breeding season.
# That means we need to remove migration locations.
# How do we know when individuals started migrating North?
# Plot overall paths for each individual:
#######################>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!!!!!!!!!!!!!!!!!!!!!

for( i in 1:dim(trks)[1]){
  a <- as_sf_points( trks$data[[i]] ) %>% 
    ggplot(.) + theme_bw(base_size = 17) +
    labs( title = paste0('individual =', trks$territory[i]) ) +
    geom_sf(data = NCA_Shape, inherit.aes = FALSE ) +
    geom_sf() 
  print(a)
}

sf::st_bbox(NCA_Shape)

# Then use the Eastern & Northern coordinates to filter out data 

xmax <- as.numeric(st_bbox(NCA_Shape)$xmax) # 627081.5
ymax <- as.numeric(sf::st_bbox(NCA_Shape)$ymax)  + 10000

# subset those tracks less than as breeding and those > as migrating:

trks <- trks %>% mutate(
  breeding = map( data, ~ filter(., x_ < xmax, y_ < ymax, jday < 181 ) ),
  migrating = map( data, ~ filter(., x_ >= xmax, y_ >= ymax, jday >= 181 ) ) )

# view

trks

# Use info to report how many usable points we have during time of each inv @ NCA.
# Note we created two other groups of tibbles for the breeding season and migrating
# Plot step lengths

for( i in 1:dim(trks)[1]){
  a <-  steps( trks$breeding[[i]] ) %>% 
    # a <-  steps( trks$migrating[[i]] ) %>% 
    mutate( jday = lubridate::yday( t1_ ) ) %>% #what is t1_?
    group_by( jday ) %>% # daily
    summarise( sl_ = sum(sl_)/1000 ) %>% # sl = step lengths or distance traveled 
    ggplot(.) + theme_bw(base_size = 17) +
    geom_line( aes( y = sl_, x = jday), color = "dark green", lwd = 0.75) +
    ylim(NA,400) + 
    labs( title = paste0('individual =', trks$territory[i]),
          y = "Daily distance (km)", x = "Day of the year" )
  print(a)
}

# Focus on breeding season data:
# Estimate sampling rate for each individual by looping through data using 
# purrr function map( )

sumtrks <- trks %>%  summarize( 
  map( breeding, amt::summarize_sampling_rate ) )

# view
sumtrks
sumtrks[[1]] # sampling rate is seconds between fixes

trks.all <- trks %>% mutate(
  red = map( breeding, function(x) 
    x %>%  track_resample( rate = seconds(5), 
                           tolerance = seconds(3)) %>% 
      steps_by_burst() ) )

# steps = step length tibble calculated by bursts..............................?

# view
trks.all

# plot autocorrelation for step lengths for all individuals
# Don't need to view autocorrelation for SSF
# par( mfrow = c( 3,3 ) ) # what's this?
# 
# for( i in 1:dim(trks.all.med)[1] ){
#   # extract individual ids
#   idd <- trks.all.med$territory[i]
#   # extract step lengths for each individual
#   x <- pull( trks.all.med[["steps"]][[i]], sl_) # direction_p )
#   # remove missing data
#   x <- x[!is.na(x)]
#   # calculate autocorrelation function based on step length
#   acf( x, lag.max = 100,
#        main = paste0( "individual = ",idd ) )
#   # Note you can modify the lag.max according to your data # lag is in minutes?
# }

# What is ACF ?
# Autocorrelation Function
# What would be a reasonable rate to resample at?
# Answer:

# Choose 20 sec - For some individual 20 sec there's no autocorrelation,
# For others, at 20 sec declines for final time

# trks.all.med <- trks.all %>% 
#   mutate(red = map(breeding, function(x ) x %>%  
#                      track_resample( rate = seconds(5),
#                                      tolerance = seconds(3) ) ) )

# red= 5 second intervals data
# steps= 20 second intervals data

# view
# trks.all.med

# plot autocorrelation for turning angles? for all individuals
# 
# for( i in 1:dim(trks.all)[1] ){
#   # extract individual ids
#   idd <- trks.all$territory[i]
#   # extract step lengths for each individual
#   x <- pull( trks.all[["steps"]][[i]], direction_p )
#   # remove missing data
#   x <- x[!is.na(x)]
#   # calculate autocorrelation function based on step length
#   acf( x, lag.max = 200,
#        main = paste0( "individual = ",idd ) )
#   # Note you can modify the lag.max according to your data # lag is in minutes?
# }

# I keep the original sampling rate (all autocorrelation as is) and  
# also resample the 'breeding' tibbles to 30 min intervals

# trks.all <- trks.all %>% 
#   mutate(red = map(breeding, function( x ) x %>%  
#                      track_resample( rate = minutes(30),
#                                      tolerance = minutes(5) ) ),
#          red.steps = map( breeding, function(x) 
#            x %>%  track_resample( rate = seconds(30), 
#                                   tolerance = seconds(30)) %>% 
#              steps_by_burst() ) ) # plot autocorrelation for step lengths for all individuals

# 
# view
# 
# trks.all

# We can now unnest the dataframes of interest
# Starting with all breeding season data

trks.breed <- trks %>% dplyr::select( territory, breeding ) %>% 
  unnest( cols = breeding ) 

head( trks.breed )

trks.5sec <- trks.all %>% dplyr::select( territory, red ) %>% 
  unnest( cols = red ) 

head( trks.5sec )
length( trks.5sec$burst_ )
class( trks.5sec )

# filter_min_n_burst : Filter bursts by number of relocations
# Description-
# Only retain bursts with a minimum number (= min_n) of relocations.

# as_sf_points Coerces a track to points
# Description
# Coerces a track to points from the sf package
# Must be in class 'track_xyt'

trks.all_nb <- trks %>% mutate(
  red = map( breeding, function(x) 
    x %>%  track_resample( rate = seconds(5), 
                           tolerance = seconds(3)) ))

head(trks.all_nb)
class(trks.all_nb)
trks.5sec_nb <- trks.all_nb %>% dplyr::select( territory, red ) %>% 
  unnest( cols = red ) 
head(trks.5sec_nb)
class(trks.5sec_nb)
trks.5filter <- filter_min_n_burst(trks.5sec_nb, 50)
head(trks.5filter)
class(trks.5filter)

trks.5sf <- as_sf_points(trks.5sec_nb)


######################     CALCULATING STEP LENGTHS    #########################
#.....................         To Define Foraging      ........................#

# We can plot step lengths by:
# trks.steps30 %>% 
head(trks.5sec)

trks.5sec %>%   
  ggplot(.) +
  # geom_density( aes( x = sl_, fill = as.factor(burst_)), alpha = 0.4 ) +
  geom_histogram( aes( x = sl_ ) ) +
  xlab("Step length" ) + 
  # ylim( 0, 0.01 ) + xlim(0, 2000 ) +
  theme_bw( base_size = 19 )  +
  theme( legend.position = "none" ) +
  facet_wrap( ~territory, scales = 'free_y' )

# What is the unit on step length? meters?.....................................?

#######################     CALCULATING TURN ANGLES    #########################
#.....................         To Define Foraging      ........................#

trks.5sec %>% #filter( id == 1 ) %>% 
  ggplot( .) +
  geom_histogram( aes( x = ta_ ) ) +
  #geom_histogram( aes( x = direction_p ) ) +
  #coord_polar() +
  ylab("Count") + xlab("TA") + 
  theme_bw( base_size = 19 ) +
  facet_wrap( ~territory, scales = 'free_y' )

# Warning message:
#   Removed 1979 rows containing non-finite values (stat_bin). 
#   Missing TA values at start and end of burst maybe?

########################    SAVING OBJECTS AND DATA    #########################
#..............................................................................#

# # save breeding season data (not thinned)
# 
write_rds( trks.breed, "trks.breed")
# 
# #save breeding season data (turned into steps)
# 
write_rds( trks.5sec, "trks.5sec" )
# 
# #save breeding season data (thinned)
# 
# write_rds( trks.thin, "trks.thin" )
# 
# #save breeding season data (steps thinned)
# 
# write_rds( trks.steps30, "trks.steps30" )
# 
# #save migration data (unthinned)
# 
# write_rds( trks.mig, "trks.mig" )
# 
# #save workspace in case we need to make changes

save.image( "TracksWorkspace22.RData" )


########################         END OF SCRIPT       ###########################









