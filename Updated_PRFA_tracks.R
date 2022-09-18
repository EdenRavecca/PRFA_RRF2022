

##############################  Load PRFA Data  ################################

# Clean your workspace to reset your R environment

rm( list = ls() )

# Script order up to this point: 1) cleaning_PRFA 2) sf_nestbuffer 3) Updated_PRFA_tracks

# load packages relevant to this script:

library( tidyverse ) #easy data manipulation
library( amt )
library( sp )
library( lubridate ) #easy date manipulation
library( sf )
library( ggmap )

# Load or create data

# Set working directory. This is the path to your Rstudio folder for this 
# project. If you are in your correct Rstudio project then it should be:
# getwd()
# if so then:
# workdir <- getwd()

# load workspace 
load( "TracksWorkspace_updated.RData" )

# We first check sample size

table( datadf$territory )
table(datadf$yr)
head(All_pts_out)
table( All_pts_out$territory )
table(All_pts_out$yr)

# Remove non breeders from data; Data frame is All_pts_out = all points outside of 750 m nest buffers

data_breeders <- All_pts_out %>%
  group_by( territory ) %>%
  filter( !territory == "Owyhee" ) %>%
  ungroup()

unique(data_breeders$territory)

# We can also get an idea of the data collection for each individual
# by plotting histograms

# sampling dates

ggplot( data_breeders, aes( x = jday, color = as.factor(yr) ) ) +
  theme_classic( base_size = 15 ) +
  geom_histogram( ) +
  labs(title = "Individual", x = "Day of Year", y = "Count", col = "Year") +
  facet_wrap( ~ territory )

# speeds traveled (in knots)
ggplot( data_breeders, aes( x = speed, color = as.factor(yr) ) ) +
  theme_classic( base_size = 15 ) +
  geom_histogram( ) +
  labs(title = "Individual", x = "Speed (knots)", y = "Count", col = "Year") +
  facet_wrap( ~ territory )
# Distribution of speed remains fairly consistent across individuals, regardless of how much data
# was actually able to be collected

max(All_pts_out$speed)
mean(All_pts_out$speed)
range(All_pts_out$alt) # [1] -324 7668; -324 makes no sense??
dim( All_pts_out ) - dim( data_breeders )
# check
table( All_pts_out$territory ) # 9406 points removed were from Owyhee

#..............................................................................#
#......                                                                  ......#
#......   Creating tracks, calculating step lengths and turning angles   ......#
#......             for all individuals at the same time:                ......#
#......                                                                  ......#
#..............................................................................#

########################       CREATING TRACKS      ############################
#.......................    For HR Estimation AMT   ...........................#

# amt requires us to turn data into tracks for further analyses.
head(data_breeders)
colnames(data_breeders)
data_breeders <- data_breeders %>%
  dplyr::select(serial, ts, territory, id, Sex, speed, alt, date, mth, jday, yr, geometry)
data_breed <- data_breeders %>%
  mutate(., x = unlist(map(data_breeders$geometry,1)), y = unlist(map(data_breeders$geometry,2))) # separate geometry column
data_breed <- st_drop_geometry(data_breed) # remove geometry column

trks <- data_breed %>% 
  # make track. Note you can add additional columns to it.
  amt::make_track(.y = y, .x = x, .t = ts, 
                  # define columns that you want to keep, relabel if you need:
                  serial = serial, id = id, sex = Sex, speed = speed, 
                  alt = alt, territory = territory, jday = jday,
                  # assign correct crs
                  crs = crstracks )
head(trks)

# Turn into a grouped tibble list = nesting by individual territories:

trks <- trks %>%  amt::nest( data = -"territory" )

# view

trks

# We have multiple types of data collection rates:
# FM 2-sec fixes in flight 3 times a week, 20min fixes during the day, hourly fixes during migration
# Focusing on data during breeding season
# Remove migration locations.(already previously filtered by jday 60-182)

# Plot overall paths for each individual:

for( i in 1:dim(trks)[1]){
  a <- as_sf_points( trks$data[[i]] ) %>% 
    ggplot(.) + theme_bw(base_size = 17) +
    labs( title = paste0('individual =', trks$territory[i]) ) +
    geom_sf(data = NCA_Shape, inherit.aes = FALSE ) +
    geom_sf() 
  print(a)
}
range(data_breeders$jday) # currently data is subset from jday 61-181 (March 1 to June 30)
# Last year many birds left prior to Jun 30, 2021 and some may have returned after Mar 1, 2022
# That explains the points outside of NCA boundary

# Use the NCA boundary coordinates to remove points remaining migrating points

sf::st_bbox(NCA_Shape)

# Use the Eastern & Northern NCA coordinates to filter out data 

xmax <- as.numeric(st_bbox(NCA_Shape)$xmax) # 627081.5
ymax <- as.numeric(sf::st_bbox(NCA_Shape)$ymax) + 10000

# subset those tracks less than max and greater than min as breeding and all else as migrating:

trks <- trks %>% mutate(
  breeding = map( data, ~ filter(., x_ < xmax, y_ < ymax ) ),
  migrating = map( data, ~ filter(., x_ >= xmax, y_ >= ymax ) ) )

# view

trks
trks[[3]] # data

# Note we created two additional groups of tibbles: breeding season and migrating

sumtrks <- trks %>%  summarize( 
  map( breeding, amt::summarize_sampling_rate ) )

# view
sumtrks
sumtrks[[1]] # sampling rate is seconds between fixes

#####################     RESAMPLING DATA INTERVALS    #########################
#.........      Resample at 5s with & without steps_by_burst      .............#

# Keep columns of interest = territory & breeding, unnest to view
trks_breed <- trks %>% dplyr::select( territory, breeding ) %>% 
  unnest( cols = breeding ) 
head( trks_breed )

# nest prior to resampling, so bursts are calculated for each individual seperately
trks_breed <- trks_breed %>%  amt::nest( data = -"territory" )

# Resample breeding tracks at 2-8 secs
trks_breed_5s <- trks_breed %>% 
  dplyr::mutate( resample = map( data, function(x) 
    x %>%  track_resample( rate = seconds(5), tolerance = seconds(3) )))

trks_5s <- trks_breed_5s %>% dplyr::select( territory, resample ) %>% 
  unnest( cols = resample ) 
head( trks_5s )

max(trks_5s$burst_)
as.integer(names(which.max(table(trks_5s$burst_))))
length(which(trks_5s$burst_ == "2128"))

# Resample breeding tracks at 2-8 secs INCLUDE steps_by_burst
trks_breed_5s_sbb <- trks_breed %>% 
  mutate( sbb_5s = map( data, function(x) 
    x %>%  track_resample( rate = seconds(5), tolerance = seconds(3) ) %>% 
      steps_by_burst() ) )
# new column "sbb_5s" = resampled tracks at 2-8 seconds w/ steps by burst + amt SL & TA calculated
# including the steps_by_burst argument returns less tracks than without.......?

trks_5s_sbb <- trks_breed_5s_sbb %>% dplyr::select( territory, sbb_5s ) %>% 
  unnest( cols = sbb_5s ) 
head( trks_5s_sbb )

max(trks_5s_sbb$burst_)
as.integer(names(which.max(table(trks_5s_sbb$burst_))))
length(which(trks_5s_sbb$burst_ == "2128"))

max(trks_5s_sbb$sl_)
mean(trks_5s_sbb$sl_)
range(trks_5s_sbb$sl_)

######################      STEP LENGTHS HISTOGRAMS    #########################
#.....................    Compare Individual Foraging      ....................#

# Use resampled data that includes step_by_burst

# We can plot step lengths by:
head(trks_5s_sbb)

trks_5s_sbb %>%   
  ggplot(.) +
  # geom_density( aes( x = sl_, fill = as.factor(burst_)), alpha = 0.4 ) +
  geom_histogram( aes( x = sl_ ) ) +
  xlab("Step Length" ) + 
  theme_bw( base_size = 19 )  +
  theme( legend.position = "none" ) +
  facet_wrap( ~territory, scales = 'free_y' )
# Count (collection rate) isn't consistent across individuals, but the shape of distribution
# remains similar 

######################     TURNING ANGLE HISTOGRAMS    #########################
#.....................    Compare Individual Foraging      ....................#

# Use resampled data that includes step_by_burst

trks_5s_sbb %>% # filter( id == 1 ) %>% 
  ggplot( .) +
  geom_histogram( aes( x = ta_ ) ) +
  #geom_histogram( aes( x = direction_p ) ) +
  #coord_polar() +
  ylab("Count") + xlab("TA") + 
  theme_bw( base_size = 19 ) +
  facet_wrap( ~territory, scales = 'free_y' )
# turning angle distribution is less uniform across individuals

# Warning message:
#   Removed 'x number of' rows containing non-finite values (stat_bin). 
#   Missing TA values at start and end of burst maybe?

########################    SAVING OBJECTS AND DATA    #########################
#..............................................................................#

# save all breeding season data (not resampled)

write_rds( trks_breed, "trks_breed_updated")

# save 5s breeding season data (without step_by_bursts)

write_rds( trks_5s, "trks_5s_updated" )

# save 5s breeding season data INCLUDES step_by_bursts

write_rds( trks_5s_sbb, "trks_5s_sbb")

save.image( "TracksWorkspace_updated.RData" )


########################         END OF SCRIPT       ###########################

class(trks_5s)
class(trks_5s_sbb)
colnames(trks_5s)
colnames(trks_5s_sbb)

# sbb_5s is tracks resampled at 2-8 seconds using steps_by_burst

sbb_test1 <- trks_breed_5s_sbb %>% dplyr::select( territory, sbb_5s ) %>% 
  unnest( cols = sbb_5s ) 
head( sbb_test1 )

sbb_test2 <- trks_breed_5s_sbb %>% dplyr::select( territory, data ) %>% 
  unnest( cols = data ) 
head( sbb_test2 )

# dplyr::left_join(): includes all rows in x.
# left_join(x, y, by = what_to_join_by)
sbb_test3 <- dplyr::left_join(sbb_test1, sbb_test2)









