
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
library( ggmap )

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
load( "nest_site_bufferpts.RData" )
load( "TracksWorkspace22.RData" )

# We first check sample size

table( datadf$territory )
table(datadf$yr)

# Remove non breeders from data

data_breeders <- datadf %>%
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

trks <- data_breeders %>% 
  # make track. Note you can add additional columns to it.
  amt::make_track(.y = lat, .x = lon, .t = ts, 
                  # define columns that you want to keep, relabel if you need:
                  serial = serial, id = id, sex = Sex, mth = mth, jday = jday, speed = speed, 
                  alt = alt, territory = territory, date = date, yr = yr,
                  # assign correct crs
                  crs = crsdata )

# Reproject to UTM to convert lat lon to easting northing:
# This step is only necessary for calculating SL to be in meters

st_crs(crstracks) # UTM
trks <- amt::transform_coords( trks, crs_to = crstracks )

# Turn into a tibble list by grouping and nesting by individual IDs:

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
trks[[2]] # data

# Note we created two other groups of tibbles for the breeding season and migrating

# Plot sum of step lengths by jday

for( i in 1:dim(trks)[1]){
  a <-  steps( trks$breeding[[i]] ) %>% 
    # a <-  steps( trks$migrating[[i]] ) %>% 
    mutate( jday = lubridate::yday( t1_ ) ) %>% #what is t1_?
    group_by( jday ) %>% # daily
    mutate( summarise( sl_ = sum(sl_)/1000 )) %>% # sl = step lengths or distance traveled 
    ggplot(.) + theme_bw(base_size = 17) +
    geom_line( aes( y = sl_, x = jday, color = as.factor(yr)), lwd = 0.75) +
    ylim( NA,400 ) + 
    labs( title = paste0('individual = ', trks$territory[i]),
          y = "Daily distance (km)", x = "Day of the year", col = "Year" )
  print(a)
}

# Estimate sampling rate for each individual by looping through data using 
# purrr function map( )

sumtrks <- trks %>%  summarize( 
  map( breeding, amt::summarize_sampling_rate ) )

# view
sumtrks
sumtrks[[1]] # sampling rate is seconds between fixes

#####################     RESAMPLING DATA INTERVALS    #########################
#.................      Resample at 5s, 30s, 2m, 5 m      .....................#

# Later remove steps_by_burst argument, since amt's TA and SL not needed here
trks_all_5s <- trks %>% 
  mutate( red5s = map( breeding, function(x) 
    x %>%  track_resample( rate = seconds(5), tolerance = seconds(3) ) %>% 
      steps_by_burst() ) )
# new column "red5s" = resampled tracks at 2-8 seconds

# view

trks_all_5s

# Unnest the dataframes of interest

trks_breed <- trks %>% dplyr::select( territory, breeding ) %>% 
  unnest( cols = breeding ) 
head( trks_breed )

trks_5s <- trks_all_5s %>% dplyr::select( territory, red5s ) %>% 
  unnest( cols = red5s ) 
head( trks_5s )

range( trks_5s$burst_ )
class( trks_5s )

# filter_min_n_burst : Filter bursts by number of relocations
# Description-
# Only retain bursts with a minimum number (= min_n) of relocations.

# This not working for some reason..............................................?
# trks_5s_filter <- filter_min_n_burst(trks_5s_nb, min_n = 50)
# head(trks_5s_filter)
# class(trks_5s_filter)

# 5s, 30s, 2m, 5m

# resample breeding tracks at 2-8 seconds but remove steps_by_burst argument
trks_all_5s_nb <- trks_all_5s %>% mutate( red5s_nb = map( breeding, function(x) 
  x %>%  track_resample( rate = seconds(5), 
                         tolerance = seconds(3)) ))
head(trks_all_5s_nb)
class(trks_all_5s_nb)

# Unnest columns of interest

trks_5s_nb <- trks_all_5s_nb %>% dplyr::select( territory, red5s_nb ) %>% 
  unnest( cols = red5s_nb ) 
head(trks_5s_nb)
class(trks_5s_nb)
range(trks_5s_nb$burst_)

# resample at 30s with 5s tolerance (25-35s)

trks_all_30s <- trks %>% 
  mutate( red30s = map( breeding, function(x) 
    x %>%  track_resample( rate = seconds(30), tolerance = seconds(5) ) %>% 
      steps_by_burst() ) )
trks_all_30s

trks_30s <- trks_all_30s %>% dplyr::select( territory, red30s ) %>% 
  unnest( cols = red30s ) 
head(trks_30s)
range( trks_30s$burst_ )

# resample breeding tracks at 25-35 seconds but remove steps_by_burst argument

trks_all_30s_nb <- trks_all_30s %>% mutate( red30s_nb = map( breeding, function(x) 
  x %>%  track_resample( rate = seconds(30), 
                         tolerance = seconds(5)) )) 
    
head(trks_all_30s_nb)

trks_30s_nb <- trks_all_30s_nb %>% dplyr::select( territory, red30s_nb ) %>% 
  unnest( cols = red30s_nb ) 
head(trks_30s_nb)
class(trks_30s_nb)
range(trks_30s_nb$burst_)

# resample at 2 min with 1 min tolerance

trks_all_2min <- trks %>% 
  mutate( red2min = map( breeding, function(x) 
    x %>%  track_resample( rate = minutes(2), tolerance = minutes(1) ) %>% 
      steps_by_burst() ) )
trks_all_2min

trks_2min <- trks_all_2min %>% dplyr::select( territory, red2min ) %>% 
  unnest( cols = red2min ) 
head(trks_2min)
range( trks_2min$burst_ )

# resample breeding tracks at 1-3 min but remove steps_by_burst argument

trks_all_2min_nb <- trks_all_2min %>% mutate( red2min_nb = map( breeding, function(x) 
  x %>%  track_resample( rate = minutes(2), 
                         tolerance = minutes(1)) )) 

head(trks_all_2min_nb)
class(trks_all_2min_nb)

trks_2min_nb <- trks_all_2min_nb %>% dplyr::select( territory, red2min_nb ) %>% 
  unnest( cols = red2min_nb ) 
head(trks_2min_nb)
class(trks_2min_nb)
range(trks_2min_nb$burst_)

# resample at 5 min with 2 min tolerance

trks_all_5min <- trks %>% 
  mutate( red5min = map( breeding, function(x) 
    x %>%  track_resample( rate = minutes(5), tolerance = minutes(2) ) %>% 
      steps_by_burst() ) )
trks_all_5min

trks_5min <- trks_all_5min %>% dplyr::select( territory, red5min ) %>% 
  unnest( cols = red5min ) 
head(trks_5min)
range( trks_5min$burst_ )

# resample breeding tracks at 3-7 min but remove steps_by_burst argument

trks_all_5min_nb <- trks_all_5min %>% mutate( red5min_nb = map( breeding, function(x) 
  x %>%  track_resample( rate = minutes(5), 
                         tolerance = minutes(2)) )) 

head(trks_all_5min_nb)
class(trks_all_5min_nb)

trks_5min_nb <- trks_all_5min_nb %>% dplyr::select( territory, red5min_nb ) %>% 
  unnest( cols = red5min_nb ) 
head(trks_5min_nb)
class(trks_5min_nb)
range(trks_5min_nb$burst_)

##################     VIZUALIZING & EXPLORING TRACKS    #######################
#..............       All individuals & collection rates      .................#

# as_sf_points Coerces a track to points
# Description
# Coerces a track to points from the sf package
# Must be in class 'track_xyt'

# create SF data frame for plotting

trks_5s_sf <- as_sf_points( trks_5s_nb, coords = c("x_", "y_") )
head(trks_5s_sf)

trks_30s_sf <- as_sf_points( trks_30s_nb, coords = c("x_", "y_") )
head(trks_5s_sf)

trks_2min_sf <- as_sf_points( trks_2min_nb, coords = c("x_", "y_") )
head(trks_5s_sf)

trks_5min_sf <- as_sf_points( trks_5min_nb, coords = c("x_", "y_") )
head(trks_5s_sf)

ggplot() +
  geom_sf(data = NCA_Shape, inherit.aes = FALSE ) +
  geom_sf(trks_5s_sf, mapping = aes(color = as.factor(yr)), inherit.aes = FALSE ) + 
  coord_sf()+
  labs(title = "Movement by Year", x = "Lon", y = "Lat", col = "Year")
table(trks_5s_sf$territory)

ggplot() +
  geom_sf(data = NCA_Shape, inherit.aes = FALSE ) +
  geom_sf(trks_30s_sf, mapping = aes(color = as.factor(yr)), inherit.aes = FALSE ) + 
  coord_sf()+
  labs(title = "Movement by Year", x = "Lon", y = "Lat", col = "Year")

ggplot() +
  geom_sf(data = NCA_Shape, inherit.aes = FALSE ) +
  geom_sf(trks_2min_sf, mapping = aes(color = as.factor(yr)), inherit.aes = FALSE ) + 
  coord_sf()+
  labs(title = "Movement by Year", x = "Lon", y = "Lat", col = "Year")

ggplot() +
  geom_sf(data = NCA_Shape, inherit.aes = FALSE ) +
  geom_sf(trks_5min_sf, mapping = aes(color = as.factor(yr)), inherit.aes = FALSE ) + 
  coord_sf()+
  labs(title = "Movement by Year", x = "Lon", y = "Lat", col = "Year")
table(trks_5min_sf$territory)

head(trks_5s_sf)
table(trks_5s_sf$burst_)

max(trks_5s_sf$burst_)
max(trks_30s_sf$burst_)
max(trks_2min_sf$burst_)
max(trks_5min_sf$burst_)

#.........   1 Individual, view a few bursts, all collection rates  ...........#

trks_5s_SG <- trks_5s_sf %>%
  filter(., territory == "SG")
as.integer(names(which.max(table(trks_5s_SG$burst_))))
length(which(trks_5s_SG$burst_ == "3367"))

trks_5s_SG3367 <- trks_5s_SG %>%
  filter(., burst_ == "3367")
trks_5s_SG3366 <- trks_5s_SG %>%
  filter(., burst_ == "3366")
trks_5s_SG3368 <- trks_5s_SG %>%
  filter(., burst_ == "3368")
trks_5s_SG3364 <- trks_5s_SG %>%
  filter(., burst_ == "3364")
trks_5s_SG3365 <- trks_5s_SG %>%
  filter(., burst_ == "3365")
trks_5s_SG3415 <- trks_5s_SG %>%
  filter(., burst_ == "3415")
trks_5s_SG_btw <- trks_5s_SG %>%
  filter(., between(trks_5s_SG$burst_, "3368", "3414"))

SG3367 <- st_transform(trks_5s_SG3367, crsdata)
SG3367 <- SG3367 %>%
  mutate(., lon = unlist(map(SG3367$geometry,1)), lat = unlist(map(SG3367$geometry,2)))
head(SG3367)
class(SG3367)

SG3364 <- st_transform(trks_5s_SG3364, crsdata)
SG3364 <- SG3364 %>%
  mutate(., lon = unlist(map(SG3364$geometry,1)), lat = unlist(map(SG3364$geometry,2)))
SG3365 <- st_transform(trks_5s_SG3365, crsdata)
SG3365 <- SG3365 %>%
  mutate(., lon = unlist(map(SG3365$geometry,1)), lat = unlist(map(SG3365$geometry,2)))
SG3366 <- st_transform(trks_5s_SG3366, crsdata)
SG3366 <- SG3366 %>%
  mutate(., lon = unlist(map(SG3366$geometry,1)), lat = unlist(map(SG3366$geometry,2)))

SG3368 <- st_transform(trks_5s_SG3368, crsdata)
SG3368 <- SG3368 %>%
  mutate(., lon = unlist(map(SG3368$geometry,1)), lat = unlist(map(SG3368$geometry,2)))

SG3415 <- st_transform(trks_5s_SG3415, crsdata)
SG3415 <- SG3415 %>%
  mutate(., lon = unlist(map(SG3415$geometry,1)), lat = unlist(map(SG3415$geometry,2)))

SGbtw <- st_transform(trks_5s_SG_btw, crsdata)
SGbtw <- SGbtw %>%
  mutate(., lon = unlist(map(SGbtw$geometry,1)), lat = unlist(map(SGbtw$geometry,2)))


unique(trks_5s_SG$id)
head(trks_5s_SG3367)
class(trks_5s_SG3367)

ggplot() +
  geom_sf(trks_5s_SG3367, mapping = aes(), inherit.aes = FALSE ) + 
  coord_sf()+
  labs(title = "SG Single Burst (4840 GPS points)", x = "Lon", y = "Lat")

trks_30s_sf <- as_sf_points( trks_30s_nb, coords = c("x_", "y_") )
trks_30s_SG <- trks_30s_sf %>%
  filter(., territory == "SG")
range(trks_30s_SG$burst_)
table(trks_30s_SG$burst_)
as.integer(names(which.max(table(trks_30s_SG$burst_))))
length(which(trks_30s_SG$burst_ == "2177"))
trks_30s_SG2177 <- trks_30s_SG %>%
  filter(., burst_ == "2177")
unique(trks_30s_SG$id)

SG2177 <- st_transform(trks_30s_SG2177, crsdata)
SG2177 <- SG2177 %>%
  mutate(., lon = unlist(map(SG2177$geometry,1)), lat = unlist(map(SG2177$geometry,2)))
head(SG2177)
class(SG2177)

ggplot() +
  geom_sf(trks_30s_SG2177, mapping = aes(), inherit.aes = FALSE ) + 
  coord_sf()+
  labs(title = "SG Single Burst (808 GPS points)", x = "Lon", y = "Lat")

trks_2min_sf <- as_sf_points( trks_2min_nb, coords = c("x_", "y_") )
trks_2min_SG <- trks_2min_sf %>%
  filter(., territory == "SG")
range(trks_2min_SG$burst_)
table(trks_2min_SG$burst_)
as.integer(names(which.max(table(trks_2min_SG$burst_))))
length(which(trks_2min_SG$burst_ == "1293"))
trks_2min_SG1293 <- trks_2min_SG %>%
  filter(., burst_ == "1293")
unique(trks_2min_SG$id)

SG1293 <- st_transform(trks_2min_SG1293, crsdata)
SG1293 <- SG1293 %>%
  mutate(., lon = unlist(map(SG1293$geometry,1)), lat = unlist(map(SG1293$geometry,2)))

head(SG1293)
class(SG1293)

ggplot() +
  geom_sf(trks_2min_SG1293, mapping = aes(), inherit.aes = FALSE ) + 
  coord_sf()+
  labs(title = "SG Single Burst (481 GPS points)", x = "Lon", y = "Lat")

trks_5min_sf <- as_sf_points( trks_5min_nb, coords = c("x_", "y_") )
trks_5min_SG <- trks_5min_sf %>%
  filter(., territory == "SG")
range(trks_5min_SG$burst_)
table(trks_5min_SG$burst_)
as.integer(names(which.max(table(trks_5min_SG$burst_))))
length(which(trks_5min_SG$burst_ == "1161"))
trks_5min_SG1161 <- trks_5min_SG %>%
  filter(., burst_ == "1161")
unique(trks_5min_SG$id)

SG1161 <- st_transform(trks_5min_SG1161, crsdata)
SG1161 <- SG1161 %>%
  mutate(., lon = unlist(map(SG1161$geometry,1)), lat = unlist(map(SG1161$geometry,2)))

head(SG1161)
class(SG1161)

ggplot() +
  geom_sf(trks_5min_SG1161, mapping = aes(), inherit.aes = FALSE ) + 
  coord_sf()+
  labs(title = "SG Single Burst (178 GPS points)", x = "Lon", y = "Lat")

#.................        Compare burst resolutions       .....................#

ggplot() +
  geom_sf(trks_5s_SG3367, mapping = aes(color = "5 seconds"), inherit.aes = FALSE ) + 
  geom_sf(trks_30s_SG2177, mapping = aes(color = "30 seconds"), inherit.aes = FALSE ) + 
  geom_sf(trks_2min_SG1293, mapping = aes(color = "2 minutes"), inherit.aes = FALSE ) +
  geom_sf(trks_5min_SG1161, mapping = aes(color = "5 minutes"), inherit.aes = FALSE ) +
  coord_sf()+
  scale_color_manual(name = "Collection Rate",
                     values = c( "5 seconds" = "blue", "30 seconds" = "red", "2 minutes" = "darkorange", "5 minutes" = "darkviolet"),
                     labels = c("5 seconds", "30 seconds", "2 minutes", "5 minutes")) +
  labs(title = "SG Single Burst", x = "Lon", y = "Lat")

ggplot() +
  geom_path(SG3367, mapping = aes(x= lat, y= lon, color = "5 seconds"), lwd = 2.5, inherit.aes = FALSE ) +
  geom_path(SG2177, mapping = aes(x= lat, y= lon, color = "30 seconds"), lwd = 2, inherit.aes = FALSE ) +
  geom_path(SG1293, mapping = aes(x= lat, y= lon, color = "2 minutes"), lwd = 1.5, inherit.aes = FALSE ) +
  geom_path(SG1161, mapping = aes(x= lat, y= lon, color = "5 minutes"), lwd = 1, inherit.aes = FALSE ) +
  scale_color_manual(name = "Collection Rate",
                     values = c( "5 seconds" = "blue", "30 seconds" = "red", "2 minutes" = "darkorange", "5 minutes" = "darkviolet"),
                     labels = c("5 seconds", "30 seconds", "2 minutes", "5 minutes")) +
  labs(title = "SG Single Burst", x = "Lon", y = "Lat")

ggplot() +
  geom_path(SG3364, mapping = aes(x= lat, y= lon, color = "Prior Burst"), lwd = 1.5, inherit.aes = FALSE ) +
  geom_path(SG3367, mapping = aes(x= lat, y= lon, color = "Longest Burst"), lwd = 1.5, inherit.aes = FALSE ) +
  geom_path(SG3415, mapping = aes(x= lat, y= lon, color = "After Burst"), lwd = 1.5, inherit.aes = FALSE ) +
  geom_path(SGbtw, mapping = aes(x= lat, y= lon, color = "Points Between"), lwd = 1.5, inherit.aes = FALSE ) +
  scale_color_manual(name = "Bursts",
                     values = c(  "Prior Burst" = "blue", "Longest Burst" = "red", "After Burst" = "darkviolet", "Points Between" = "darkorange"),
                     labels = c("Prior Burst", "Longest Burst", "After Burst", "Points Between")) +
  labs(title = "SG 3 Consecutive Bursts", x = "Lon", y = "Lat") +
  geom_point(SG3365, mapping = aes(x= lat, y= lon), cex = 2, inherit.aes = FALSE ) + 
  geom_point(SG3366, mapping = aes(x= lat, y= lon), cex = 2, inherit.aes = FALSE ) + 
  geom_point(SGbtw, mapping = aes(x= lat, y= lon), cex = 2, inherit.aes = FALSE )


# shape: numeric values as pch for setting plotting points shapes.
# size: numeric values cex for changing points size

#.........       View bursts on Google maps for spatial context      ..........#

SG3364_pm <- st_transform(SG3364, crs = crsgoogle) # Transform to EPSG 3857 (Pseudo-Mercator, what Google uses)
SG3367_pm <- st_transform(SG3367, crs = crsgoogle)
SG3415_pm <- st_transform(SG3415, crs = crsgoogle)
SGbtw_pm <- st_transform(SGbtw, crs = crsgoogle)
SG3365_pm <- st_transform(SG3365, crs = crsgoogle)
SG3366_pm <- st_transform(SG3366, crs = crsgoogle)

NCA_google <- get_map( location = c(lon = -116.176396, lat = 43.278176), 
                         maptype = "terrain", source = "google", zoom = 11 )
# plot map
ggmap(NCA_google)

# Use function to assign correct google map CRS for plotting with geom_sf( )
NCA_google_x <- ggmap_bbox(NCA_google)

st_crs(NCA_google_x)

# Plot nest buffer over google map
ggmap(NCA_google_x) + 
  coord_sf(crs = crsgoogle) + # force the ggplot2 map to be in 3857
  geom_sf(SG3364_pm, mapping = aes( color = "Burst 1"), lwd = 1.5, inherit.aes = FALSE ) +
  geom_sf(SG3367_pm, mapping = aes( color = "Burst 2"), lwd = 1.5, inherit.aes = FALSE ) +
  geom_sf(SG3415_pm, mapping = aes( color = "Burst 3"), lwd = 1.5, inherit.aes = FALSE ) +
  geom_sf(SGbtw_pm, mapping = aes( color = "Points Between 2&3"), lwd = 1.5, inherit.aes = FALSE ) +
  scale_color_manual(name = "Bursts",
                     values = c(  "Burst 1" = "blue", "Burst 2" = "red", "Burst 3" = "darkviolet", "Points Between 2&3" = "darkorange"),
                     labels = c("Burst 1", "Burst 2", "Burst 3", "Points Between 2&3")) +
  labs(title = "SG 3 Consecutive Bursts", x = "Lon", y = "Lat")
# This map looks weird because Burst 1 and 2 are on Jun 2nd, 2021 (Wed) and Burst 3 are on Jun 4 (Fri). All
# "between" points are from Jun 3 (Thurs) which is a "slow" day, no FM, and W/F are "fast" days.

head(trks_5s_SG)
range(trks_5s_SG$burst_)

trks_5s_SG <- trks_5s_SG %>%
  mutate(., BID = paste(territory, burst_, sep = "_"))

trks_SG_FM <- filter_min_n_burst(trks_5s_SG, min_n = 10)
head(trks_SG_FM)
range(trks_SG_FM$BID)
table(trks_SG_FM$BID)

trks_SG_FM <- trks_SG_FM %>%
  group_by(burst_) %>%
  dplyr::mutate(BID = cur_group_id()) %>%
  dplyr::mutate(., BurstID = paste(territory, BID, sep = "_"))

######################      STEP LENGTHS HISTOGRAMS    #########################
#.....................    Compare Individual Foraging      ....................#

# Use resampled data that includes step_by_burst

# We can plot step lengths by:
head(trks_5s)

trks_5s %>%   
  ggplot(.) +
  # geom_density( aes( x = sl_, fill = as.factor(burst_)), alpha = 0.4 ) +
  geom_histogram( aes( x = sl_ ) ) +
  xlab("Step Length" ) + 
  theme_bw( base_size = 19 )  +
  theme( legend.position = "none" ) +
  facet_wrap( ~territory, scales = 'free_y' )
# Again, count (collection rate) isn't consistent across individuals, but the shape of distribution
# remains similar 

######################     TURNING ANGLE HISTOGRAMS    #########################
#.....................    Compare Individual Foraging      ....................#

# Use resampled data that includes step_by_burst

trks_5s %>% # filter( id == 1 ) %>% 
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

write_rds( trks_breed, "trks_breed")

# save 5s breeding season data (without step_by_bursts)

write_rds( trks_5s_nb, "trks_5s_nb" )

# save 30s breeding season data (without step_by_bursts)

write_rds( trks_30s_nb, "trks_30s_nb" )

# save 2min breeding season data (without step_by_bursts)

write_rds( trks_2min_nb, "trks_2min_nb" )

# save 5min breeding season data (without step_by_bursts)

write_rds( trks_5min_nb, "trks_5min_nb" )

# save workspace in case we need to make changes

save.image( "TracksWorkspace22.RData" )


########################         END OF SCRIPT       ###########################










