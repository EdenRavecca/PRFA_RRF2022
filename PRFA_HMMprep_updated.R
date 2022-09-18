

###################### PRFA HMM ANALYSIS PREP SCRIPT ###########################
#..............    Prep data for HMM with all individuals   ....................
#..............           Using momentuHMM package          ....................

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

# load( "TracksWorkspace22.RData" )
load( "PRFA_HMM_prep_updated.RData" )

# all breeding season data (not resampled)
trks_breed <- read_rds( "trks_breed_updated" )
# 5s breeding season data (without step_by_bursts)
trks_5s <- read_rds( "trks_5s_updated" )
# 5s breeding season data (with step_by_bursts)
trks_5s_sbb <- read_rds( "trks_5s_sbb")
# nest site locations
# nest_site <- read_rds( "nest_site" )
# # nest site locations sf object
# nest_sf <- read_rds( "nest_sf" )
# # nest site locations sf object- WGS 84 for accurate buffer measurement
# nest_sf_reproj <- read_rds( "nest_sf_reproj" )
# # 750m nest buffer
nest_buffer <- read_rds( "nest_buffer" )

NCA_Shape <- sf::st_read("Z:/Common/QCLData/Habitat/NCA/GIS_NCA_IDARNGpgsSampling/BOPNCA_Boundary.shp")

trks_5s_sf <- as_sf_points( trks_5s, coords = c("x_", "y_") )
head(trks_5s_sf) # 5 sec tracks in sf data frame # as_sf_points Coerces a track to points
# Description
# Coerces a track to points from the sf package
# Must be in class 'track_xyt'
class(trks_5s_sf)

as.integer(names(which.max(table(trks_5s_sf$burst_))))
length(which(trks_5s_sf$burst_ == "2128"))

unique(trks_5s_sf$territory)

# Why are objects different lengths............................................?

par(ask = FALSE) # turns of Hit <Return> to see next plot

head(trks_5s_sf)
head(nest_buffer)
st_crs(trks_5s_sf) == st_crs(nest_buffer)

ggplot() + 
  geom_sf(data = NCA_Shape, size = 1.5, color = "black", fill = NA) +
  stat_sf_coordinates(data = trks_5s_sf, size = .75, aes( color = as.factor(territory)) ) +
  geom_sf(data = nest_buffer, size = 1, color = "black", fill = NA) +
  coord_sf(xlim = c( 532740.2 , 627081.5 ), ylim = c( 4741845.2 , 4810408.4 ), expand = FALSE) +
  ggtitle("NCA Out points")

colnames(trks_5s_sf)
hist(trks_5s_sf$burst_, breaks= 100) # count on y, burst ID on x
as.integer(names(which.max(table(trks_5s_sf$burst_)))) # which burst has greatest number of rows
length(which(trks_5s_sf$burst_ == "2128")) # how many rows match max burst; 4840 relocations
range(trks_5s_sf$burst_)

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX#

trks_5s_nested <- trks_5s  %>% nest( data = -territory)
class(trks_5s)
trks_5s_nested %>%
  pull(data) %>% # view data column
  pluck(1) # view element 1 of data column

trks_5s_filter <- trks_5s_nested  %>% mutate( red = map( data, function(x) x  %>%
                                                     filter_min_n_burst(min_n = 100))) # keep bursts with >=100 relocations
trks_5s_filter %>%
  pull(red) 

data_5s <- trks_5s_filter %>% dplyr::select( territory, red ) %>% 
  unnest( red ) 
class(data_5s)

hist(data_5s$burst_, breaks= 100)
max(data_5s$burst_) # total number of bursts
max(trks_5s$burst_) - max(data_5s$burst_) # total number of bursts removed
as.integer(names(which.min(table(data_5s$burst_)))) # which burst has least number of rows
length(which(data_5s$burst_ == "278")) # how many rows match min burst;  100 relocations
head(data_5s)

data_5s_BID <- data_5s %>%
  group_by(territory) %>%
  dplyr::mutate(., BurstID = paste(territory, burst_, sep = "_")) %>% # create new column with territory & burst ID
  ungroup()
head(data_5s_BID)

colnames(data_5s_BID)
AllPRFA_alt <- data_5s_BID %>%
  dplyr::select(ID = "BurstID", y_, x_, t_, sex, alt) 

head(AllPRFA_alt)

PRFA_alt_df <- data.frame(AllPRFA_alt) %>% 
  select( ID, t_, easting = "x_", northing = "y_", alt ) %>% 
  mutate( scaled_alt = scale( alt ) )
class(PRFA_alt_df)
head(PRFA_alt_df)

# check for missing values in scaled predictor
sum( is.na(PRFA_alt_df$scaled_alt))
?prepData

PRFA_alt_prep <- prepData(PRFA_alt_df, type = "UTM", coordNames = c("easting","northing"), 
                          covNames = c("alt", "scaled_alt" ) )

head(PRFA_alt_prep)
write_rds(PRFA_alt_prep, "PRFA_alt_prep")

save.image( "PRFA_HMM_prep_updated.RData" )

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX#

# Can't compare SL & TA of amt vs momentuHMM
# filter_min_burst requires class track_xyt
# resampling 5s data in amt using steps_by_burst returns object of class steps_xyt
# prepData in momentuHMM gives point SL & TA
# resample + steps_by_burst in amt gives SL & TA
# filter_min_burst is applied to 5 sec data prior to using prepData
# SEE end of Updated_PRFA_tracks.R to refresh thought process
