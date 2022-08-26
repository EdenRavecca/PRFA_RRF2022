

######################### PRFA HMM ANALYSIS SCRIPT #############################
#...................    Testing HMM with 1 individual  .........................

# load packages relevant to this script:

library( momentuHMM )
library( dplyr )
library( sf )
library ( amt )

# Load or create data
# Clean your workspace to reset your R environment. 
# rm( list = ls() )

# Set working directory. This is the path to your Rstudio folder for this 
# project. If you are in your correct Rstudio project then it should be:
# getwd()
# if so then:
# workdir <- getwd()

# load workspace 
# load( "cleaningPRFA22.RData" )
# load( "nest_site_bufferpts.RData" )
# load( "TracksWorkspace22.RData" )
load( "HMM_SG_Workspace.RData" )

head(trks_5s_SG)
# SG_5s_wgs84 <- st_transform(trks_5s_SG, crsdata) # reproject SG 5s data to WGS 84
#  head(SG_5s_wgs84)

colnames(trks_5s_SG)
hist(trks_5s_SG$burst_, breaks= 1000, xlim = c(0, 5100), ylim = c(0, 5000)) # count on y, burst ID on x$burst_, breaks= 150, xlim = c(1000, 4800), ylim = c(0, 400)) # count on y, burst ID on x
max(trks_5s_SG$burst_) # total number of bursts
as.integer(names(which.max(table(trks_5s_SG$burst_)))) # which burst has greatest number of rows
length(which(trks_5s_SG$burst_ == "3367")) # how many rows match max burst; 4840 relocations
range(trks_5s_SG$burst_)

SG_FM_utm <- filter_min_n_burst(trks_5s_SG, min_n = 100) # keep bursts with >=100 relocations
hist(SG_FM_utm$burst_, breaks= 1000, xlim = c(0, 5100), ylim = c(0, 5000)) # count on y, burst ID on x$burst_, breaks= 150, xlim = c(1000, 4800), ylim = c(0, 400)) # count on y, burst ID on x
max(SG_FM_utm$burst_) # total number of bursts
max(trks_5s_SG$burst_) - max(SG_FM_utm$burst_) # total number of bursts removed
as.integer(names(which.max(table(SG_FM_utm$burst_)))) # which burst has greatest number of rows
head(SG_FM_utm)

SG_FM_utm <- SG_FM_utm %>%
  group_by(burst_) %>% # need to ALSO group by territory when using data including all individuals
  dplyr::mutate(BID = cur_group_id()) %>% # creates ID beginning at 1:nrow
  dplyr::mutate(., BurstID = paste(territory, BID, sep = "_")) %>% # create new column with territory & burst ID
  ungroup()

SG_FM_utm <- SG_FM_utm %>%
  mutate(., y = unlist(map(SG_FM_utm$geometry,1)), x = unlist(map(SG_FM_utm$geometry,2))) # separate geometry column

colnames(SG_FM_utm)
SG_FM_utm <- SG_FM_utm %>%
  dplyr::select(ID = "BurstID", date, t_, sex, alt, y, x)

SG_FM_utm <- st_drop_geometry(SG_FM_utm) # remove geometry column
class(SG_FM_utm)
SG_df <- data.frame(SG_FM_utm)
class(SG_df)

?prepData

SG_prep <- prepData(SG_df, type = "UTM", coordNames = c("y","x"), covNames = "alt")
# some SL are 0 resulting in NA turning angle

sum(is.na(SG_prep$step)) # 164 NA step lengths
sum(is.na(SG_prep$angle)) # 1283 NA turning angles
length(which(SG_prep$step == "0")) # 500 step lengths = 0
as.integer(which(SG_prep$step == "0")) # which rows have step length 0
class(SG_prep) # "momentuHMMData" "data.frame"  

mean(SG_prep$alt)
max(SG_prep$alt)
min(SG_prep$alt)

SG_prep$scaled_alt <- scale(SG_prep$alt)
range(SG_prep$scaled_alt)
plot(density(SG_prep$alt))
hist(SG_prep$alt)

### Fit HMM
# initial step distribution natural scale parameters
# stepPar0 <- c(1,5,0.5,3) # (mu_1,mu_2,sd_1,sd_2)
# initial angle distribution natural scale parameters
# anglePar0 <- c(0,0,1,8) # (mean_1,mean_2,concentration_1,concentration_2)

altPar0 <- c(0, 0.5, 0, 0.5) # mu_1, mu_2, sd_1, sd_2

fitAlt1PRFA <- fitHMM(data = SG_prep, nbStates = 2,
                    dist = list(scaled_alt = "gamma"),
                    Par0 = list(scaled_alt = altPar0))






mean(data_breeders$alt)
min(data_breeders$alt)
range(data_breeders$alt)







