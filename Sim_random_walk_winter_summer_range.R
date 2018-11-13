# what causes migration -> cost landscape -> circuitscape -> SDM 0-1
# cost layer and benefit being food and cost being the habitat matrix
# cost: wetland species moving through dry area -> Landcover -> 
# https://cran.r-project.org/web/packages/SiMRiv/SiMRiv.pdf FOR SDMs
# git add *
  # git commit -i "Stuff"
# Shift q :wq
# git push origin master

# --- --- --- --- --- --- --- ---
# We have 3 distributions of 
# --- --- --- --- --- --- --- ---
# I could draw number of seeds per dung pile of guava from a normal distribution based on tortoise seed numbers. There are 3 distributions, 1 for highland, 2 for transition zone and 3 for lowlands. 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Psidium guajava: Mean fruit weight: 59.8g and mean seeds per fruit 226. 
# 10 seeds planted per treatment -> 80 % germinate -> 1 out of 1 makes it to shrub.
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Next steps: 40462_2017_104_MOESM2_ESM -> Include briana abrahams code -> So that I can make walks for different type of movement (such as a trait depending whether your nomadic, etc)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# [1] Load packages, data and scripts ####
setwd('/Users/diegoellis/projects/Yale_Classes/Quant_Methods_EEB/Project/Animations/')
source('/Users/diegoellis/projects/development/Simulation_the_spread_of_poop/Animate_sim.R') # 74935.32
source('/Users/diegoellis/projects/development/Simulation_the_spread_of_poop/similate_RW_no_migr_range.R')
source('/Users/diegoellis/projects/development/Simulation_the_spread_of_poop/simulate_migration.R')
source('/Users/diegoellis/projects/development/Simulation_the_spread_of_poop/dung_along_vegzones.R') # returns highland, low and transition distributions. # Also returns gut retention time distribution
# source('/Users/diegoellis/projects/development/Simulation_the_spread_of_poop/expand_CRW_BB_LEVY.R')
require(tidyverse)
require(MASS)
require(rgdal)
require(marcher)
require(smoove)
require(geosphere)
require(move)
require(lubridate)
require(raster)
require(reshape)
# https://cran.r-project.org/web/packages/emojifont/vignettes/emojifont.html
lowland_UD_shp <- readOGR('/Users/diegoellis/Dropbox/Marius_Galapagos/Inputs/Seasonal_UD/LR_Lowland.shp')
highland_UD_shp <- readOGR('/Users/diegoellis/Dropbox/Marius_Galapagos/Inputs/Seasonal_UD/Highland_polygon_final_basedon90UD.shp')
SRTM <- raster('/Users/diegoellis/Dropbox/Marius_Galapagos/Inputs/RS_Data_Santa_Cruz/SRTM/SRTM_Santa_Cruz.tif')

load('/Users/diegoellis/projects/Yale_Classes/Quant_Methods_EEB/Project/Data/1km_hexagon.rdata')
lowland_UD_shp= spTransform(lowland_UD_shp,   "+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs" )
highland_UD_shp = spTransform(highland_UD_shp, proj4string(lowland_UD_shp))
SRTM = projectRaster(SRTM, crs = proj4string(highland_UD_shp))
range_shp = highland_UD_shp
ndays = 150; daily_distance_moved = 200; avg_gut_retention_time = 8; guava_seed_per_dung_pile = 624; germinatioN_success = 0.8;seedling_to_shrub = 1/1000 # one out of every 1000 seedlings becomes a shrub  
# We could also use the entire seed dataset take the mean and the standard deviation.
# y <- dnorm(x, mean=10, sd=3)

  # [2] Simulate highland movement ####
# Parameters are 150 days we simulate, move 200 meters per day, simulate the highland range, average gut retention time is 8 days
steps.df = rw_within_homerange(150, 200, highland_UD_shp,  8, 'random_walk')
steps.df = rw_within_homerange(150, 200, highland_UD_shp, 8 , 'levy_walk')
# [3] Simulate migration ####
# SOMETHING IS WRONG WITH THE WAY I GET THE POINTS! #####
migration = simulate_migration(steps.df, highland_UD_shp, lowland_UD_shp, 8, 21, 'marcher') # Add colour first and last point!
migration = simulate_migration(steps.df, highland_UD_shp, lowland_UD_shp, 8, 21, 'brownian_bridge')
defined_start_point = tail(migration,1)
proj =   "+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
defined_start_point = SpatialPoints(defined_start_point[,c('Longitude', 'Latitude')])
proj4string(defined_start_point) = proj
plot(lowland_UD_shp)
points(defined_start_point, col = 'blue', pch = 16)

if(!gIntersects(defined_start_point, lowland_UD_shp)){
  message('Does not intersect')
  lowland_UD_shp_df = fortify(lowland_UD_shp)
  
  lowland_UD_shp_df <- SpatialPointsDataFrame(coords= lowland_UD_shp_df[,c('long', 'lat')], data = lowland_UD_shp_df,proj4string = CRS(proj))

  df_tmp = data.frame(distance = 0,
             Longitude = lowland_UD_shp_df$long,
             Latitude = lowland_UD_shp_df$lat)
  
  for(i in 1:nrow(lowland_UD_shp_df)){
    df_tmp[i,]$distance = gDistance(defined_start_point  ,lowland_UD_shp_df[i,])
    df_tmp[i,]$Longitude = lowland_UD_shp_df[i,]$long
    df_tmp[i,]$Latitude = lowland_UD_shp_df[i,]$lat
      # print(paste0( gDistance(defined_start_point  ,lowland_UD_shp_df[i,]) ))
  }
  closest_to_last_point = df_tmp[df_tmp$distance ==  min(df_tmp$distance),] 
  closest_to_last_point = SpatialPoints(closest_to_last_point[,c('Longitude', 'Latitude')], proj4string = CRS(proj))
  defined_start_point = closest_to_last_point
  # gDistance(defined_start_point, lowland_UD_shp_df)
  # d <- gDistance(sp.mydata, byid=T)
}
plot(lowland_UD_shp)
points(defined_start_point, col = 'blue', pch = 16)
  # lowland_UD_shp_df = SpatialPoints(lowland_UD_shp_df[,c('long', 'lat')])
  # proj4string(lowland_UD_shp_df) = proj
  # 
  # lowland_UD_shp_df$nearest <- apply(gDistance(defined_start_point, lowland_UD_shp_df, byid=TRUE), 1, which.min)
# [4] Simulate lowland movement ####
steps.df_lowland = rw_within_homerange(150, 200, lowland_UD_shp, 8, 'random_walk', defined_start_point)
steps.df_lowland = rw_within_homerange(150, 200, lowland_UD_shp, 8, 'levy_walk', defined_start_point)
# [5] Combine all three datasets" ####
migration = migration[,-1] # remove ndays column
migration = as.data.frame(migration)
migration = migration[,c(1:6)]
migration$Type = 'Migration'
steps.df$Type = 'Highland'
steps.df_lowland$Type = 'Lowland'
names(steps.df_lowland)
names(steps.df)[4] <- 'NSeeds'
names(steps.df) ==  names(migration)
names(steps.df) ==  names(steps.df_lowland)
names(migration) ==  names(steps.df_lowland)
anual_track = rbind(steps.df, migration, steps.df_lowland)
# anual_track = melt(steps.df, migration, steps.df_lowland, id.vars = 'Type')
message(paste0('We found a total of ', nrow(anual_track[anual_track$Poop_event == 1,])), ' poop event')
####
# [6] Make an animation ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
library("animation")
library("tweenr") # applies to both base R and ggplot2
library("gganimate") # makes 
library("dplyr")
library("gapminder") # where our data comes from
# Inputs:
ndays = 150

saveHTML({ # Take all figures out of the loop. and save as html. stirch ogether in html format
  for (idx in 2:ndays) {
    make_plot_all_movement_as_bg(idx)
  }
},
img.name = "lowland_migration_all_movements_bg", # I give it a unique name and file
htmlfile = "lowland_migration_all_movements_bg.html"
)


saveHTML({ # Take all figures out of the loop. and save as html. stirch ogether in html format
  for (idx in 2:ndays) {
    make_plot(idx)
  }
},
img.name = "lowland_migration", # I give it a unique name and file
htmlfile = "lowland_migration.html"
)
# .idx = 10
saveHTML({
  for (.idx in 2:ndays) {
    make_plot2(.idx)
  }
},
verbose = FALSE,
interval = .05,
loop = TRUE,
img.name = "sim_ggplot",
htmlfile = "sim_ggplot.html"
)

saveHTML({
  for (idx in 2:ndays) {
    make_plot3_emoji(idx)
  }
},
verbose = FALSE,
interval = .05,
loop = TRUE,
img.name = "sim_ggplot_emoji",
htmlfile = "sim_ggplot_emoji.html"
)
# Simulate the entire track


ndays = nrow(anual_track)
make_plot_all_track <- function(ndays){ # function input takes number from 2 to 8
  
  step_A <- anual_track[((1:ndays) -1),] # If I put 10, show me the first 9 points here
  step_B<- anual_track[((ndays) -1),] # The last point that will be input. zB if i input ndays 10 its the 10th day
  plot(x = step_A$Longitude,y = step_A$Latitude, type = "l",col = alpha("#A50026", .3),
       xlim = c(min(step_A$Longitude), max(step_A$Longitude)), ylim = c(min(step_A$Latitude), max(step_A$Latitude)))
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightgreen")
  points(x = step_A$Longitude,y = step_A$Latitude, type = "l",col = alpha("#A50026", .3),
         xlim = c(min(step_A$Longitude), max(step_A$Longitude)), ylim = c(min(step_A$Latitude), max(step_A$Latitude)))
  points(x = step_B$Longitude,y=step_B$Latitude,col = alpha("#A50026", 1),cex = 1.3,pch = 16)
  plot(lowland_UD_shp, col = alpha('red', 0.3), add=T)
  plot(highland_UD_shp, col = alpha('lightblue', 0.3), add=T)
  # If the moving point (tortoise) in the animation has poop_event = 1 make a purple color
  if(step_B$Poop_event == 1){
    points(step_B$Longitude, step_B$Latitude,col = 'brown', pch = 24, bg = alpha('chocolate4', 0.4 ))
  }
  
  step_A %>% subset(Poop_event == 1) %>% dplyr::select(Longitude, Latitude) %>%
    points(col = 'brown', pch = 24, bg = alpha('chocolate4', 0.4 ))
  
  Sys.sleep(.5)
}


saveHTML({
for (idx in 2:ndays) {
  print(idx)
  make_plot_all_track(idx)
}
  },
verbose = FALSE,
interval = .05,
loop = TRUE,
img.name = "all_tracks_2",
htmlfile = "all_tracks_2.html"
)
