# I could draw number of seeds per dung pile of guava from a normal distribution based on tortoise seed numbers. There are 3 distributions, 1 for highland, 2 for transition zone and 3 for lowlands. 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Psidium guajava: Mean fruit weight: 59.8g and mean seeds per fruit 226. 
# 10 seeds planted per treatment -> 80 % germinate -> 1 out of 1 makes it to shrub.
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# https://cran.r-project.org/web/packages/emojifont/vignettes/emojifont.html
require(tidyverse)
require(MASS)
require(rgdal)
require(marcher)
require(smoove)
require(tidyverse)
require(MASS)
require(rgdal)
require(geosphere)
require(move)
require(lubridate)
require(raster)
setwd('/Users/diegoellis/projects/Yale_Classes/Quant_Methods_EEB/Project/Animations/')
lowland_UD_shp <- readOGR('/Users/diegoellis/Dropbox/Marius_Galapagos/Inputs/Seasonal_UD/LR_Lowland.shp')
highland_UD_shp <- readOGR('/Users/diegoellis/Dropbox/Marius_Galapagos/Inputs/Seasonal_UD/Highland_polygon_final_basedon90UD.shp')
load('/Users/diegoellis/projects/Yale_Classes/Quant_Methods_EEB/Project/Data/1km_hexagon.rdata')
lowland_UD_shp= spTransform(lowland_UD_shp,   "+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs" )
highland_UD_shp = spTransform(highland_UD_shp, proj4string(lowland_UD_shp))
range_shp = lowland_UD_shp
ndays = 150
daily_distance_moved = 200
avg_gut_retention_time = 8
guava_seed_per_dung_pile = 624  # average of 624 guava seeds
# We could also use the entire seed dataset take the mean and the standard deviation.
# y <- dnorm(x, mean=10, sd=3)
  germinatioN_success = 0.8
  seedling_to_shrub = 1/1000 # one out of every 1000 seedlings becomes a shrub
# Start at 1, then 9, then 17 ...
# Every nth event (discrete timestep) a pooping event happens
# Random walk within winter/summerrange
# Parameters are 150 days we simulate, move 200 meters per day, simulate the highland range, average gut retention time is 8 days
steps.df = rw_within_homerange(150, 200, highland_UD_shp, 8)
# Make an animation
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
    make_plot(idx)
  }
},
img.name = "lowland_migration", # I give it a unique name and file
htmlfile = "lowland_migration.html"
)

saveHTML({
  for (idx in 2:ndays) {
    make_plot2(idx)
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