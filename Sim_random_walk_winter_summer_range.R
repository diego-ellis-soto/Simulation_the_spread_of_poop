# https://cran.r-project.org/web/packages/emojifont/vignettes/emojifont.html
require(tidyverse)
require(MASS)
require(rgdal)
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
rw_within_homerange = function(ndays, daily_distance_moved, range_shp, avg_gut_retention_time){
  message('May I suggest setting a seed?')
  sp =  spsample(range_shp, 1, 'random') # startin point is a random point within the shape (winter and breeding range)
  
  days_of_poop_event <- seq(1, daily_distance_moved,
                            avg_gut_retention_time)
  
  
  steps.df <- data.frame(matrix(0,ndays,6)) # 2 # Add the output to a big data frame
  # Columns: Lat/Long, poop_event, Nseed_in_timestep, Germinate,
  # There is a third columns now that has poop event
  colnames(steps.df) <- c("Longitude", "Latitude", 'Poop_event',
                          'NSeeds', 'Nseeds_germinated', 'N_seedling_to_shrub')
  
  # Ignacio: How do I add 20 000 individuals on top?
  for(i in 1:ndays){ # i = 1; i =2
    print(i)
    if(i == 1){
      #  IGNACIO HELP HERE: HOW DO I DO CUMSUM INSIDE A LOOP?
      xy = mvrnorm(1, c(0,0), matrix(c(daily_distance_moved,0,0,daily_distance_moved),2,2))
      steps.df[i, 1] <- xy[1] # + sp@coords[1] # Add the random walk with longitude
      steps.df[i, 2] <- xy[2] # + sp@coords[2]
      
      if(i %in% days_of_poop_event){
        message('Day ', i, ' had a poop event')
        steps.df[i, 3] <- 1 # 1 means a poop event
      }
        
    }else{
      xy   = mvrnorm(1, c(steps.df[(i-1),1], steps.df[(i-1),2]), matrix(c(daily_distance_moved,0,0,daily_distance_moved),2,2))
      require(rgeos)
      
      tmp_xy =   data.frame(Longitude = ( xy[1] + sp@coords[1]),
                            Latitude = ( xy[2]  + sp@coords[2] ))
      tmp_xy <- SpatialPoints(tmp_xy)
      proj4string(tmp_xy) <- proj4string(range_shp)
      
      
      if(gContains(range_shp,tmp_xy) == FALSE){
        cat('Step was outside the species range, skipping it')
        next
      } # If the random step is outside the polygon, draw the step again
      # point.in.polygon(tmp_xy@coords[1],tmp_xy@coords[2], bbox(range_shp))
      # plot(range_shp)
      # plot(tmp_xy, col = 'green', add=T)
      # tmp_xy %over% range_shp
      # any( !is.na(over(tmp_xy, range_shp)))
      # inside.park <- !is.na(over(bears, as(parks, "SpatialPolygons")))
      steps.df[i, 1] <- xy[1] # + sp@coords[1] # Add the random walk with longitude
      steps.df[i, 2] <- xy[2] # + sp@coords[2]
      
      if(i %in% days_of_poop_event){
        message('Day ', i, ' had a poop event')
        steps.df[i, 3] <- 1 # 1 means a poop event
      }
      
      
    }
  }
  # Alternatively the starting point can be the centroid of the homerange
  # steps.df[i, 1] <- xy[1] + sp@coords[1] # Add the random walk with longitude
  # steps.df[i, 2] <- xy[1] + sp@coords[2]
  steps.df[, 1] = steps.df[, 1] + sp@coords[1] # Add the random walk with longitude
  steps.df[, 2] = steps.df[, 2] + sp@coords[2]
  
proj =   "+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj = proj4string(range_shp)
xysp <- SpatialPointsDataFrame(coords= steps.df[,c('Longitude', 'Latitude')], data = steps.df,proj4string = CRS(proj))
last_point = tail(xysp,1)
last_point = SpatialPoints(last_point)
proj4string(last_point) = proj
#  xysp <- SpatialPoints(steps.df)
#   proj4string(xysp) <- proj4string(range_shp)
  par(mfrow=c(1,1))
  plot(range_shp)
  plot(xysp, type = 'l',add=T)
  points(last_point, col = 'blue')
  plot( sp, col = 'red', add=T)
  
  plot(xysp@coords, type = 'l', main = 'Brownian motion in wintering range', lwd = 1.5)
  points( sp, col = 'red', pch = 16) # first point
  points(last_point, col = 'blue', pch = 16) # last point
  xysp %>% subset(Poop_event == 1) %>%
     points(col = 'brown', pch = 24, bg = alpha('chocolate4', 0.4 ))
  # points(tail(xysp@coords,1), col = 'blue', pch = 16)
  return(steps.df)
}

steps.df = rw_within_homerange(150, 200, lowland_UD_shp, 8)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Psidium guajava: Mean fruit weight: 59.8g and mean seeds per fruit 226. 
# 10 seeds planted per treatment -> 80 % germinate -> 1 out of 1 makes it to shrub.
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Make an animation
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
library("animation")
library("tweenr") # applies to both base R and ggplot2
library("gganimate") # makes 
library("dplyr")
library("gapminder") # where our data comes from
# Inputs:

ndays = 150




make_plot_all_movement_as_bg <- function(ndays){ # function input takes number from 2 to 8
  
  
  plot(x = steps.df$Longitude,y = steps.df$Latitude, type = "l",col = alpha("#A50026", .3),
       xlim = c(min(step_A$Longitude), max(step_A$Longitude)), ylim = c(min(step_A$Latitude), max(step_A$Latitude)))
  
  step_A <- steps.df[((1:ndays) -1),] # If I put 10, show me the first 9 points here
  step_B<- steps.df[((ndays) -1),] # The last point that will be input. zB if i input ndays 10 its the 10th day
  # plot(x = step_A$Longitude,y = step_A$Latitude, type = "l",col = alpha("#A50026", .3),
  #      xlim = c(min(step_A$Longitude), max(step_A$Longitude)), ylim = c(min(step_A$Latitude), max(step_A$Latitude)))
  # rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightgreen")
  points(x = step_A$Longitude,y = step_A$Latitude, type = "l",col = alpha("#A50026", .3),
         xlim = c(min(step_A$Longitude), max(step_A$Longitude)), ylim = c(min(step_A$Latitude), max(step_A$Latitude)))
  points(x = step_B$Longitude,y=step_B$Latitude,col = alpha("#A50026", 1),cex = 1.3,pch = 16)
  
  # If the moving point (tortoise) in the animation has poop_event = 1 make a purple color
  if(step_B$Poop_event == 1){
    points(step_B$Longitude, step_B$Latitude,col = 'brown', pch = 24, bg = alpha('chocolate4', 0.4 ))
  }
  
  step_A %>% subset(Poop_event == 1) %>% dplyr::select(Longitude, Latitude) %>%
    points(col = 'brown', pch = 24, bg = alpha('chocolate4', 0.4 ))
  
  Sys.sleep(.5)
}

ndays = 150
saveHTML({ # Take all figures out of the loop. and save as html. stirch ogether in html format
  for (idx in 2:ndays) {
    make_plot_all_movement_as_bg(idx)
  }
},
img.name = "lowland_migration_all_movements_bg", # I give it a unique name and file
htmlfile = "lowland_migration_all_movements_bg.html"
)





make_plot <- function(ndays){ # function input takes number from 2 to 8
  
  step_A <- steps.df[((1:ndays) -1),] # If I put 10, show me the first 9 points here
  step_B<- steps.df[((ndays) -1),] # The last point that will be input. zB if i input ndays 10 its the 10th day
  plot(x = step_A$Longitude,y = step_A$Latitude, type = "l",col = alpha("#A50026", .3),
       xlim = c(min(step_A$Longitude), max(step_A$Longitude)), ylim = c(min(step_A$Latitude), max(step_A$Latitude)))
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightgreen")
  points(x = step_A$Longitude,y = step_A$Latitude, type = "l",col = alpha("#A50026", .3),
         xlim = c(min(step_A$Longitude), max(step_A$Longitude)), ylim = c(min(step_A$Latitude), max(step_A$Latitude)))
  points(x = step_B$Longitude,y=step_B$Latitude,col = alpha("#A50026", 1),cex = 1.3,pch = 16)
  
  # If the moving point (tortoise) in the animation has poop_event = 1 make a purple color
  if(step_B$Poop_event == 1){
    points(step_B$Longitude, step_B$Latitude,col = 'brown', pch = 24, bg = alpha('chocolate4', 0.4 ))
  }
  
  step_A %>% subset(Poop_event == 1) %>% dplyr::select(Longitude, Latitude) %>%
  points(col = 'brown', pch = 24, bg = alpha('chocolate4', 0.4 ))
  
  Sys.sleep(.5)
}



ndays = 100
for (idx in 2:ndays) {
  make_plot(idx) # loop.  # in order to use animation package we need to do this
} # loop through soemthing that acts as frame for each we need to draw a picture
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
ndays = 150
saveHTML({ # Take all figures out of the loop. and save as html. stirch ogether in html format
  for (idx in 2:ndays) {
    make_plot(idx)
  }
},
img.name = "lowland_migration", # I give it a unique name and file
htmlfile = "lowland_migration.html"
)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# This is the ggplot version of the base R function
make_plot2 <- function(ndays) {
  step_A <- steps.df[((1:ndays) -1),] 
  step_B<- steps.df[((ndays) -1),] 
  poop_events = step_A %>% subset(Poop_event == 1) %>% dplyr::select(Longitude, Latitude)
  rownames(poop_events) <- NULL
  p <- ggplot(step_A, aes(x = Longitude, y = Latitude))+
    geom_path(alpha = .3)+
    geom_point(aes(step_B$Longitude, step_B$Latitude),color = '#A50026',fill = '#A50026',alpha = 1) +
  theme_classic()+
    coord_cartesian(xlim = c(min(step_A$Longitude), max(step_A$Longitude)),
                    ylim = c(min(step_A$Latitude), max(step_A$Latitude)))
    
  p = p + geom_point(data = poop_events,aes(x = poop_events$Longitude, y = poop_events$Latitude), color = "chocolate4", fill = '#E69F00')+ theme(legend.position="none")+  ggtitle(label = .idx) 
  print(p)
  Sys.sleep(.5) # r pauses half a second, make a fake animation
}
ndays = 150
for(idx in 2:ndays) {
  make_plot2(idx)
}



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
library("tweenr")
# devtools::install_github("dill/emoGG")
require(emoGG)
emoji_search("turtle")
emoji_search("poop")


make_plot3_emoji = function(ndays) {
  require(tidyverse)
  require(emoGG)
  step_A <- steps.df[((1:ndays) -1),] 
  step_B<- steps.df[((ndays) -1),] 
  poop_events = step_A %>% subset(Poop_event == 1) %>% dplyr::select(Longitude, Latitude)
  rownames(poop_events) <- NULL
p = ggplot(step_A, aes(x = Longitude, y = Latitude))+ geom_path(alpha = .3)+
  geom_emoji(data = step_B, aes(step_B$Longitude, step_B$Latitude), emoji = "1f422") +
  theme_classic()+
  coord_cartesian(xlim = c(min(step_A$Longitude), max(step_A$Longitude)),
                  ylim = c(min(step_A$Latitude), max(step_A$Latitude))) +
  geom_emoji(data = poop_events, aes(poop_events$Longitude, poop_events$Latitude), emoji = "1f4a9")
print(p)
Sys.sleep(.5) # r pauses half a second, make a fake animation
}

ndays = 150
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
