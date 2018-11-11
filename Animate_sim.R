library("animation")
library("tweenr") # applies to both base R and ggplot2
library("gganimate") # makes 
library("dplyr")
library("gapminder") # where our data comes from
# This script is for animations
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
# saveHTML({ # Take all figures out of the loop. and save as html. stirch ogether in html format
#   for (idx in 2:ndays) {
#     make_plot_all_movement_as_bg(idx)
#   }
# },
# img.name = "lowland_migration_all_movements_bg", # I give it a unique name and file
# htmlfile = "lowland_migration_all_movements_bg.html"
# )

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
# for (idx in 2:ndays) {
#   make_plot(idx) # loop.  # in order to use animation package we need to do this
# } # loop through soemthing that acts as frame for each we need to draw a picture
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# ndays = 150
# saveHTML({ # Take all figures out of the loop. and save as html. stirch ogether in html format
#   for (idx in 2:ndays) {
#     make_plot(idx)
#   }
# },
# img.name = "lowland_migration", # I give it a unique name and file
# htmlfile = "lowland_migration.html"
# )
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
# for(idx in 2:ndays) {
#   make_plot2(idx)
# }

# saveHTML({
#   for (idx in 2:ndays) {
#     make_plot2(idx)
#   }
# },
# verbose = FALSE,
# interval = .05,
# loop = TRUE,
# img.name = "sim_ggplot",
# htmlfile = "sim_ggplot.html"
# )
# library("tweenr")
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
# saveHTML({
#   for (idx in 2:ndays) {
#     make_plot3_emoji(idx)
#   }
# },
# verbose = FALSE,
# interval = .05,
# loop = TRUE,
# img.name = "sim_ggplot_emoji",
# htmlfile = "sim_ggplot_emoji.html"
# )

# Idea: Using marcher: First point is last poimt of tortoise random walk and endpoint is a random point in the lowland range: 