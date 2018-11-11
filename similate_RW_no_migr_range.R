# Simulate random walks  within wintering and breedingg range:
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