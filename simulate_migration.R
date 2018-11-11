simulate_migration = function(steps.df, highland_UD_shp, lowland_UD_shp, avg_gut_retention_time){
# steps.df is the output of the random walk in the highlands # shapefiles of both winter and summer range
x1 =  steps.df$Longitude[nrow(steps.df)] # choose the last row of the random walker
y1 = steps.df$Latitude[nrow(steps.df)] = centroid(highland_UD_shp)[2]
# End point is a random point in the lowland.
sp =  spsample(lowland_UD_shp, 1, 'random') 
x2 = sp@coords[1]
y2 = sp@coords[2]
mean.pars <- c(x1 = x1, y1 = y1, x2 = x2, y2 = y2, t1 = 306, dt = 21) # start on julian day 306 which is november. And say migration can take 21 days # dt 21 it takes 21 days to make the transition
time <- 1:365; time <- 250:365 # One year
Mean <- getMu(T = time, p.m = mean.pars)
scan_track(time = time, x = Mean) # Tortoise migration
# To simulate the complete range shift message, we need only specify the ranging parameters, which are the ranging area A and, if needed, values of the autocorrelation time scales (tau_z and tau_v).
# Next, the mean process parameters - which must be a named vector (i.e., the order does not matter, but the names x1, x2, y1, y2, t1, dt are necessary.):
# The getMu() function simulates the mean process, generating a two-column matrix of x and y coordinates
taus <- c(tau.z = 1, tau.v = 0.5)
# r(t)# captures the spatial extent of the respective home ranges and features of the autocorrelation in the data 
# https://cran.r-project.org/web/packages/marcher/vignettes/marcher.html
lowland_UD_shp$Area_sqm <- area(lowland_UD_shp); area_low = lowland_UD_shp$Area_sqm
highland_UD_shp$Area_sqm <- area(highland_UD_shp); area_high = highland_UD_shp$Area_sqm

MOUF.sim <- simulate_shift(T = time, tau = c(tau.z = 5, tau.v = 1), mu = Mean, A = highland_UD_shp$Area_sqm) %>% scan_track
title("Position and Velocity Autocorrelation: MOUF", outer = TRUE)
# It is now quick and easy to compare models with more or less position and velocity autocorrelation TAU
# ESTIMATION RANGE SHIFT
MWN.sim = simulate_shift(T = time, tau = c(tau.z = 5, tau.v = 1), mu = Mean, A = highland_UD_shp$Area_sqm)
MWN.fit <- with(MWN.sim, estimate_shift(T=MWN.sim$T, X=MWN.sim$X, Y=MWN.sim$Y))
summary(MWN.fit)
plot(MWN.fit) # Note that in this visualization, the area circles are (dark to light) the 50% and 95% areas of use, whereas the dark and light blue lines in the time series figure reflect the confidence intervals around the estimated means, which are rather narrow.
# EXTRACT THE COORDINATES FROM STEP 306 to 321!
migration = data.frame(nday = MWN.fit$T[57:81], # steps where migration occurs ffrom day 306 to 330
Longitude = MWN.fit$X[57:81],
Latitude = MWN.fit$Y[57:81],
Poop_event = 0,
NSeeds = 0,
Nseeds_germinated = 0,
  N_seedling_to_shrub = 0
)

# Find the last time a tortoise pooped:
last_poop_highland = steps.df %>% subset(Poop_event == 1) %>% row.names()  %>% last() %>% as.numeric() # Get the last time a tortoise pooped
days_without_pooping_prior_to_migration_onset = nrow(steps.df) - last_poop_highland
# So before it started migrating, a tortoise has not pooped for 5 days zB, so at the third migration timestep there will be a poop event. 
dwpptmo = days_without_pooping_prior_to_migration_onset  # get a shorter acronym
first_poop_during_migration = avg_gut_retention_time - dwpptmo
end_migration = nrow(migration)
# These are the days of poop
days_of_poop_event <- seq(
  ( head(migration$nday, 1) + first_poop_during_migration ),
  tail(migration$nday, 1) , # this is the end of migration basically 25 days
                          avg_gut_retention_time)

migration[migration$nday %in% days_of_poop_event,]$Poop_event <- 1

proj =   "+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
migration_xysp <- SpatialPointsDataFrame(coords= migration[,c('Longitude', 'Latitude')], data = migration,proj4string = CRS(proj))
par(mfrow=c(1,1))
plot(migration_xysp)
plot(lowland_UD_shp, col = alpha('red', 0.3) , add=T)
plot(highland_UD_shp, col = alpha('forestgreen',0.3) , add=T)
migration_xysp %>% subset(Poop_event == 1) %>%
  points(col = 'brown', pch = 24, bg = alpha('chocolate4', 0.4 ))
title('Migration')
return(migration_xysp)
}