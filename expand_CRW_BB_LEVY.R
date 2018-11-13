# ADD BB TO MIGRATION, LEVY WALK AND CRW TO WITHIN BOUNDARY, READ BRETT CODE!
# MODIFY THE FUNCTION MOVE WITHIN SUMMER OR WINTER RANGE:
# YOU CAN DEFINE WHAT TYPE OF MOVEMENT YOU WANT YOUR TORTOISES TO DO OR ALL OF THEM FOR SEEING HOW ACTIVE SUBSIDIES ARE DISPERSED IN THE RANGES UNDER DIFERENT MOVEMENT MODELS
# INPUT:
# [A] Brownian Motion
# [B] Correlated random walk
# [C] Levy Wal
# [D] MOU model (good for sedentary animal)
# [E] Something else
# [F] Brownian Bridge between migration

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Simulate movement: Levy Walks:
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# MODIFY BOTH MIGRATION AND NO MIGRATION FUNCTION TO BE ABLE TO INCLUDE LEVY WALK AND BROWNIAN BRIDGE
require(adehabitatLT)
sp =  spsample(range_shp, 1, 'random')
lv4 = simm.levy(date = 1:150, mu = 3, l0 = 200, x0 = c(sp@coords), # minimum lengh of a step l0
                id = "A1", burst = "mu = 3.0", typeII = TRUE,
                proj4string=CRS("+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(lv4)

# simm.mou can be used to simulate a bivariate Ornstein-Uhlenbeck motion (often used to describe the sedentarity of an animal, e.g. Dunn and Gipson 1977);
# FUNCTION LOWLAND: LEVY, MOU, depending if i want my animal to be sedentary, or not.simm.mba can be used to simulate an arithmetic Brownian motion (with a drift parameter and a covariance between the coordinates, see Brillinger et al. 2002);
# simm.crw can be used to simulate a correlated random walk. This model has been often used to describe animal movements (Kareiva and Shigesada 1983);
# simm.levy can be used to simulate a Levy walk, as described (Bartumeus et al. 2005).
begin_sp =  spsample(highland_UD_shp, 1, 'random') # startin point is a random point within the shape (winter
end_sp =  spsample(lowland_UD_shp, 1, 'random') # startin point is a random point within the shape (winter
sims = simm.bb(1:30, begin = c(begin_sp@coords), end = c(end_sp@coords), id = "Animal1",
        burst = 'Animal1', proj4string=CRS("+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(sims)




simsM = simm.mou(1:3000, b = c(end_sp@coords), a = diag(0.5, 2), sigma = diag(2),  x0 = c(begin_sp@coords), id = "Animal1",burst = 'Animal1', proj4string=CRS("+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(simsM)
# SMITHSONIAN
# Multistage random walk: (c) Bias switch
# BROWNIAN BRIDGE! -> ADD MIGRATION PACKAGE TO IT!
require('waddle')
simsCRW =  simm.crw(1:150, x0 = c(begin_sp@coords), h = 200, r = 0.99, id = "Animal1",burst = 'Animal1', proj4string=CRS("+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# Multistage changing parameters during migration
plot(simsCRW)

# Since the seminal paper of Kareiva and Shigesada (1983), most biologists describe the trajectories of an animal with the help of two distributions: the distribution of distances between successive relocations, and the distribution of turning angles between successive moves (relative angles in the class ltraj). The CRW is built iteratively. At each step of the simulation process, the orientation of the move is drawn from a wrapped normal distribution (with concentration parameter r). The length of the move is drawn from a chi distribution, multiplied by h * sqrt(dt). h is a scale parameter (the same as in the function simm.brown(), and the distribution is multiplied by sqrt(t) to make it similar to the discretized Brownian motion if r == 0.

# You may have a look at the functions 'NMs.CRW' and 'NMs.randomCRW' in the 
#package 'adehabitatLT'. Please check the vignette of the package, section 5 
#(this is quite long, but really worth it), with emphasis on 5.3 and 5.7:
  # SIMULATE WITHIN A BOUNDARY:
# testNM
# 
# plotfun <- function(x, par){
#   image(par)
#        points(x[,1:2], pch=16)
#        lines(x[,1:2])
#    return(x) }
# 
# confun <- function(x, par){
#        ## Define a SpatialPointsDataFrame from the trajectory
#          coordinates(x) <- x[,1:2]
#          ## overlap the relocations x to the elevation map par
#            jo <- join(x, par)
#            ## checks that there are no missing value
#              res <- all(!is.na(jo))
#              ## return this check
#           return(res)}
# 
# nmo2 <- NMs.randomShiftRotation(na.omit(boar1), rshift = TRUE, rrot = TRUE,
#                                 rx = range(xo[,1]), ry = range(xo[,2]),
#                                 treatment.func = plotfun,
#                                 treatment.par = puechabonsp$map[,1],
#                                 constraint.func = confun,
#                                 constraint.par = puechabonsp$map[,1],
#                                 nrep=9)