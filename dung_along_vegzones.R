caca = read.csv('/Users/diegoellis/Desktop/Two datasets_MS/Species_short.csv')
require(tidyverse)
require(plyr)
require(dplyr)
caca_psidium = caca %>% subset(Species == 'Psidium guajava')
ddply(caca_psidium, 'Veg.zone', function(x){range(x$Altitude)})
# Low: 0-100
# Transition 101-250
# Highland 251-420
veg_zone_seeds = ddply(caca_psidium, 'Veg.zone', function(x){
  data.frame(
#     Veg_zone = unique(x$Veg.zone),
    Mean_seeds_per_zone = mean(x$N.seeds, na.rm = T),
    SD_seeds_per_zone = sd(x$N.seeds, na.rm = T),
    NDung = nrow(x)
    )
}
)

veg_zone_seeds = caca_psidium %>% group_by(Veg.zone) %>% dplyr::summarize(
  count = n(),
  Mean_seeds_per_zone = mean(N.seeds, na.rm = T),
  SD_seeds_per_zone = sd(N.seeds,na.rm = T))

par(mfrow=c(1,3))
distr_of_seeds_along_veg_zone <- apply(veg_zone_seeds, 1, function(x){
  library(truncnorm)
  rtruncnorm(n=x[2], a=0,  mean = x[3], sd = x[4]) # truncated normal distribution, cant be bellow zero
# hist(rtruncnorm(n=x[2], a=0,  mean = x[3], sd = x[4]), breaks = 20, main = paste0('Veg zone ', x[1]))
  }
) # Makes sense: veg zone 1 is right skewed, mostly little guava makes it to lowland, same but less extreme in veg zone 2 and veg zone 3 seems normal. 
low = unlist(distr_of_seeds_along_veg_zone[1])
transition = unlist(distr_of_seeds_along_veg_zone[2])
highland = unlist(distr_of_seeds_along_veg_zone[3])
rm(distr_of_seeds_along_veg_zone, veg_zone_seeds, caca_psidium, caca)