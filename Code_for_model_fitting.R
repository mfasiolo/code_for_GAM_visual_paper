########################################################################
####### Code for model fitting
########################################################################

######
# Load packages and data
# mgcViz: version 0.1.1 as of 08/08/2018
# mgcv: version 1.8-24
######
library(mgcViz)
library(RhpcBLASctl); blas_set_num_threads(1) # Optional

# Load UK load data
load("UKData.RData")

#########
###### Gaussian GAM fit
#########
# This takes seconds
fit <- bam(NetDemand ~ Dow + JourFerie +
             NetDemand.48 +
             s(Tendance, k = 6, bs = "cr") + 
             te(wM, Instant, k = c(20, 10), bs = c("cr", "cc")) + 
             te(wM_s95, Instant, k = c(20, 10), bs = c("cr", "cc")) + 
             te(Posan, Instant, k = c(30, 20), bs = c("cc", "cc")),
           data = Data, discrete = F, nthreads = 4)

summary(fit)

# Save the fit 
# save(file = "gausUK_lag.RData", fit)

#########
###### Gaussian location-scale GAM fit
#########
# This can take hours
fitG <- gam(list(NetDemand ~ Dow + JourFerie +
                   NetDemand.48 + 
                   s(Tendance, k = 6, bs = "cr") + 
                   te(wM, Instant, k = c(20, 10), bs = c("cr", "cc")) + 
                   te(wM_s95, Instant, k = c(20, 10), bs = c("cr", "cc")) + 
                   te(Posan, Instant, k = c(30, 20), bs = c("cc", "cc")), 
                 ~ Dow + JourFerie + 
                   s(wM_s95, k = 20) + 
                   s(Instant, k = 20, bs = "cc") + 
                   s(Posan, k = 30, bs = "cc")),
            data = Data, family = gaulss, optimizer = c("efs"))

summary(fitG)

# Save the fit 
# save(file = "gaulssUK_lag.RData", fitG)


#########
###### Shash GAMLSS fit
#########
# WARNING: This can take days and GBytes of memory
# Without a good initialization for the smoothing parameters the first iterations are very
# slow (smoothing parameters seem not to move at all), but then convergence progresses relatively
# fast

# Good initialization for smoothing parameters (obtained from a previous fit)
initSP <- c(6.818117e-05, 2.145205e-04, 2.986198e-09, 1.245987e-04, 1.817169e-07, 
            1.042212e-05, 4.084534e-06, 3.594808e-01, 1.355517e+02, 2.619704e+01, 
            9.473822e+02)

library(mgcFam)
fitS <- gam(list(NetDemand ~ Dow + JourFerie +
                   NetDemand.48 + 
                   s(Tendance, k = 6, bs = "cr") + 
                   te(wM, Instant, k = c(20, 10), bs = c("cr", "cc")) + 
                   te(wM_s95, Instant, k = c(20, 10), bs = c("cr", "cc")) + 
                   te(Posan, Instant, k = c(30, 20), bs = c("cc", "cc")), 
                 ~ Dow + JourFerie + 
                   s(wM_s95, k = 20) + 
                   s(Instant, k = 20, bs = "cc") + 
                   s(Posan, k = 30, bs = "cc"), 
                 ~ Dow + 
                   JourFerie +
                   s(Instant, k = 20, bs = "cc"), 
                 ~ -1 + s(JourFerie, bs = "re", sp = 1e6)), 
            data = Data, 
            family = shash(a1=-1, a2=1), optimizer = c("outer", "bfgs"), 
            control = list(trace = TRUE, nthreads = 4),  
            in.out = list("sp" = initSP, "scale" = 1)) # Using good initialization

summary(fitS)

# Save the fit 
# save(file = "shash_final", fitS)

