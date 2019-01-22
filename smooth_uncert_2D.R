
set.seed(248)
library(mgcViz)
library(viridis)
library(gridExtra)
# n = 200 or n = 1e5
dat <- gamSim(eg = 2, n = 1e5, dist = "normal", scale = 4, verbose = TRUE)

fit <- bamV(y ~ s(x, z), data = dat$data)

tru <- data.frame(x = rep(dat$truth$x, 40), 
           y = rep(dat$truth$z, each = 40),
           z = as.vector(dat$truth$f))


pl <- list()

pl1 <- plot(sm(fit, 1)) + l_fitRaster( ) + labs(title = NULL)

pl2 <- plot(sm(fit, 1)) + l_fitRaster(noiseup = T) +  labs(title = NULL)

pl3 <- plot(sm(fit, 1)) + l_fitRaster(pTrans = zto1(0.05, 3, 0.2)) + labs(title = NULL)

pl4 <- ggplot(data = tru, mapping = aes(x = x, y = y, fill = z)) + geom_raster() + 
  scale_fill_gradientn(colours = viridis(50, begin = 0.2)) + theme_bw() 

pl4 <-  list("ggObj" = pl4)
class(pl4) <- "plotSmooth"

gridPrint(pl1, pl4, pl2, pl3, ncol = 2)


#################################################
# Version with beta distributed covariates
#################################################

# set.seed(248)
# library(mgcViz)
# library(viridis)
# library(gridExtra)
# # n = 200 or n = 1e5
# dat <- gamSim(eg = 2, n = 1000, dist = "normal", scale = 4, verbose = TRUE)
# 
# ## Simulate data
# test1<-function(x,z,sx=0.3,sz=0.4)  
# { (pi**sx*sz)*(1.2*exp(-(x-0.2)^2/sx^2-(z-0.3)^2/sz^2)+
#                  0.8*exp(-(x-0.7)^2/sx^2-(z-0.8)^2/sz^2))
# }
# n <- 100000; scale = 4
# x <- rbeta(n, 2, 5); z <- rbeta(n, 5, 2);
# xs<-seq(0,1,length=40); zs<-seq(0,1,length=40)
# pr <- data.frame(x=rep(xs,40),z=rep(zs,rep(40,40)))
# truth <- matrix(test1(pr$x,pr$z),40,40)
# f <- test1(x,z)
# y <- f + rnorm(n)*scale
# data <- data.frame(y=y,x=x,z=z,f=f)
# truth <- list(x=xs,z=zs,f=truth)
# 
# fit <- bamV(y ~ s(x, z), data = data)
# 
# tru <- data.frame(x = rep(truth$x, 40), 
#                   y = rep(truth$z, each = 40),
#                   z = as.vector(truth$f))
# 
# 
# pl <- list()
# 
# pl1 <- plot(sm(fit, 1)) + l_fitRaster( ) + labs(title = NULL) + l_points() + l_rug()
# 
# pl2 <- plot(sm(fit, 1)) + l_fitRaster(noiseup = T) +  labs(title = NULL)
# 
# pl3 <- plot(sm(fit, 1)) + l_fitRaster(pTrans = zto1(0.05, 3, 0.2)) + labs(title = NULL)
# 
# pl4 <- ggplot(data = tru, mapping = aes(x = x, y = y, fill = z)) + geom_raster() + 
#   scale_fill_gradientn(colours = viridis(50, begin = 0.2)) + theme_bw() 
# 
# pl4 <-  list("ggObj" = pl4)
# class(pl4) <- "plotSmooth"
# 
# gridPrint(pl1, pl4, pl2, pl3, ncol = 2)



