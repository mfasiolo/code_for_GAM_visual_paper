###########################
# l_densCheck
###########################

library(mgcViz);
library(gridExtra);

lDensCheckFun <- function(fit, ngr, loc, ypo = -0.01, tol = -1){
  
  a <-check1D(fit, x = "x", maxpo = 500, type = "tnormal") + 
    l_densCheck(n = ngr, tol = tol) + l_points(shape = 19, size = 0.2) + 
    labs(title = NULL, y = "Residuals", x = "x")# + 
    #theme(legend.position="none") 
  
  estYcX <- mgcViz:::.fastKernDens(dat = a$data$res, xlimit = NULL, ylimit = NULL,
                                   cond = TRUE, bw = NULL, ngr = ngr, tol = tol)$dXY
  oldDat <- estYcX
  
  b <- list()
  for(ii in 1:length(loc)){
    
    a <- a + geom_vline(xintercept = oldDat$x1[loc[ii]])
    
    estYcX$fhat <- sqrt(oldDat$fhat[loc[ii], ])
    
    estYcX <- as.data.frame(estYcX)
    
    datN <- data.frame(x = estYcX$x2, y = sqrt(dnorm(estYcX$x2)))
    datN$dist <- sign(estYcX$fhat - datN$y) * abs(estYcX$fhat - datN$y)^(1/3)
    
    b[[ii]] <- ggplot(data = estYcX, mapping = aes(x = x2, y = fhat)) + geom_line() + 
      geom_line(data = datN, mapping = aes(x = x, y = y), inherit.aes = F, linetype = 2)+
      geom_point(data = datN, mapping = aes(x = x, y = ypo, colour = dist)) +
      labs(title = NULL, y = "Density", x = "Residuals") + theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_colour_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0)
  }
  
  return( list(a, b) )
  
}


##### 1 Well specified
set.seed(424)
n <- 1e4
dataf <- data.frame("y" = rnorm(n), "x" = runif(n))
names(dataf) <- c("y", "x")

pl <- list()

## Fit model

fit <- gamV(form = y ~ x, data = dataf) # Here we are using a new optimizer 

ngr <- c(81, 81)
loc <- 41

plts <- lDensCheckFun(fit = fit, ngr = c(81, 81), loc = 41)

grid.arrange(grobs = list(plts[[1]]$ggObj, plts[[2]][[1]]), ncol = 2)




##### 2 Varying mean
library(mgcFam)
##  Simulate some data form shash
set.seed(847)
n <- 10000
x <- sort(runif(n, -4, 4))

X <- cbind(1, x, x^2)
beta <- c(4, 1, 0.1)
mu <- X %*% beta 

sigma =  .5 #+0.4*(x+4)*.5            # Scale
eps = 0 #2*sin(x)                     # Skewness
del = 1 #1 + 0.2*cos(3*x)             # Kurtosis

dat <-  mu + (del * sigma) * sinh((1/del) * asinh(qnorm(runif(n))) + (eps/del))
dataf <- data.frame(cbind(dat, x))
names(dataf) <- c("y", "x")

## Fit model
fit <- gamV(form = list(y ~ x, ~ 1, ~ 1, ~1), data = dataf, family = shash) # Here we are using a new optimizer 

plts <- lDensCheckFun(fit = fit, ngr = c(81, 81), loc = c(11, 41, 71), ypo = -0.02)

plts[[1]] <- plts[[1]] + annotate(geom="text", x=-3.2, y=4, label="a)") + 
            annotate(geom="text", x=-0.2, y=4, label="b)") + 
            annotate(geom="text", x=2.8, y=4, label="c)")

plts[[2]][[1]] <- plts[[2]][[1]] + annotate(geom="text", x=-3.5, y=0.8, label="a)") 
plts[[2]][[2]] <- plts[[2]][[2]] + annotate(geom="text", x=-3.5, y=0.8, label="b)") 
plts[[2]][[3]] <- plts[[2]][[3]] + annotate(geom="text", x=-3.5, y=0.8, label="c)") 

grid.arrange(grobs = list(plts[[1]]$ggObj, plts[[2]][[1]], plts[[2]][[2]], plts[[2]][[3]]), ncol = 2)


##### 3 Varying variance 
set.seed(847)
n <- 10000
x <- sort(runif(n, -4, 4))

X <- cbind(1, x, x^2)
beta <- c(4, 1, 0.1)
mu <- 1 #X %*% beta 

sigma =  .5 +0.3*(x+4)*.5            # Scale
eps = 0 #2*sin(x)                     # Skewness
del = 1 #1 + 0.2*cos(3*x)             # Kurtosis

dat <-  mu + (del * sigma) * sinh((1/del) * asinh(qnorm(runif(n))) + (eps/del))
dataf <- data.frame(cbind(dat, x))
names(dataf) <- c("y", "x")

## Fit model
fit <- gamV(form = list(y ~ x, ~ 1, ~ 1, ~1), data = dataf, family = shash) # Here we are using a new optimizer 

plts <- lDensCheckFun(fit = fit, ngr = c(81, 81), loc = c(11, 41, 71), ypo = -0.02)

plts[[1]] <- plts[[1]] + annotate(geom="text", x=-3.2, y=4.3, label="a)") + 
  annotate(geom="text", x=-0.2, y=4.3, label="b)") + 
  annotate(geom="text", x=2.8, y=4.3, label="c)")

plts[[2]][[1]] <- plts[[2]][[1]] + annotate(geom="text", x=-3.5, y=0.8, label="a)") 
plts[[2]][[2]] <- plts[[2]][[2]] + annotate(geom="text", x=-3.5, y=0.8, label="b)") 
plts[[2]][[3]] <- plts[[2]][[3]] + annotate(geom="text", x=-3.5, y=0.8, label="c)") 

grid.arrange(grobs = list(plts[[1]]$ggObj, plts[[2]][[1]], plts[[2]][[2]], plts[[2]][[3]]), ncol = 2)

##### 4 Varying skewness
set.seed(847)
n <- 10000
x <- sort(runif(n, -4, 4))

X <- cbind(1, x, x^2)
beta <- c(4, 1, 0.1)
mu <- 1 #X %*% beta 

sigma =  .5 #+0.3*(x+4)*.5            # Scale
eps = 0.65*sin(x*0.6)                      # Skewness
del = 1 #1 + 0.2*cos(3*x)             # Kurtosis

dat <-  mu + (del * sigma) * sinh((1/del) * asinh(qnorm(runif(n))) + (eps/del))
dataf <- data.frame(cbind(dat, x))
names(dataf) <- c("y", "x")

## Fit model
fit <- gamV(form = list(y ~ x, ~ s(x), ~ 1, ~1), data = dataf, family = shash) # Here we are using a new optimizer 

plts <- lDensCheckFun(fit = fit, ngr = c(81, 81), loc = c(11, 41, 71), ypo = -0.02)

plts[[1]] <- plts[[1]] + annotate(geom="text", x=-3.2, y=4.8, label="a)") + 
  annotate(geom="text", x=-0.2, y=4.8, label="b)") + 
  annotate(geom="text", x=2.8, y=4.8, label="c)")

plts[[2]][[1]] <- plts[[2]][[1]] + annotate(geom="text", x=-3.5, y=0.7, label="a)") 
plts[[2]][[2]] <- plts[[2]][[2]] + annotate(geom="text", x=-3.5, y=0.7, label="b)") 
plts[[2]][[3]] <- plts[[2]][[3]] + annotate(geom="text", x=-3.5, y=0.7, label="c)") 

grid.arrange(grobs = list(plts[[1]]$ggObj, plts[[2]][[1]], plts[[2]][[2]], plts[[2]][[3]]), ncol = 2)


##### 4 Varying kurtosis
set.seed(847)
n <- 10000
x <- sort(runif(n, -4, 4))

X <- cbind(1, x, x^2)
beta <- c(4, 1, 0.1)
mu <- 1 #X %*% beta 

sigma =  .5 #+0.3*(x+4)*.5            # Scale
eps = 0 #0.65*sin(x*0.6)                      # Skewness
del = 1 + 0.65*cos(1*x)             # Kurtosis

dat <-  mu + (del * sigma) * sinh((1/del) * asinh(qnorm(runif(n))) + (eps/del))
dataf <- data.frame(cbind(dat, x))
names(dataf) <- c("y", "x")

## Fit model
fit <- gamV(form = list(y ~ x, ~ s(x), ~ 1, ~1), data = dataf, family = shash) # Here we are using a new optimizer 

plts <- lDensCheckFun(fit = fit, ngr = c(81, 81), loc = c(11, 41, 71), ypo = -0.02)

plts[[1]] <- plts[[1]] + annotate(geom="text", x=-3.2, y=5.8, label="a)") + 
  annotate(geom="text", x=-0.2, y=5.8, label="b)") + 
  annotate(geom="text", x=2.8, y=5.8, label="c)")

plts[[2]][[1]] <- plts[[2]][[1]] + annotate(geom="text", x=-3.5, y=0.7, label="a)") 
plts[[2]][[2]] <- plts[[2]][[2]] + annotate(geom="text", x=-3.5, y=0.7, label="b)") 
plts[[2]][[3]] <- plts[[2]][[3]] + annotate(geom="text", x=-3.5, y=0.7, label="c)") 

grid.arrange(grobs = list(plts[[1]]$ggObj, plts[[2]][[1]], plts[[2]][[2]], plts[[2]][[3]]), ncol = 2)

