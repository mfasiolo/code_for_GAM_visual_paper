########################################################################
####### Code for generating plot, given fitted models
########################################################################

library(dplyr)
library(mgcViz)
library(gridExtra)
library(ggplot2)
library(viridis)
library(e1071)

load("UKData.RData")

##############################
## GAUSSIAN GAM model
##############################
# Load fitted model and convert it to gamViz object
load("gausUK_lag.RData")
set.seed(555)
v <- getViz(fit, nsim = 50)

##### Code for Figure 1
pl <- list()
pl[[1]] <- plot(sm(v, 1)) + l_dens(type = "cond", n = c(50, 50), tol = -1, trans = function(.x) .x^(0.33)) +
  l_fitLine() +
  labs(x = "t", y = expression(f[1] * "(t) (in 100s)")) + #ylim(-7500, 7500) + 
  coord_cartesian(expand=F)
pl[[4]] <- qq(v, rep = 50, show.reps = T, level = 0.99, method = "tnormal", CI = "normal", 
               type = "tnormal", worm = F, a.replin = list(alpha = 0.2))[[1]] + 
  labs(title = NULL, y = "Observed quantiles")
pl[[2]] <- check1D(v, x = "wM_s95", maxpo = 1e3, type = "tnormal") + l_densCheck(tol = -1) + 
  guides(fill=FALSE) + labs('x' = expression(T^s), 'y' = "Quantile residuals") + 
  l_rug(mapping = aes(x = x, y = y)) # + coord_cartesian(expand=F)
pl[[3]] <- check1D(v, x = "wM_s95", maxpo = 1e3, type = "scaled.pearson") + 
  l_gridCheck1D(gridFun = function(.x) sd(.x), n = 30, stand = "none", show.reps = F) +
  theme(panel.grid.minor = element_line(size = 0.1, colour = "grey"), 
                  panel.grid.major = element_line(size = 0.1, colour = "grey")) + 
  guides(fill=FALSE) + labs('x' = expression(T^s), "y" = "sd(r)")
pl[[5]] <- check1D(v, x = "Posan", maxpo = 1e3, type = "tnormal") + l_densCheck(tol = -1) + 
  guides(fill=FALSE) + labs('x' = "toy", 'y' = "Quantile residuals") + 
  l_rug(mapping = aes(x = x, y = y)) # + coord_cartesian(expand=F)
pl[[6]] <-  check1D(v, x = "Posan", maxpo = 1e3, type = "scaled.pearson") + 
  l_gridCheck1D(gridFun = function(.x) sd(.x), n = 30, stand = "none", show.reps = F) +
  theme(panel.grid.minor = element_line(size = 0.1, colour = "grey"), 
                  panel.grid.major = element_line(size = 0.1, colour = "grey")) + 
  guides(fill=FALSE) + labs('x' = "toy", "y" = "sd(r)")

pl0 <- pl

for(ii in c(1, 2, 3, 5, 6)){
  pl[[ii]] <- pl[[ii]]$ggObj
}

# Changing axes marks
tkX <- quantile(Data$Tendance, c(0.15, 0.5, 0.85))
laX <- round(nrow(Data) * c(0.15, 0.5, 0.85))
tmp <- c(pl[[1]]$layers[[1]]$data$y)
tkY <- seq(min(tmp), max(tmp), length.out = 4)
laY <- round(tkY / 100)
pl[[1]] <- pl[[1]] + theme(legend.position="none") + 
  scale_x_continuous(breaks = tkX, labels = laX) + 
  scale_y_continuous(breaks = tkY, labels = laY)

kk <- 1
cols <- c("white", rep("black", 5))
for(xx in c("a", "c", "e", "b", "d", "f")){
  xpos <- c(-Inf,-Inf,Inf,Inf)
  ypos <- c(-Inf, Inf,-Inf,Inf)
  annotateText <- c("",xx,"","")
  hjustvar<-c(0,-0.8,0.99,1) 
  vjustvar<-c(-1,1.2,-1,1)
  
  pl[[kk]] <-  pl[[kk]] + geom_text(data=data.frame(xpos=xpos,ypos=ypos), inherit.aes = F,
                                    aes_(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=force(annotateText)), 
                                    color=cols[kk])
  kk <- kk+1
}

# Plotting
grid.arrange(grobs=pl, ncol = 3)

##############################
## Gaussian location-scale GAM
##############################
# Load fitted GAM model
load("gaulssUK_lag.RData")

# Add function for simulating observations
fitG$family$rd <- function(mu,wt,scale) {
  ## simulate data 
  return( rnorm(nrow(mu), mu[ , 1], sqrt(scale/wt)/mu[ , 2]) )
} ## rd

# Convert it to gamViz object
set.seed(555)
v <- getViz(fitG, nsim = 50)

##### Code for Figure 2
pl <- list()
pl[[1]] <- plot(sm(v, 5), n = 200) + l_ciPoly() + l_fitLine() + 
  l_ciLine(colour = 2, linetype = 2) + ylim(-0.9, 0.38) + 
  labs(x = expression(T^s), y = expression(v[1] * "(" * T^s * ")"))
pl[[2]] <- plot(sm(v, 6), n = 200) + l_ciPoly() + l_fitLine() + 
  l_ciLine(colour = 2, linetype = 2) +  
  labs(x = "Instant", y = expression(v[2] * "(" * I * ")"))
pl[[3]] <- plot(sm(v, 7), n = 200) + l_ciPoly() + l_fitLine() + 
  l_ciLine(colour = 2, linetype = 2) + ylim(-0.3, 1.21) +
  labs(x = "toy", y = expression(v[3] * "(toy)"))

library( "e1071" )
fn <- function(.x, .sdr){
  .a <- skewness(.x) 
  .si <- sign(.a)
  return( .si*sqrt(abs(.a)) )
}

set.seed(784)
pl[[4]] <- check2D(v, x1 = "wM_s95", x2 = "Instant") + 
  l_gridCheck2D(bw = c(1.5, 3), gridFun = sd) + 
  scale_fill_gradientn(colours = viridis(50,begin=0), na.value="white") + 
  coord_cartesian(expand=F) + labs(x = expression(T^s))
pl[[5]] <- check2D(v, x1 = "Posan", x2 = "Instant") + 
  l_gridCheck2D(bw = c(0.02, 1), gridFun = skewness) + 
  scale_fill_gradientn(colours = viridis(50,begin=0), na.value="white") + 
  coord_cartesian(expand=F) + labs(x = expression("toy"))
pl[[6]] <- check1D(v, x = "wM_s95", type = "deviance") + 
  l_gridCheck1D(n = 30, gridFun = sd, stand = "none", show.reps = F) +
  theme_bw() +  guides(fill=FALSE) + 
  labs('x' = expression(T^s), "y" = "sd(r)")
pl[[7]] <- check1D(v, x = "Instant", type = "deviance") + 
  l_gridCheck1D(n = 30, gridFun = skewness, stand = "none", show.reps = F) +
  theme_bw() +  guides(fill=FALSE) + 
  labs('x' = "Instant", "y" = "skew(r)")

pl0 <- pl

kk <- 1
cols <- c(rep("black", 3), c("black", "black"), rep("black", 2))
for(xx in c("a", "b", "c", "d", "e", "f", "g")){
  xpos <- c(-Inf,-Inf,Inf,Inf)
  ypos <- c(-Inf, Inf,-Inf,Inf)
  annotateText <- c("",xx,"","")
  hjustvar<-c(0,-0.8,0.99,1) 
  vjustvar<-c(-1,1.2,-1,1)
  
  pl[[kk]] <-  pl[[kk]] + geom_text(data=data.frame(xpos=xpos,ypos=ypos), inherit.aes = F,
                                    aes_(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=force(annotateText)), 
                                    color=cols[kk])
  kk <- kk+1
}

lay <- matrix(c(1,1,1,1,4,4,4,4,4,6,6,6,6,
                1,1,1,1,4,4,4,4,4,6,6,6,6,
                2,2,2,2,4,4,4,4,4,6,6,6,6,
                2,2,2,2,5,5,5,5,5,7,7,7,7,
                3,3,3,3,5,5,5,5,5,7,7,7,7,
                3,3,3,3,5,5,5,5,5,7,7,7,7), 6, 13, byrow = T)

grid.arrange(grobs=lapply(pl, "[[", "ggObj"), layout_matrix = lay)


##############################
## Shash GAM model
##############################
# Load fitted model
load("shash_final.RData")
set.seed(555)
v <- getViz(fitS, nsim = 50)

library(plyr)

##### Code for Figures 3 and 4


Dat2 <- Data
Dat2$Dow <- factor(Dat2$Dow, levels(Data$Dow)[ c(3, 4, 5, 2, 7, 6, 1) ])
Dat2$Dow <- mapvalues(Dat2$Dow, 
                      from = levels(Dat2$Dow), 
                      to = c("aMon", "bTue", "cWed", "dThu", "eFri", "fSat", "gSun"))
ft <- bam(NetDemand ~ s(Instant, Dow, bs = "fs", k = 20), data = Dat2)
bb <- getViz(ft, nsim = 50)

pl <- list()

pl[[1]] <- plot(sm(v, 8), n = 200) + l_ciPoly() + l_ciLine(colour = 2) + l_fitLine() +
  labs(x = "Instant", y = expression(s[1] * "(Instant)"))
pl[[2]] <- qq(v, rep = 50, show.reps = T, level = 0.99, 
                  method = "tnormal", type = "tnormal", worm = F, CI = "normal",
                  a.replin = list(alpha = 0.2) )[[1]] + 
  labs(title = NULL, y = "Observed quantiles")

# Add glyphs to v(3)
glyFun <- function(.d){
  #print(length(.d$res))
  P <- stats::qqnorm(.d$z, plot.it = F)
  #P <- .qq$y - .qq$x
  P$Dq <- sort(P$x)
  P$D <- sort(P$y)
  alpha <- (1 - 0.95)/2
  p <- (1:length(P$Dq) - 0.5)/length(P$Dq)
  tmp <- qnorm(alpha) * sqrt(p * (1 - p)/length(P$Dq)) / dnorm(P$Dq)
  P$conf <- rbind(P$Dq + tmp, P$Dq - tmp)
  DI <- mgcViz:::.discretize.qq.gam(P = P, discrete = T, ngr = 100, 
                                    CI = TRUE, show.reps = FALSE)
  clr <- rep("black", length(DI$D))
  clr[DI$D < DI$conf$y[1:length(DI$Dq)] | 
        DI$D > rev(DI$conf$y[-(1:length(DI$Dq))])] <- "red"
  #.qq <- bkde(.d$res, gridsize = 100)
  return( data.frame("x" = DI$Dq, "y" = DI$D - DI$Dq, "color" = clr) )
}

pl[[3]] <- check2D(v, x1 = "wM_s95", x2 = "Instant", type = "tnormal") + 
  l_gridCheck2D(gridFun = sd, bw = c(1.5, 3)) + 
  l_glyphs2D(glyFun = glyFun, ggLay = "geom_point", 
             n = c(8, 8), mapping = aes(x=gx, y=gy, group = gid, colour = I(color)), 
             height=8.5, width = 4, size = 0.1) + 
  scale_fill_gradientn(colours = viridis(50, begin=0.4)) + 
  coord_cartesian(expand=F) + labs(x = expression(T^s)) # + 
# theme(legend.position="none")

# Glyph with densities
library(KernSmooth)
glyFun <- function(.d){
  #print(length(.d$z))
  .qq <- bkde(.d$z, gridsize = 100)
  return( as.data.frame(.qq) )
}

pl[[4]] <- check2D(v, x1 = "Posan", x2 = "Instant", type = "tnormal") + 
  l_gridCheck2D(gridFun = skewness, bw = c(0.02, 1)) + 
  l_glyphs2D(glyFun = glyFun, ggLay = "geom_path", 
             n = c(10, 10), mapping = aes(x=gx, y=gy, group = gid), 
             height=4.5, width = 0.2) + 
  scale_fill_gradientn(colours = viridis(50, begin=0.4)) + 
  coord_cartesian(expand=F) + labs(x = "toy") # + 
# theme(legend.position="none")

pl[[5]] <- plot(sm(bb, 1), trans = function(.x) .x + coef(ft)[1]) + 
  l_fitLine(size = 1) + theme_bw() + 
  scale_colour_discrete("Day",
                        labels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) + 
  theme(legend.position = "bottom", legend.key.width = unit(0.5, "cm")) + 
  guides(colour = guide_legend(nrow = 1)) + labs(y = "Load (MW)")

lay <- matrix(c(1,1,2,2,
                3,3,4,4,
                3,3,4,4), 3, 4, byrow = T)

pl1 <- pl[c(5, 1, 2)]

kk <- 1
cols <- rep("black", 3)
for(xx in c("a", "b", "c")){
  xpos <- c(-Inf,-Inf,Inf,Inf)
  ypos <- c(-Inf, Inf,-Inf,Inf)
  annotateText <- c("",xx,"","")
  hjustvar<-c(0,-0.8,0.99,1) 
  vjustvar<-c(-1,1.2,-1,1)
  
  pl1[[kk]] <-  pl1[[kk]] + geom_text(data=data.frame(xpos=xpos,ypos=ypos), inherit.aes = F,
                                      aes_(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=force(annotateText)), 
                                      color=cols[kk])
  kk <- kk+1
}

# Rendering Figure 3
grid.arrange(grobs=list(pl1[[1]]$ggObj, pl1[[2]]$ggObj, pl1[[3]]),
             ncol = 3)

pl2 <- pl[3:4]

kk <- 1
cols <- rep("black", 2)
for(xx in c("a", "b")){
  xpos <- c(-Inf,-Inf,Inf,Inf)
  ypos <- c(-Inf, Inf,-Inf,Inf)
  annotateText <- c("",xx,"","")
  hjustvar<-c(0,-0.8,0.99,1) 
  vjustvar<-c(-1,1.2,-1,1)
  
  pl2[[kk]] <-  pl2[[kk]] + geom_text(data=data.frame(xpos=xpos,ypos=ypos), inherit.aes = F,
                                      aes_(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=force(annotateText)), 
                                      color=cols[kk])
  kk <- kk+1
}

# Rendering Figure 4 
grid.arrange(grobs=lapply(pl2, "[[", "ggObj"), ncol = 2)


#############################
# Plotting smooth effects of shash model 
#############################
# Load fitted model and convert it to gamViz object
load("shash_final.RData")
set.seed(555)
v <- getViz(fitS)

##### Code for figure 5
pl <- list()
pl[[1]] <- plot(sm(v, 2), n = 100, too.far = 0) + 
  l_fitRaster(noiseup = T, pTrans = zto1(0.05, 2, 0.1)) + 
  l_fitContour() + 
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) + 
  labs(x = "T", y = "Instant", "title" = NULL) + coord_cartesian(expand=F) 

pl[[2]] <- plot(sm(v, 3), n = 100, too.far = 0) + 
  l_fitRaster(noiseup = T, pTrans = zto1(0.05, 2, 0.1)) + 
  l_fitContour() + 
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) + 
  labs(x = expression(T^s), y = "Instant", "title" = NULL) + coord_cartesian(expand=F) 

pl[[3]] <-  plot(sm(v, 4), n = 100, too.far = 0) + 
  l_fitRaster(noiseup = T, pTrans = zto1(0.05, 2, 0.1)) + 
  l_fitContour() + 
  theme(legend.position = "bottom", legend.key.width = unit(1, "cm")) + 
  labs(x = "toy", y = "Instant", "title" = NULL) + coord_cartesian(expand=F) 

grid.arrange(grobs = lapply(pl, "[[", "ggObj"), ncol = 3)

##### Code for figure 6 (interactive rgl version)
open3d()
mfrow3d(2, 2)
plotRGL(sm(v, 2), residuals = T, xlab = "T", ylab = "I", main = "f(T,I)")
next3d()
plotRGL(sm(v, 3), residuals = T, xlab = "Ts", ylab = "I", main = "f(Ts,I)")
next3d()
plotRGL(sm(v, 4), residuals = T, xlab = "toy", ylab = "I", main = "f(toy,I)")
next3d()
plotRGL(sm(v, 4), residuals = T, xlab = "toy", ylab = "I", main = "f(toy,I)")

rgl.snapshot(filename = "~/Desktop/smooth2.png", fmt = "png", top = TRUE )


