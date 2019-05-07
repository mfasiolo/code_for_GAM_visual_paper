########################################################################
####### Code for generating plot, given fitted models
########################################################################

library(dplyr)
library(mgcViz)
library(gridExtra)
library(ggplot2)
library(viridis)
library(e1071)

library(devtools)
install_github("mfasiolo/mgcFam")

load("UKData.RData")
data_UK_load <- Data # More meaningful name

##############################
## GAUSSIAN GAM model
##############################
# Load fitted model and convert it to gamViz object
load("gausUK_lag.RData")
set.seed(555)
fit_gaus <- getViz(fit, nsim = 50)

themeSize <- theme(axis.text=element_text(size=14),
                   axis.title=element_text(size=18), 
                   legend.title=element_text(size=18), 
                   legend.text=element_text(size=14))

##### Code for reproducing Figure 1
figure_1 <- list()
suppressMessages(
figure_1[[1]] <- plot(sm(fit_gaus, 1)) + 
                 l_dens(type = "cond", n = c(50, 50), tol = -1, trans = function(.x) .x^(0.33)) +
                 l_fitLine() +
                 labs(x = "t", y = expression(f[1] * "(t) (in 100s)")) + #ylim(-7500, 7500) +
                 scale_fill_gradientn(colours = viridis(50, begin = 0.2), 
                                      na.value = "white", 
                                      name="p(r|t)") + 
                 theme(legend.position="bottom", legend.key.width = unit(1, "cm")) +
                 coord_cartesian(expand=F) + themeSize
                 
)

figure_1[[4]] <- qq(fit_gaus, rep = 50, showReps = T, level = 0.99, method = "tnormal", CI = "normal", 
                 type = "tnormal", worm = F, a.replin = list(alpha = 0.2))[[1]] + 
                 labs(title = NULL, y = "Observed quantiles") + themeSize

suppressMessages(
figure_1[[2]] <- check1D(fit_gaus, x = "wM_s95", maxpo = 1e3, type = "tnormal") + 
                 l_densCheck(tol = -1) + 
                 xlim(0, 25) +
                 scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                                      midpoint = 0, limits = c(-0.7, 0.77)) +
                 guides(fill=FALSE) +
                 labs('x' = expression(T^s), 'y' = "Quantile residuals") + 
                 l_rug(mapping = aes(x = x)) + coord_cartesian(expand=F) + themeSize)

figure_1[[3]] <- check1D(fit_gaus, x = "wM_s95", maxpo = 1e3, type = "scaled.pearson") + 
                 l_gridCheck1D(gridFun = function(.x) sd(.x), n = 30, stand = "none", showReps = F) +
                 theme_bw() +
                 # theme(panel.grid.minor = element_line(size = 0.1, colour = "grey"), 
                 # panel.grid.major = element_line(size = 0.1, colour = "grey")) + 
                 guides(fill=FALSE) + 
                 labs('x' = expression(T^s), "y" = "sd(r)") + themeSize
suppressMessages(
figure_1[[5]] <- check1D(fit_gaus, x = "Posan", maxpo = 1e3, type = "tnormal") + 
                 l_densCheck(tol = -1) + 
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       midpoint = 0, limits = c(-0.7, 0.77), name = expression(delta (p, p[m]) )) +
                 #guides(fill=FALSE) + 
                 theme(legend.position="bottom", legend.key.width = unit(1, "cm")) +
                 labs('x' = "toy", 'y' = "Quantile residuals") + 
                 l_rug(mapping = aes(x = x)) + coord_cartesian(expand=F) + themeSize)

figure_1[[6]] <-  check1D(fit_gaus, x = "Posan", maxpo = 1e3, type = "scaled.pearson") + 
                  l_gridCheck1D(gridFun = function(.x) sd(.x), n = 30, stand = "none", showReps = F) +
                  theme_bw() +
                  # theme(panel.grid.minor = element_line(size = 0.1, colour = "grey"), 
                  #       panel.grid.major = element_line(size = 0.1, colour = "grey")) + 
                  guides(fill=FALSE) + 
                  labs('x' = "toy", "y" = "sd(r)") + themeSize



# Extract ggObject from each element of figure_1
for(ii in c(1, 2, 3, 5, 6)){
  figure_1[[ii]] <- figure_1[[ii]]$ggObj
}

# Changing axes marks in the first plots (for aesthetic reasons)
ticks_x <- quantile(data_UK_load$Tendance, c(0.15, 0.5, 0.85))
labels_x <- round(nrow(data_UK_load) * c(0.15, 0.5, 0.85))
responses_y <- c(figure_1[[1]]$layers[[1]]$data$y)
ticks_y <- seq(min(responses_y), max(responses_y), length.out = 4)
labels_y <- round(ticks_y / 100)
figure_1[[1]] <- figure_1[[1]] + # theme(legend.position="none") + 
  scale_x_continuous(breaks = ticks_x, labels = labels_x) + 
  scale_y_continuous(breaks = ticks_y, labels = labels_y)

# Annotating each plot with a), b), ...
kk <- 1
cols <- c("white", rep("black", 5))
for(xx in c("a", "c", "e", "b", "d", "f")){
  x_position <- c(-Inf,-Inf,Inf,Inf)
  y_position <- c(-Inf, Inf,-Inf,Inf)
  annotateText <- c("", xx, "", "")
  horiz_justif<-c(0,-0.8,0.99,1) 
  vertic_justif<-c(-1,1.2,-1,1)
  
  figure_1[[kk]] <- figure_1[[kk]] + 
                    geom_text(data=data.frame(x_position=x_position,y_position=y_position), inherit.aes = F,
                              aes_(x=x_position,y=y_position,hjust=horiz_justif,vjust=vertic_justif,
                                   label=force(annotateText)), 
                              color=cols[kk], size = 8)
  kk <- kk+1
}

fig_layout <- matrix(c(1, 1, 2, 2, 3, 3, 
                       1, 1, 2, 2, 3, 3, 
                       1, 1, 2, 2, 3, 3,
                       1, 1, 2, 2, 3, 3,
                       1, 1, 2, 2, 3, 3,
                       1, 1, 2, 2, 3, 3,
                       1, 1, 2, 2, 3, 3,
                       1, 1, 5, 5, 3, 3,
                       1, 1, 5, 5, 6, 6,
                       4, 4, 5, 5, 6, 6,
                       4, 4, 5, 5, 6, 6,
                       4, 4, 5, 5, 6, 6,
                       4, 4, 5, 5, 6, 6,
                       4, 4, 5, 5, 6, 6,
                       4, 4, 5, 5, 6, 6,
                       4, 4, 5, 5, 6, 6), 16, 6, byrow = T)

grid.arrange(grobs=figure_1, layout_matrix = fig_layout)

# Free some memory
rm(fit_gaus); rm(fit)
gc()





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
fit_gaus_loc_scale <- getViz(fitG, nsim = 50)

##### Code for Figure 2
figure_2 <- list()
figure_2[[1]] <- plot(sm(fit_gaus_loc_scale, 5), n = 200) + 
                 l_ciPoly() + l_fitLine() + 
                 l_ciLine(colour = 2, linetype = 2) + ylim(-0.9, 0.45) + 
                 labs(x = expression(T^s), y = expression(v[1] * "(" * T^s * ")")) + themeSize

figure_2[[2]] <- plot(sm(fit_gaus_loc_scale, 6), n = 200) + l_ciPoly() + l_fitLine() + 
                 l_ciLine(colour = 2, linetype = 2) +  
                 labs(x = "I", y = expression(v[2] * "(" * I * ")")) + themeSize

figure_2[[3]] <- plot(sm(fit_gaus_loc_scale, 7), n = 200) + l_ciPoly() + l_fitLine() + 
                 l_ciLine(colour = 2, linetype = 2) + ylim(-0.3, 1.3) +
                 labs(x = "toy", y = expression(v[3] * "(toy)")) + themeSize

# Create function that calculated (signed) sqrt of sample skewness, 
# to be fed to l_gridCheck1D and l_gridCheck2D
library( "e1071" )
fn <- function(.x, .sdr){
  .a <- skewness(.x) 
  .si <- sign(.a)
  return( .si*sqrt(abs(.a)) )
}

set.seed(784)
suppressMessages(
figure_2[[4]] <- check2D(fit_gaus_loc_scale, x1 = "wM_s95", x2 = "Instant") + 
                 l_gridCheck2D(bw = c(1.5, 3), gridFun = sd) + 
                 scale_fill_gradientn(colours = viridis(50,begin=0), na.value="white") + 
                 coord_cartesian(expand=F) + labs(x = expression(T^s), y = "I") + themeSize)

suppressMessages(
figure_2[[5]] <- check2D(fit_gaus_loc_scale, x1 = "Posan", x2 = "Instant") + 
                 l_gridCheck2D(bw = c(0.02, 1), gridFun = skewness) + 
                 scale_fill_gradientn(colours = viridis(50,begin=0), na.value="white") + 
                 coord_cartesian(expand=F) + labs(x = expression("toy"), y = "I") + themeSize)

figure_2[[6]] <- check1D(fit_gaus_loc_scale, x = "wM_s95", type = "deviance") + 
                 l_gridCheck1D(n = 30, gridFun = sd, stand = "none", showReps = F) +
                 theme_bw() +  guides(fill=FALSE) + 
                 labs('x' = expression(T^s), "y" = "sd(r)") + themeSize

figure_2[[7]] <- check1D(fit_gaus_loc_scale, x = "Instant", type = "deviance") + 
                 l_gridCheck1D(n = 30, gridFun = skewness, stand = "none", showReps = F) +
                 theme_bw() +  guides(fill=FALSE) + 
                 labs('x' = "I", "y" = "skew(r)") + themeSize

# Annotating each plot with a), b), ...
kk <- 1
cols <- c(rep("black", 3), c("black", "black"), rep("black", 2))
for(xx in c("a", "b", "c", "d", "e", "f", "g")){
  x_position <- c(-Inf,-Inf,Inf,Inf)
  y_position <- c(-Inf, Inf,-Inf,Inf)
  annotateText <- c("",xx,"","")
  horiz_justif<-c(0,-0.8,0.99,1) 
  vertic_justif<-c(-1,1.2,-1,1)
  
  figure_2[[kk]] <-  figure_2[[kk]] + geom_text(data=data.frame(x_position=x_position,y_position=y_position), inherit.aes = F,
                                    aes_(x=x_position,y=y_position,hjust=horiz_justif,vjust=vertic_justif,label=force(annotateText)), 
                                    color=cols[kk], size = 8)
  kk <- kk+1
}

fig_layout <- matrix(c(1,1,1,1,4,4,4,4,4,6,6,6,6,
                       1,1,1,1,4,4,4,4,4,6,6,6,6,
                       2,2,2,2,4,4,4,4,4,6,6,6,6,
                       2,2,2,2,5,5,5,5,5,7,7,7,7,
                       3,3,3,3,5,5,5,5,5,7,7,7,7,
                       3,3,3,3,5,5,5,5,5,7,7,7,7), 6, 13, byrow = T)

grid.arrange(grobs=lapply(figure_2, "[[", "ggObj"), layout_matrix = fig_layout)

# Free some memory
rm(fit_gaus_loc_scale); rm(fitG)
gc()







##############################
## Shash GAM model
##############################
# Load fitted model
load("shash_final.RData")
set.seed(555)
fit_shash <- getViz(fitS, nsim = 50)

library(plyr)

##### Code for Figures 3 and 4

# Getting daily consumption profile curves
# First we need to renames day of the week from 1,2,3... to Mon, Tues...
dat_2 <- data_UK_load
dat_2$Dow <- factor(dat_2$Dow, levels(data_UK_load$Dow)[ c(3, 4, 5, 2, 7, 6, 1) ])
dat_2$Dow <- mapvalues(dat_2$Dow, 
                      from = levels(dat_2$Dow), 
                      to = c("aMon", "bTue", "cWed", "dThu", "eFri", "fSat", "gSun"))
fit_profile <- bam(NetDemand ~ s(Instant, Dow, bs = "fs", k = 20), data = dat_2)
fit_profile <- getViz(fit_profile, nsim = 50)

figure_3_and_4 <- list()
figure_3_and_4[[1]] <- plot(sm(fit_shash, 8), n = 200) + l_ciPoly() + 
                       l_ciLine(colour = 2) + l_fitLine() +
                       labs(x = "I", y = expression(s[1] * "(I)")) + themeSize

figure_3_and_4[[2]] <- qq(fit_shash, rep = 50, showReps = T, level = 0.99, 
                          method = "tnormal", type = "tnormal", worm = F, CI = "normal",
                          a.replin = list(alpha = 0.2) )[[1]] + 
                          labs(title = NULL, y = "Observed quantiles") + themeSize

# Function that produces the data for the QQ-plot glyphs in Figure 4
# See ?l_glyphs2D to learn what data.frame ".d" contains and type of data.frame
# must be returned by QQ_glyphs().
QQ_glyphs <- function(.d){
  P <- stats::qqnorm(.d$z, plot.it = F)
  P$Dq <- sort(P$x) # Theoretical quantiles
  P$D <- sort(P$y)  # Observed quantile
  
  # Significance level
  alpha <- (1 - 0.95)/2
  p <- (1:length(P$Dq) - 0.5)/length(P$Dq)
  
  # CI for quantile residuals as in Buuren and Fredriks (2001)
  tmp <- qnorm(alpha) * sqrt(p * (1 - p)/length(P$Dq)) / dnorm(P$Dq)
  P$conf <- rbind(P$Dq + tmp, P$Dq - tmp)
  
  # Discretize QQ-plot using mgcViz internal methods (not advisable in general)
  DI <- mgcViz:::.discretize.qq.gam(P = P, discrete = T, ngr = 100, CI = TRUE, showReps = FALSE)
  
  # Setting colour to red (black) if observed quantiles DI$D fall outside CIs
  clr <- rep("grey", length(DI$D))
  clr[DI$D < DI$conf$y[1:length(DI$Dq)] | DI$D > rev(DI$conf$y[-(1:length(DI$Dq))])] <- "black"

  return( data.frame("x" = DI$Dq, "y" = DI$D - DI$Dq, "color" = clr) )
}



# Add QQ-plot glyphs to left plot in Figure 4
suppressMessages(
figure_3_and_4[[3]] <- check2D(fit_shash, x1 = "wM_s95", x2 = "Instant", type = "tnormal") + 
  l_gridCheck2D(gridFun = sd, bw = c(1.5, 3)) + 
  l_glyphs2D(glyFun = QQ_glyphs, ggLay = "geom_point", 
             n = c(8, 8), mapping = aes(x=gx, y=gy, group = gid, colour = I(color)), 
             height=8.5, width = 4, size = 0.5) + 
  scale_fill_gradientn(colours = viridis(50, begin=0.2, end=0.95)) + 
  coord_cartesian(expand=F) + labs(x = expression(T^s), y = "I") + themeSize) 

# Function that produces the data for the kernel density estimates glyphs in Figure 4
# See ?l_glyphs2D to learn what data.frame ".d" contains and type of data.frame
# must be returned by QQ_glyphs().
library(KernSmooth)
kernel_dens_glyph <- function(.d){
  .qq <- bkde(.d$z, gridsize = 100)
  return( as.data.frame(.qq) )
}

# Add QQ-plot glyphs to right plot in Figure 4
suppressMessages(
figure_3_and_4[[4]] <- check2D(fit_shash, x1 = "Posan", x2 = "Instant", type = "tnormal") + 
                       l_gridCheck2D(gridFun = skewness, bw = c(0.02, 1)) + 
                       l_glyphs2D(glyFun = kernel_dens_glyph, ggLay = "geom_path", 
                                  n = c(10, 10), mapping = aes(x=gx, y=gy, group = gid), 
                                  height=4.5, width = 0.2, linemitre = 30) + 
                       scale_fill_gradientn(colours = viridis(50, begin=0.2, end=0.95)) + 
                       coord_cartesian(expand=F) + labs(x = "toy", y = "I") + themeSize)

# Plot daily consumption profiles for Figure 3
savLines <- plot(sm(fit_profile, 1), trans = function(.x) .x + coef(fit_profile)[1], n = 30)$data$fit
savLines$id <- factor(substring(as.character(savLines$id), 2), 
                      levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
figure_3_and_4[[5]] <- ggplot(savLines, aes(x, ty, colour = id, shape=id)) +
  geom_line() +  
  geom_point(size = 2) +
  scale_colour_discrete("") +
  scale_shape_manual("", values=1:7) +
  theme_bw() + 
  theme(legend.position = "right") + #, legend.key.width = unit(0.5, "cm")) +
  labs(y = "Load (MW)", x = "I") + themeSize

fig_layout <- matrix(c(1,1,2,2,
                       3,3,4,4,
                       3,3,4,4), 3, 4, byrow = T)

figure_3 <- figure_3_and_4[c(5, 1, 2)]

# Annotating each plot with a), b), ...
kk <- 1
cols <- rep("black", 3)
for(xx in c("a", "b", "c")){
  x_position <- c(-Inf,-Inf,Inf,Inf)
  y_position <- c(-Inf, Inf,-Inf,Inf)
  annotateText <- c("",xx,"","")
  horiz_justif<-c(0,-0.8,0.99,1) 
  vertic_justif<-c(-1,1.2,-1,1)
  
  figure_3[[kk]] <-  figure_3[[kk]] + 
                     geom_text(data=data.frame(x_position=x_position,y_position=y_position), inherit.aes = F,
                               aes_(x=x_position,y=y_position,hjust=horiz_justif,vjust=vertic_justif,label=force(annotateText)), 
                               color=cols[kk], size = 8)
  kk <- kk+1
}

# Rendering Figure 3
grid.arrange(grobs=list(figure_3[[1]], figure_3[[2]]$ggObj, figure_3[[3]]),
             layout_matrix = matrix(c(1,1,1,1,2,2,2,3,3,3,
                                      1,1,1,1,2,2,2,3,3,3), 2, 10, byrow = T))

figure_4 <- figure_3_and_4[3:4]

# Annotating each plot with a), b), ...
kk <- 1
cols <- rep("black", 2)
for(xx in c("a", "b")){
  x_position <- c(-Inf,-Inf,Inf,Inf)
  y_position <- c(-Inf, Inf,-Inf,Inf)
  annotateText <- c("",xx,"","")
  horiz_justif<-c(0,-0.8,0.99,1) 
  vertic_justif<-c(-1,1.2,-1,1)
  
  figure_4[[kk]] <-  figure_4[[kk]] +
                     geom_text(data=data.frame(x_position=x_position,y_position=y_position), inherit.aes = F,
                               aes_(x=x_position,y=y_position,hjust=horiz_justif,vjust=vertic_justif,label=force(annotateText)), 
                               color=cols[kk], size = 8)
  kk <- kk+1
}

# Rendering Figure 4 
grid.arrange(grobs=lapply(figure_4, "[[", "ggObj"), ncol = 2)

# Free some memory
rm(fit_shash); rm(fitS)
gc()



#############################
# Plotting smooth effects of shash model 
#############################
# Load fitted model and convert it to gamViz object
load("shash_final.RData")
set.seed(555)
fit_shash <- getViz(fitS)

##### Code for figure 5
figure_5 <- list()
figure_5[[1]] <- plot(sm(fit_shash, 2), n = 100, too.far = 0) + 
  l_fitRaster(noiseup = T, pTrans = zto1(0.05, 2, 0.1)) + 
  l_fitContour() + 
  theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm")) + 
  labs(x = "T", y = "I", "title" = NULL) + coord_cartesian(expand=F) + themeSize

figure_5[[2]] <- plot(sm(fit_shash, 3), n = 100, too.far = 0) + 
  l_fitRaster(noiseup = T, pTrans = zto1(0.05, 2, 0.1)) + 
  l_fitContour() + 
  theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm")) + 
  labs(x = expression(T^s), y = "I", "title" = NULL) + coord_cartesian(expand=F) + themeSize

figure_5[[3]] <-  plot(sm(fit_shash, 4), n = 100, too.far = 0) + 
  l_fitRaster(noiseup = T, pTrans = zto1(0.05, 2, 0.1)) + 
  l_fitContour() + 
  theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm")) + 
  labs(x = "toy", y = "I", "title" = NULL) + coord_cartesian(expand=F) + themeSize

# Rendering Figure 5
grid.arrange(grobs = lapply(figure_5, "[[", "ggObj"), ncol = 3)

##### Code for figure 6 (interactive rgl version)
open3d()
mfrow3d(2, 2)
plotRGL(sm(fit_shash, 2), residuals = T, xlab = "T", ylab = "I", main = "f(T,I)")
next3d()
plotRGL(sm(fit_shash, 3), residuals = T, xlab = "Ts", ylab = "I", main = "f(Ts,I)")
next3d()
plotRGL(sm(fit_shash, 4), residuals = T, xlab = "toy", ylab = "I", main = "f(toy,I)")
next3d()
plotRGL(sm(fit_shash, 4), residuals = T, xlab = "toy", ylab = "I", main = "f(toy,I)")

rgl.snapshot(filename = "~/Desktop/smooth2.png", fmt = "png", top = TRUE )


