# Class 4: playing with the IRIS data set
# Factor Maps with Groups:
# Compute and display group means, confidence intervals and convex hulls
#
## clean start

rm(list = ls())
graphics.off()
# Get there with an Rproject
# if not an Rproject set the directory here
# e.g.,
# setwd(~/Box Sync/RM3-CurrentYear/R4RM3/r-Class04/'')

library(ExPosition)
library(InPosition)
# Please install the updated version of PTCA4CATA
devtools::install_github('HerveAbdi/PTCA4CATA') 
devtools::install_github("HerveAbdi/data4PCCAR")
library(PTCA4CATA)
library(corrplot)
library(ggplot2)
install.packages('gplots') # comment this if you already installed gplots

#
#here we load the classic iris data set
data(iris)
# The iris data set ----
#_____________________________________________________________________
# Have a look at the iris data set
summary(iris)
#_____________________________________________________________________
# separate the measurements from the factor (design)
# resIris ----
mesIris <- iris[,1:4] # measures: SL SW PL PW
grIris <- iris[,5] # Three types pf irises
#_____________________________________________________________________
# PCA ----
resPCA <- epPCA(DATA = mesIris,
                   scale = 'SS1', # Make to use 'SS1' rather than TRUE
                   DESIGN = grIris,
                   graphs =  FALSE # TRUE first pass only
                    )
#_____________________________________________________________________
# corrplot ----
# First a correlation plot
cor.iris <- cor(mesIris)
corrplot.mixed(round(cor.iris, 2), lower = 'number',
               upper = 'ellipse')
a001.corrMap <- recordPlot()
#_____________________________________________________________________
# Scree ----
PlotScree(ev = resPCA$ExPosition.Data$eigs)
a001a.screePlot <- recordPlot()
#_____________________________________________________________________
# I-set map ----
# a graph of the observations
iris.Imap <- PTCA4CATA::createFactorMap(
                   resPCA$ExPosition.Data$fi,
                   col.points = resPCA$Plotting.Data$fi.col,
                   display.labels = FALSE,
                   alpha.points = .5
                   )

#_____________________________________________________________________
# make labels ----
label4Map <- createxyLabels.gen(1,2,
                   lambda =resPCA$ExPosition.Data$eigs,
                   tau = resPCA$ExPosition.Data$t)
#_____________________________________________________________________
a002.Map.I <- iris.Imap$zeMap + label4Map
print(a002.Map.I)
#_____________________________________________________________________
# J-set Map ----
#_____________________________________________________________________
# color 4 J
# create a color scheme for the variables
# Width is darker than length: Sepal is orange. Petal is red
col4Var <- c('orange','orange4','red','red4')
#_____________________________________________________________________
# A graph for the J set ----
baseMap.j <- PTCA4CATA::createFactorMap(resPCA$ExPosition.Data$fj,
                                        col.points   = col4Var,
                                        alpha.points =  .3,
                                        col.labels   = col4Var)
#_____________________________________________________________________
# arrows
zeArrows <- addArrows(resPCA$ExPosition.Data$fj, color = col4Var)
# A graph for the J-set
b000.aggMap.j <- baseMap.j$zeMap_background + # background layer
  baseMap.j$zeMap_dots + baseMap.j$zeMap_text +  # dots & labels
  label4Map + zeArrows
# We print this Map with the following code
dev.new()
print(b000.aggMap.j)
#_____________________________________________________________________
# Contribution Plots ----
# get the Contributions and make a plot.
#_____________________________________________________________________
# Here we look only at the (signed) contributions for the variables
# compute teh signed contributions
signed.ctrJ <- resPCA$ExPosition.Data$cj * sign(resPCA$ExPosition.Data$fj)
# Contribution # 1 & 2
#_____________________________________________________________________
#
b003.ctrJ.s.1 <- PrettyBarPlot2(signed.ctrJ[,1],
                         threshold = 1 / NROW(signed.ctrJ),
                         font.size = 5,
                         color4bar = gplots::col2hex(col4Var), # we need hex code
                         main = 'PCA on the Iris Set: Variable Contributions (Signed)',
                         ylab = 'Contributions',
                         ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
)
print(b003.ctrJ.s.1)
# 
b004.ctrJ.s.2 <- PrettyBarPlot2(signed.ctrJ[,2],
                           threshold = 1 / NROW(signed.ctrJ),
                           font.size = 5,
                           color4bar = gplots::col2hex(col4Var), # we need hex code
                           main = 'PCA on the Iris Set: Variable Contributions (Signed)',
                           ylab = 'Contributions',
                           ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
)
print(b004.ctrJ.s.2)
#_____________________________________________________________________
# What about a rotation -----
#
dim.row <- 2
res.Varimax <- varimax(resPCA$ExPosition.Data$fj[,1:dim.row],
                       normalize = TRUE, eps = 1e-6)
# The rotation matrix is in res.Varimax$rotmat

rotated.J <- resPCA$ExPosition.Data$fj[,1:2] %*% res.Varimax$rotmat
rotated.I <- resPCA$ExPosition.Data$fi[,1:2] %*% res.Varimax$rotmat

baseMap.j.rot <- PTCA4CATA::createFactorMap(rotated.J,
                                            col.points   = col4Var,
                                            alpha.points =  .3,
                                            col.labels   = col4Var,
                                            title = 'Loadings Post Varimax')
# arrows
zeArrows.rot <- addArrows(rotated.J, color = col4Var)
# A graph for the J-set
c001.aggMap.J.rot <- baseMap.j.rot$zeMap_background + # background layer
                     baseMap.j.rot$zeMap_dots +
                     baseMap.j.rot$zeMap_text +  # dots & labels
                     zeArrows.rot
dev.new()
print(c001.aggMap.J.rot)

# Plot the Rotated observations too
# a graph of the observations
iris.Imap.rot <- PTCA4CATA::createFactorMap(
  rotated.I,
  col.points = resPCA$Plotting.Data$fi.col,
  display.labels = FALSE,
  alpha.points = .5,
  title = 'Factor Scores Post Varimax'
)
c002.Map.I.rot <- iris.Imap.rot$zeMap
dev.new()
print(c002.Map.I.rot)
#_____________________________________________________________________
# Inferences
# Use the factor from resPCA. The group for the irises is in grIris
#_____________________________________________________________________
# Mean Map
#  create the map for the means
#  get the means by groups

IrisMeans <- PTCA4CATA::getMeans(resPCA$ExPosition.Data$fi, grIris)
# a vector of color for the means
col4Iris <- resPCA$Plotting.Data$fi.col
col4Means <- unique(col4Iris)
# the map
MapGroup    <- PTCA4CATA::createFactorMap(IrisMeans,
                            # use the constraint from the main map
                            constraints = iris.Imap$constraints,
                            col.points = col4Means,
                            cex = 7,  # size of the dot (bigger)
                            col.labels = col4Means,
                            text.cex = 6)
# The map with observations and group means
a003.Map.I.withMeans <- a002.Map.I +
                         MapGroup$zeMap_dots + MapGroup$zeMap_text
print(a003.Map.I.withMeans)
#_____________________________________________________________________
# Confidence Intervals ----
# Bootstrap for CI:
BootCube <- PTCA4CATA::Boot4Mean(resPCA$ExPosition.Data$fi, 
                                 design = grIris,
                                 niter = 100,
                                 suppressProgressBar = TRUE)
#_____________________________________________________________________
# Create the ellipses
#_____________________________________________________________________
# Create Confidence Interval Plots
# use function MakeCIEllipses from package PTCA4CATA
GraphElli <- PTCA4CATA::MakeCIEllipses(BootCube$BootCube[,1:2,],
                            names.of.factors = c("Dimension 1","Dimension 2"),
                            col = col4Means,
                            p.level = .95
)
#_____________________________________________________________________
# create the I-map with Observations, means and confidence intervals
#
a004.Map.I.withCI <-  a002.Map.I + MapGroup$zeMap_text +  GraphElli
#_____________________________________________________________________
# plot it!
dev.new()
print(a004.Map.I.withCI)
#_____________________________________________________________________
# Tolerance Intervals ----
# use function MakeToleranceIntervals from package PTCA4CATA
GraphTI.Hull <- PTCA4CATA::MakeToleranceIntervals(resPCA$ExPosition.Data$fi,
                            design = as.factor(grIris),
                            # line below is needed
                            names.of.factors =  c("Dim1","Dim2"), # needed 
                            col = col4Means,
                            line.size = .50, 
                            line.type = 3,
                            alpha.ellipse = .2,
                            alpha.line    = .4,
                            p.level       = .75)
#_____________________________________________________________________
# Create the map:
a005.Map.I.withTIHull <-a002.Map.I  +
                            GraphTI.Hull + MapGroup$zeMap_dots +
                           MapGroup$zeMap_text + MapGroup$zeMap_dots
#_____________________________________________________________________
# plot it
dev.new()
print(a005.Map.I.withTIHull)
#_____________________________________________________________________
# End of graphs ----
#_____________________________________________________________________
# Save as Powerpoint ----
#_____________________________________________________________________
#  
listSaved <- saveGraph2pptx(
file2Save.pptx = 'PCA4TheIrisDataSet.pptx', 
title = "The Iris data set: PCA, confidence and tolerance intervals", 
addGraphNames = TRUE)

