rm(list = ls())
graphics.off()
library(ExPosition)
library(InPosition)
library(PTCA4CATA)
library(corrplot)
library(ggplot2)
library(data4PCCAR)

my_data <- read.csv("IBM-HR-Emplyee-NoAttrition.csv")
rownames(my_data) <- my_data$Subj
my_data1 <- my_data[sapply(my_data, function(x) !is.factor(x))]
my_data1 <- subset(my_data1, select=-Subj)

datar <- my_data1[,2:23]
datae <- my_data[,5]
datag <- my_data[,7]
#
#_____________________________________________________________________
# Scree + Inference----
PlotScree(ev = resPCA$ExPosition.Data$eigs, 
          p.ev =  resPCA.inf$Inference.Data$components$p.vals,
          title = 'IBM-No-Attririon data Set. Eigenvalues Inference',
          plotKaiser = TRUE
)
a001a.screePlot.Inf <- recordPlot()

#_____________________________________________________________________
# PCA ----
resPCA <- epPCA(DATA = datar,
                scale = 'SS1', # Make to use 'SS1' rather than TRUE
                DESIGN = datae,
                graphs =  FALSE # TRUE first pass only
)
resPCA1 <- epPCA(DATA = datar,
                scale = 'SS1', # Make to use 'SS1' rather than TRUE
                DESIGN = datag,
                graphs =  FALSE # TRUE first pass only
)




#_____________________________________________________________________
# Varimax here ----
testVari    <- data4PCCAR::epVari(resPCA)
#_____________________________________________________________________
# Inference battery ----
resPCA.inf <- InPosition::epPCA.inference.battery(DATA = datar,
                                                  scale = 'SS1', # Make sure to use 'SS1' rather than TRUE
                                                  DESIGN = datae,
                                                  graphs =  FALSE # TRUE first pass only
)
resPCA.inf1 <- InPosition::epPCA.inference.battery(DATA = datar,
                                                  scale = 'SS1', # Make sure to use 'SS1' rather than TRUE
                                                  DESIGN = datag,
                                                  graphs =  FALSE # TRUE first pass only
)
#_____________________________________________________________________
# Group Analysis ----
# Bootstrap ----
# Confidence Intervals ----
# Bootstrap for CI:
BootCube.Gr <- PTCA4CATA::Boot4Mean(resPCA$ExPosition.Data$fi, 
                                    design = datae,
                                    niter = 100,
                                    suppressProgressBar = TRUE)

BootCube.Gr1 <- PTCA4CATA::Boot4Mean(resPCA1$ExPosition.Data$fi, 
                                    design = datag,
                                    niter = 100,
                                    suppressProgressBar = TRUE)
#_____________________________________________________________________
# Bootstrap ratios ----
bootRatios.Gr <- boot.ratio.test(BootCube.Gr$BootCube)
bootRatios.Gr1 <- boot.ratio.test(BootCube.Gr1$BootCube)
#*********************************************************************
# eigenvalues: MonteCarlo Approach ----
# 
random.eigen <- data4PCCAR::monteCarlo.eigen(X = datar, nIter = 100)
#
# eigenvalues: Bootstrap approach
#
bootstrap.eigen <- data4PCCAR::boot.eigen(datar, nIter = 100)
# End of computation ---- 
#_____________________________________________________________________
# Graphics star here
#_____________________________________________________________________
# corrplot ----
# First a correlation plot
cor.my_data <- cor(datar)
corrplot(cor.my_data, method = "ellipse")
a001.corrMap <- recordPlot()
#_____________________________________________________________________

#_____________________________________________________________________
# I-set map ----
# a graph of the observations
my_data1.Imap <- PTCA4CATA::createFactorMap(
  resPCA$ExPosition.Data$fi,
  col.points = resPCA$Plotting.Data$fi.col,
  display.labels = FALSE,
  alpha.points = 0.3
)

my_data1.Imap1 <- PTCA4CATA::createFactorMap(
  resPCA1$ExPosition.Data$fi,
  col.points = resPCA1$Plotting.Data$fi.col,
  display.labels = FALSE,
  alpha.points = 0.3
)
#_____________________________________________________________________
# make labels ----
label4Map <- createxyLabels.gen(1,2,
                                lambda =resPCA$ExPosition.Data$eigs,
                                tau = resPCA$ExPosition.Data$t)

label4Map1 <- createxyLabels.gen(1,2,
                                lambda =resPCA1$ExPosition.Data$eigs,
                                tau = resPCA1$ExPosition.Data$t)
#_____________________________________________________________________
a002.Map.I <- my_data1.Imap$zeMap + label4Map
dev.new()
print(a002.Map.I)

a002.Map.I1 <- my_data1.Imap1$zeMap + label4Map1
dev.new()
print(a002.Map.I1)
#_____________________________________________________________________
# J-set Map ----
#_____________________________________________________________________
# color 4 J
# create a color scheme for the variables
col4J.ibm <- prettyGraphsColorSelection(NCOL(datar))
#_____________________________________________________________________
# A graph for the J set ----
baseMap.j <- PTCA4CATA::createFactorMap(resPCA$ExPosition.Data$fj,col.points = col4J.ibm,col.labels = col4J.ibm,
                                        alpha.points =  .3)
#_____________________________________________________________________
# arrows
zeArrows <- addArrows(resPCA$ExPosition.Data$fj , col = col4J.ibm)
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
# compute the signed contributions
signed.ctrJ <- resPCA$ExPosition.Data$cj * sign(resPCA$ExPosition.Data$fj)

# Colors for the variables 
#
#_____________________________________________________________________
# Contribution # 1 & 2 & 3
#_____________________________________________________________________
#
b003.ctrJ.s.1 <- PrettyBarPlot2(signed.ctrJ[,1],
                                threshold = 1 / NROW(signed.ctrJ),
                                font.size = 5,
                                color4bar = gplots::col2hex(col4J.ibm), # we need hex code
                                main = 'PCA on the IBM-No-Attririon data Set: Variable Contributions (Signed)',
                                ylab = 'Contributions',
                                ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
)
print(b003.ctrJ.s.1)
b001a.contribution1 <- recordPlot()
# 
b004.ctrJ.s.2 <- PrettyBarPlot2(signed.ctrJ[,2],
                                threshold = 1 / NROW(signed.ctrJ),
                                font.size = 5,
                                color4bar = gplots::col2hex(col4J.ibm), # we need hex code
                                main = 'PCA on the IBM-No-Attririon dataSet: Variable Contributions (Signed)',
                                ylab = 'Contributions',
                                ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
)
print(b004.ctrJ.s.2)
b001b.contribution2 <- recordPlot()
b004.ctrJ.s.3 <- PrettyBarPlot2(signed.ctrJ[,3],
                                threshold = 1 / NROW(signed.ctrJ),
                                font.size = 5,
                                color4bar = gplots::col2hex(col4J.ibm), # we need hex code
                                main = 'PCA on the IBM-No-Attririon dataSet: Variable Contributions (Signed)',
                                ylab = 'Contributions',
                                ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
)
print(b004.ctrJ.s.3)
b001b.contribution3 <- recordPlot()
#_____________________________________________________________________

#_____________________________________________________________________
# bootstrap ratios ----
#
BR <- resPCA.inf$Inference.Data$fj.boots$tests$boot.ratios
laDim = 1
ba001.BR1 <- PrettyBarPlot2(BR[,laDim],
                            threshold = 2,
                            font.size = 5,color4bar = gplots::col2hex(col4J.ibm),
                            main = paste0(
                              'PCA on the IBM-No-Attririon data Set: Bootstrap ratio ',laDim),
                            ylab = 'Bootstrap ratios',
                            ylim = c(1.2*min(BR[,laDim]), 1.2*max(BR[,laDim]))
)
print(ba001.BR1)
b005a.Bootstrap_ratios <- recordPlot()
#
laDim = 2
ba002.BR2 <- PrettyBarPlot2(BR[,laDim],
                            threshold = 2,
                            font.size = 5,color4bar = gplots::col2hex(col4J.ibm),
                            main = paste0(
                              'PCA on the IBM-No-Attririon data Set: Bootstrap ratio ',laDim),
                            ylab = 'Bootstrap ratios'
)
print(ba002.BR2)
b005b.Bootstrap_ratios <- recordPlot()
laDim = 3
ba002.BR3 <- PrettyBarPlot2(BR[,laDim],
                            threshold = 2,
                            font.size = 5,color4bar = gplots::col2hex(col4J.ibm),
                            main = paste0(
                              'PCA on the IBM-No-Attririon data Set: Bootstrap ratio ',laDim),
                            ylab = 'Bootstrap ratios'
)
print(ba002.BR3)
b005c.Bootstrap_ratios <- recordPlot()

#_____________________________________________________________________
#_____________________________________________________________________
# Use the factor scores from resPCA. 
# The groups for the irises is in grIris
#_____________________________________________________________________
# Mean Map
#  create the map for the means
#  get the means by groups

dataMeans <- PTCA4CATA::getMeans(resPCA$ExPosition.Data$fi, datae)
# a vector of color for the means
col4data <- resPCA$Plotting.Data$fi.col
col4Means <- unique(col4data)
# the map
MapGroup <- PTCA4CATA::createFactorMap(dataMeans,
                                       # use the constraint from the main map
                                       constraints = my_data1.Imap$constraints,
                                       col.points = col4Means,
                                       cex = 7,  # size of the dot (bigger)
                                       col.labels = col4Means,
                                       text.cex = 6)
# The map with observations and group means
a003.Map.I.withMeans <- a002.Map.I +
  MapGroup$zeMap_dots + MapGroup$zeMap_text
print(a003.Map.I.withMeans)


#_____________________________________________________________________
dataMeans1 <- PTCA4CATA::getMeans(resPCA1$ExPosition.Data$fi, datag)
# a vector of color for the means
col4data1 <- resPCA1$Plotting.Data$fi.col
col4Means1 <- unique(col4data1)
# the map
MapGroup1  <- PTCA4CATA::createFactorMap(dataMeans1,
                                       # use the constraint from the main map
                                       constraints = my_data1.Imap1$constraints,
                                       col.points = col4Means1,
                                       cex = 7,  # size of the dot (bigger)
                                       col.labels = col4Means1,
                                       text.cex = 6)
# The map with observations and group means
a003.Map.I.withMeans1 <- a002.Map.I1 +
  MapGroup1$zeMap_dots + MapGroup1$zeMap_text
print(a003.Map.I.withMeans1)

GraphElli1 <- PTCA4CATA::MakeCIEllipses(BootCube.Gr1$BootCube[,1:2,],
                                       names.of.factors = c("Dimension 1","Dimension 2"),
                                       col = col4Means1,
                                       p.level = .95
)
#_____________________________________________________________________
# create the I-map with Observations, means and confidence intervals
#
a004.Map.I.withCI1 <-  a002.Map.I1 + MapGroup1$zeMap_text +  GraphElli1
#_____________________________________________________________________
# plot it!
dev.new()
print(a004.Map.I.withCI1)
#________________________



#_____________________________________________________________________


# Create the ellipses
# Bootstrapped CI ----
#_____________________________________________________________________
# Create Confidence Interval Plots
# use function MakeCIEllipses from package PTCA4CATA
GraphElli <- PTCA4CATA::MakeCIEllipses(BootCube.Gr$BootCube[,1:2,],
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

#_____________________________________________________________________
# End of graphs ----
#_____________________________________________________________________
# Save as Powerpoint ----
#_____________________________________________________________________
#  
listSaved <- saveGraph2pptx(
  file2Save.pptx = 'PCA4TheIBM-No-Attrition-DataSet.pptx', 
  title = "IBM-No-Attrition-data set: PCA, confidence and tolerance intervals", 
  addGraphNames = TRUE)






