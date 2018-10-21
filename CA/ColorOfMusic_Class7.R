# Testing PTCA4CATA with Julien Color Example
# Hervé Abdi
# first created 01/30/2018
# Current Version 03/07/2018. HA
#
# Preamble ----
rm(list = ls())
graphics.off()

# To get the correct directory: Use .Rproject
dirHere   <- getwd()
name2save <- 'GraphicsColorsOfMusic.pptx'
path2save <- paste0(dirHere,'/',name2save)

# if needed, load the last version of PTCA4CATA
# devtools::install_github('HerveAbdi/PTCA4CATA', dependencies = FALSE)
install.packages("DistatisR")
suppressMessages(library(tidyverse))
suppressMessages(library(ExPosition))
suppressMessages(library(PTCA4CATA))
suppressMessages(library(factoextra))
suppressMessages(library(officer))    # to save Figures as powerpoint
suppressMessages(library(flextable))  # for officer
suppressMessages(library(rvg))        # for officer
suppressMessages(library(gridExtra))  # to save a table as a graph
suppressMessages(library(grid))       # that will be saved in the
suppressMessages(library(gtable))     # powerpoint with the figures
suppressMessages(library(data4PCCAR)) # more data and stuff

#_____________________________________________________________________
#_____________________________________________________________________
# get the data
# First: the analysis
# The original data for Julien are stored as data in the 
data("colorOfMusic")
# Create the array "The Cube of Music"
cubeOfMusic <- DataCheckMark2Cube(
  colorOfMusic$participantsChoice, colorOfMusic$colorInformation[,1])
# cubeOfMusic is an array with dimensions:
# 10 (colors) *9 (pieces of music) * 22 (participant)
# Create the contingency table
colorCT <- apply(cubeOfMusic,c(1,2),sum)
colorCT <- colorOfMusic$contingencyTable
# Create a heatmap for the data
#_____________________________________________________________________
# run a plain CA ----
# Run a plain CA (Fixed Effects)
resCA <- ExPosition::epCA(colorCT, graphs = FALSE)
#_____________________________________________________________________
# Participants RV-matrix ----
#********
# We will talk about Rv matrix, distance, distance matrix, and 
# multidimension scaling (MDS) later in the semester.
#********
#_____________________________________________________________________
# Get a Distance matrix between the participants
Dmat4Participants <- PTCA4CATA::createSymDist4PTCA(colorOfMusic$cubeOfData)
name4Participants <- paste0(
    substr(as.character(colorOfMusic$participantsDescription[,1]),1,1),
        as.character(colorOfMusic$participantsDescription[,2]),
        1:22)
# Metric MDS of the results
resMDS <- DistatisR::mmds(Dmat4Participants$Distance)

# 
#_____________________________________________________________________
# Inferences ----
#_____________________________________________________________________
# permutation ----
# First get the Permutation test
permTest <- PTCA4CATA::perm4PTCA(cubeOfMusic)
#_____________________________________________________________________
# Bootstrap & Bootstrap Ratios
#  Create Bootstrap Cube
bootCube <- PTCA4CATA::Boot4PTCA(colorOfMusic$cubeOfData,
                      resCA$ExPosition.Data$fi,
                      resCA$ExPosition.Data$fj,
                      eigs = resCA$ExPosition.Data$eig ,
                      nBootIter = 100,
                      eigen = TRUE , # if eigen is TRUE 
                      # compute bootstrapped eigenvalues
                      eigen.compact = FALSE 
)
#_____________________________________________________________________
#   Compute Bootstrap ratios
BRcolor  <-  boot.ratio.test(bootCube$RowsBoot)
BRmusic  <-  boot.ratio.test(bootCube$ColumnsBoot)
#_____________________________________________________________________
#
# The Graphics here ----
#_____________________________________________________________________
#_____________________________________________________________________
# Part 1. Descriptive graphs ----
#_____________________________________________________________________
# A heat map ----
#
a001.heatMap <- makeggHeatMap4CT(colorCT,
                   colorProducts = colorOfMusic$colorInformation[,2] ,
                   fontSize.x = 15
)  + 
ggtitle("Number of Participants Who Chose this Color For this Music") +
theme(plot.title = element_text(family = "Helvetica", 
                                  face = "bold", size = (20)))
#_____________________________________________________________________
# Graphic for MMDS on Participants' distance
# MMDS graph ----
#********
# This is MDS, which we will cover later in the semester.
#********
#_____________________________________________________________________
nN <- nrow(colorOfMusic$participantsDescription)
pch_M <- 17  # pch for Males
pch_F <- 19  # pch for Females
pch_MF <- rep(pch_F,nN)
pch_MF[colorOfMusic$participantsDescription[,2] == 'M' ] <- pch_M
col_C <- 'forestgreen'
col_A <- 'firebrick4'
col_CA <- rep(col_C,nN)
col_CA[ colorOfMusic$participantsDescription[,1] == 'Adult' ] <- col_A

mmds <- createFactorMap(X = resMDS$FactorScores, 
                        title = "MDS participants", 
                        col.points = col_CA, 
                        alpha.points = 0.5,
                        display.points = TRUE, 
                        pch = pch_MF, 
                        cex = 2.5, 
                        display.labels = TRUE,
                        col.labels = col_CA, 
                        alpha.labels = 1, 
                        text.cex = 4,
                        font.face = "plain", 
                        font.family = "sans")

MMDSlabels <- createxyLabels.gen(
  1,2, lambda = resMDS$eigenvalues,
  tau = round(resMDS$percentage) 
)
b001.mdsMap  <- mmds$zeMap + MMDSlabels
#print(b001.mdsMap)
#_____________________________________________________________________
# CA Fixed Effects ----
#_____________________________________________________________________
# The ScreePlot. Fixed Effects. ----
# Get the ScreePlot
PlotScree(ev = resCA$ExPosition.Data$eigs,
           title = 'The Color of Music. Inertia Scree Plot',
           plotKaiser = TRUE, 
           color4Kaiser = ggplot2::alpha('darkorchid4', .5),
           lwd4Kaiser  = 2)
# Save the plot
c001.Scree.fixed <- recordPlot()
# To print the graph of the scree:
# print(c001.Scree.fixed) # note: print not plot
#_____________________________________________________________________
# Factor maps ----
# Using the package PTCA4CATA
# Make a 1*2 Map symmetric Map
# First create the layers of the map
symMapIJ.layers <- createFactorMapIJ(
                 Fi = resCA$ExPosition.Data$fi,  
                 Fj = resCA$ExPosition.Data$fj ,
                 col.labels.i = colorOfMusic$colorInformation[,2] ,
                 col.points.i = colorOfMusic$colorInformation[,2] ,
                 font.face.i  = 'plain',
                 col.labels.j = 'gray63',
                 col.points.j = 'gray50',
                 pch.j = 15, cex.j = 4 ,
                 font.face.j = 'bold.italic')
# Make the map from its layers.
# First the labels
CAlabels <- createxyLabels(resCA)
# The map
d001.symMapIJ <-  symMapIJ.layers$baseMap +  
                symMapIJ.layers$I_labels + symMapIJ.layers$I_points +
                symMapIJ.layers$J_labels  + #  add axes labels
                CAlabels
#_____________________________________________________________________
# Asymmetric map ----
# First get the asymmetric factor scores
renormedF <- createAllNormedFactors(resCA)
# Create the layers for the Map
asymMapIJ.layers <- createFactorMapIJ(
  Fi = renormedF$Fi,  
  Fj = renormedF$Fj_A ,
  title = 'The color of Music Asymmetric Map',
  col.labels.i = colorOfMusic$colorInformation[,2] ,
  col.points.i = colorOfMusic$colorInformation[,2] ,
  font.face.i = 'plain',
  col.labels.j  = 'gray63',
  col.points.j = 'gray50',
  pch.j = 15, cex.j = 4 ,
  font.face.j = 'bold.italic')
# Make the map from its layers
d002.asymMapIJ <-  asymMapIJ.layers$baseMap +  
  asymMapIJ.layers$I_labels + asymMapIJ.layers$I_points +
  asymMapIJ.layers$J_labels + # add Labels
  CAlabels
print(d001.symMapIJ)
print(d002.asymMapIJ)
#_____________________________________________________________________
#_____________________________________________________________________

#_____________________________________________________________________
# 3. Plot with factoextra:: fviz_ca_biplot ----
#    (Thanks to Kassambara)
col4row <- factor(colorOfMusic$colorInformation[,2], 
                     levels = colorOfMusic$colorInformation[,2])
e001.asymFextra <- fviz_ca_biplot(resCA,
                map = "rowprincipal",
                col.col =   'gray60',
                col.row = col4row,
                repel = TRUE) +
                 scale_color_manual(values = as.character(col4row)) +
                 theme(legend.position = "none") + ggtitle("") +
                 theme(panel.border = element_blank(), 
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank()) +
                 coord_fixed() +
                 CAlabels
#print(e001.asymFextra)
#_____________________________________________________________________
# Contribution Plots ----
# prettyPLot / PTCA4CATA ----
# I-set
ctri <- resCA$ExPosition.Data$ci
Fi   <- resCA$ExPosition.Data$fi
signed.ctri <- ctri * sign(Fi)
# Dimension 1 
plotCtri.1 <- PrettyBarPlot2(bootratio = round(100*signed.ctri[,1]), 
                               threshold = 100/ nrow(signed.ctri) , 
                               ylim = NULL, 
                               color4bar = gplots::col2hex(col4row),
                               color4ns = "gray75", 
                               plotnames = TRUE, 
                               main = 'Important Contributions (Colors): Dimension 1' , 
                               ylab = "Signed Contributions")
# Dimension 2 
plotCtri.2 <-PrettyBarPlot2(
                          bootratio = round(100*signed.ctri[,2]), 
                          threshold = 100/ nrow(signed.ctri) , 
                          ylim = NULL, 
                          color4bar = gplots::col2hex(col4row),
                          color4ns = "gray75", 
                          plotnames = TRUE, 
                          main = NULL, ylab = "Signed Contributions")
# J-set 
ctrj <- resCA$ExPosition.Data$cj
Fj   <- resCA$ExPosition.Data$fj
signed.ctrj <- ctrj * sign(Fj)
# Dimension 1 
plotCtrj.1 <-PrettyBarPlot2(
               bootratio = round(100*signed.ctrj[,1]), 
               threshold = 100/ nrow(signed.ctrj) , 
               ylim = NULL, 
               color4bar = gplots::col2hex(rep('darkslateblue',
                                               nrow(signed.ctrj))),
               color4ns = "gray50", 
               plotnames = TRUE, 
               main = 'Important Contributions (Music): Dimension 1', 
               ylab = "Signed Contributions")
# Dimension 2 
plotCtrj.2 <- PrettyBarPlot2(
               bootratio = round(100*signed.ctrj[,2]), 
               threshold = 100/ nrow(signed.ctrj), 
               ylim = NULL, 
               color4bar = gplots::col2hex(rep('darkslateblue',
                                               nrow(signed.ctrj))),
               color4ns = "gray50", 
               plotnames = TRUE, 
               main = 'Important Contributions (Music): Dimension 2', 
              ylab = "Signed Contributions")
#_____________________________________________________________________
# Illustration of contributions with factorextra
e1.fe.ctri.1 <- fviz_contrib(resCA,choice = 'row',
                          axes = 1,
          fill = ggplot2::alpha('darkolivegreen',.5),
          color = ggplot2::alpha('darkolivegreen',.9)
          )
#_____________________________________________________________________
# Part 2.  Inferential graphs
#_____________________________________________________________________
#_____________________________________________________________________
# Malinveau Test ----
knitr::kable(permTest$MalinvaudQ, 
               caption = 'Malinvaud-Saporta Test', digit = 3)
# Save this table as a graph using gridExtra and grid
dev.new()
#MalinvaudTable <- grid.table(permTest$MalinvaudQ)
MalinvaudTable <- tableGrob(permTest$MalinvaudQ)
h <- grobHeight(MalinvaudTable)
w <- grobWidth(MalinvaudTable)
title <- textGrob("Malinvaud-Saporta Test for PTCA",
                  y=unit(0.5,"npc") + 1.2*h, 
                  vjust=0,
                  gp=gpar(fontsize=15))
TableWithTitle <- gTree(children=gList(MalinvaudTable, title))
grid.draw(TableWithTitle)
f1.MalinvaudTable <- recordPlot()
dev.off()
#_____________________________________________________________________
p4Scree <- permTest$MalinvaudQ[5,2:ncol(permTest$MalinvaudQ)]
inertia <- permTest$fixedInertia
pOmni   <- permTest$pOmnibus
# Note Malinveaud gets 3 significant components (agree with Keiser)
# Get the ScreePlot
zeTitle <-  paste0('Colors of Music. Inertia = ',round(inertia,3),
                                                      '. p = ',pOmni )
# SCree with inferences ----
PlotScree(ev = resCA$ExPosition.Data$eigs,
          p.ev = p4Scree,
          title = zeTitle,
          plotKaiser = TRUE, 
          color4Kaiser = ggplot2::alpha('darkorchid4', .5),
          lwd4Kaiser  = 2)
g1.Scree.random <- recordPlot()
#_____________________________________________________________________
# Bootstrap Ratios ----
# Dimension 1
h001.plotBR1_color <-  PrettyBarPlot2(BRcolor$boot.ratios[,1] , 
                   threshold = 2, ylim = NULL, 
                   color4bar = gplots::col2hex(col4row),
                   color4ns  =  'gray75', 
                   plotnames = TRUE, 
                   main = NULL,
                   ylab = NULL)

#Bootstrap ratios  for Music Pieces
h002.plotBR1_Music <- PrettyBarPlot2(bootratio = BRmusic$boot.ratios[,1])
# Dimension 2
h003.plotBR2_color <-  PrettyBarPlot2(BRcolor$boot.ratios[,2] , 
                                  threshold = 2, ylim = NULL, 
                                  color4bar = gplots::col2hex(col4row),
                                  color4ns  =  'gray75', 
                                  plotnames = TRUE, 
                                  main = NULL, 
                                  ylab = NULL)
#Bootstrap ratios  for Music Pieces
h004.plotBR1_Music <- PrettyBarPlot2(bootratio = BRmusic$boot.ratios[,1])
h005.plotBR2_Music <- PrettyBarPlot2(bootratio = BRmusic$boot.ratios[,2])

#
#_____________________________________________________________________
# Confidence Ellipsoids with PTCA4CATA ----
#
# For the colors  (Asymmetric Map)
leBootCube <- bootCube$RowsBoot
dimnames(leBootCube)[[2]] <- paste0('Dimension ',1:dim(leBootCube)[[2]])
ellipses4Color <- MakeCIEllipses(data = leBootCube, 
                    names.of.factors = c("Dimension 1","Dimension 2"),
                    col = colorOfMusic$colorInformation[,2], 
                    centers = NULL, 
                    line.size = 1, line.type = 1,
                    alpha.ellipse = 0.3, 
                    alpha.line = 0.5, p.level = 0.95)
# Use the old map and add ellipses
i001.asymMapIJ.CI <-  asymMapIJ.layers$baseMap +  
  asymMapIJ.layers$I_labels + asymMapIJ.layers$I_points +
  asymMapIJ.layers$J_point + # point for J
  ellipses4Color +
  CAlabels

#_____________________________________________________________________
# For the music pieces  (Asymmetric Map) ----
# First get the constraints to have the ellipses fitting in the graphs
  constrains4J <- minmaxHelper4Brick(bootCube$ColumnsBoot.asym,
                                    expandFactor = .95)
# No redo the base map to make sure 
# that the asymetric ellipses will fit 

  asymMapIJ.layers.CJ <- createFactorMapIJ(
    Fi = renormedF$Fi,    # Plain Fi
    Fj = renormedF$Fj_A , # Asymmetric Fj
    col.labels.i = colorOfMusic$colorInformation[,2] ,
    col.points.i = colorOfMusic$colorInformation[,2] ,
    font.face.i  = 'plain',
    col.labels.j = 'gray63',
    col.points.j = 'gray50',
    pch.j = 15, cex.j = 4 ,
    font.face.j = 'bold.italic',
    constraints = constrains4J  
    )
# create the asymmetric ellipses ----
ellipses4Music <- MakeCIEllipses(data = bootCube$ColumnsBoot.asym,
                                 names.of.factors = 
                                      c("Dimension 1","Dimension 2"),
                                 col = 'wheat3', 
                                 line.size = 1, line.type = 1,
                                 alpha.ellipse = 0.3, 
                                 alpha.line = 0.5, p.level = 0.95)
# create the asymetric map with ellipsoids.
i002.asymMapIJ.CJ <-  asymMapIJ.layers.CJ$baseMap +  
  # asymMapIJ.layers.CJ$I_labels + 
  asymMapIJ.layers.CJ$I_points +
  asymMapIJ.layers.CJ$J_points + 
  asymMapIJ.layers.CJ$J_labels +
  ellipses4Music +  CAlabels

#_____________________________________________________________________
# Compare with Symmetric Map and Asymmetric Map
# For the music pieces  (Asymmetric Map) ----
# First get the constraints to have the ellipses fitting in the graphs
constrains4J.sym <- minmaxHelper4Brick(bootCube$ColumnsBoot,
                                   expandFactor = .95)
# No redo the base map to make sure 
# that the asymetric ellipses will fit 

symMapIJ.layers.CJ <- createFactorMapIJ(
  Fi = renormedF$Fi,    # Plain Fi
  Fj = renormedF$Fj , # Asymmetric Fj
  col.labels.i = colorOfMusic$colorInformation[,2] ,
  col.points.i = colorOfMusic$colorInformation[,2] ,
  font.face.i  = 'plain',
  col.labels.j = 'gray63',
  col.points.j = 'gray50',
  pch.j = 15, cex.j = 4 ,
  font.face.j = 'bold.italic',
  constraints = constrains4J.sym  
  #lapply(minmaxHelper(Fi,renormedF$Fj),'*',3.2)
)
# create the asymmetric ellipses
ellipses4Music.sym <- MakeCIEllipses(data = bootCube$ColumnsBoot,
                                 names.of.factors = 
                                   c("Dimension 1","Dimension 2"),
                                 col = 'wheat3', 
                                 line.size = 1, line.type = 1,
                                 alpha.ellipse = 0.3, 
                                 alpha.line = 0.5, p.level = 0.95)
# create the asymetric map with ellipsoids.
i003.symMapIJ.CJ <-  symMapIJ.layers.CJ$baseMap +  
  # asymMapIJ.layers.CJ$I_labels + 
  symMapIJ.layers.CJ$I_points +
  symMapIJ.layers.CJ$J_points + 
  symMapIJ.layers.CJ$J_labels +
  ellipses4Music.sym +  CAlabels
#_____________________________________________________________________
# End of Creating Graphics ----
#_____________________________________________________________________

#_____________________________________________________________________
# save a pptx
# Automatic save with saveGraph2pptx
savedList <- PTCA4CATA::saveGraph2pptx(file2Save.pptx = path2save, 
                            title = NULL, 
                            addGraphNames = TRUE)

#_____________________________________________________________________
