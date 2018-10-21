rm(list = ls())
graphics.off()


my_data <- read.csv("IBM-HR-Emplyee-NoAttrition.csv")
cols <- colnames(my_data)
rownames(my_data) <- my_data$Subj
data1 <- my_data[,11:32]
data1$Age <- my_data$Age

data1$MonthlyIncome <- cut(data1$MonthlyIncome,breaks = c(min(data1$MonthlyIncome)-1,5000 ,10000, max(data1$MonthlyIncome)+1),labels = c(1,2,3))
data1$DailyRate <- cut(data1$DailyRate,breaks = c(min(data1$DailyRate)-1,600 ,1100, max(data1$DailyRate)+1),labels = c(1,2,3))
data1$HourlyRate <- cut(data1$HourlyRate,breaks = c(min(data1$HourlyRate)-1,55 ,80, max(data1$HourlyRate)+1),labels = c(1,2,3))
data1$MonthlyRate <- cut(data1$MonthlyRate,breaks = c(min(data1$MonthlyRate)-1,10000 ,17500, max(data1$MonthlyRate)+1),labels = c(1,2,3))
data1$DistanceFromHome <- cut(data1$DistanceFromHome,breaks = c(min(data1$DistanceFromHome)-1,6 , max(data1$DistanceFromHome)+1),labels = c(1,2))
data1$Education <- cut(data1$Education,breaks = c(min(data1$Education)-1, 2,3 , max(data1$Education)+1),labels = c(1,2,3))
data1$JobInvolvement <- cut(data1$JobInvolvement,breaks = c(min(data1$JobInvolvement)-1,2.5,3.5, max(data1$JobInvolvement)+1),labels = c(1,2,3))
data1$JobLevel <- cut(data1$JobLevel,breaks = c(min(data1$JobLevel)-1,2,4, max(data1$JobLevel)+1),labels = c(1,2,3))
data1$NumCompaniesWorked <- cut(data1$NumCompaniesWorked,breaks = c(min(data1$NumCompaniesWorked)-1, 2,6, max(data1$NumCompaniesWorked)+1),labels = c(1,2,3))
data1$TotalWorkingYears <- cut(data1$TotalWorkingYears,breaks = c(min(data1$TotalWorkingYears)-1, 10, max(data1$TotalWorkingYears)+1),labels = c(1,2))
data1$PercentSalaryHike <- cut(data1$PercentSalaryHike,breaks = c(min(data1$PercentSalaryHike)-1, 13,19, max(data1$PercentSalaryHike)+1),labels = c(1,2,3))
data1$WorkLifeBalance <- cut(data1$WorkLifeBalance,breaks = c(min(data1$WorkLifeBalance)-1, 2, max(data1$WorkLifeBalance)+1),labels = c(1,2))
data1$Age <- cut(data1$Age,breaks = c(min(data1$Age)-1, 30,40, max(data1$Age)+1),labels = c(1,2,3))
data1$TrainingTimesLastYear <- cut(data1$TrainingTimesLastYear,breaks = c(min(data1$TrainingTimesLastYear)-1, 2,4, max(data1$TrainingTimesLastYear)+1),labels = c(1,2,3))
data1$YearsAtCompany <- cut(data1$YearsAtCompany,breaks = c(min(data1$YearsAtCompany)-1, 5,15, max(data1$YearsAtCompany)+1),labels = c(1,2,3))
data1$YearsInCurrentRole <- cut(data1$YearsInCurrentRole,breaks = c(min(data1$YearsInCurrentRole)-1, 4,10, max(data1$YearsInCurrentRole)+1),labels = c(1,2,3))
data1$YearsSinceLastPromotion <- cut(data1$YearsSinceLastPromotion,breaks = c(min(data1$YearsSinceLastPromotion)-1, 2,7, max(data1$YearsSinceLastPromotion)+1),labels = c(1,2,3))
data1$YearsWithCurrManager <- cut(data1$YearsWithCurrManager,breaks = c(min(data1$YearsWithCurrManager)-1, 4,10, max(data1$YearsWithCurrManager)+1),labels = c(1,2,3))
library(corrplot)
data2 <- my_data[,11:32]
data2$Age <- my_data$Age
cor.my_data <- cor(data2)
corrplot(cor.my_data, method = "ellipse")

data1$PerformanceRating <- as.factor(data1$PerformanceRating)
data1$JobInvolvement <- as.factor(data1$JobInvolvement)
data1$StockOptionLevel <- as.factor(data1$StockOptionLevel)
data1$EnvironmentSatisfaction <- as.factor(data1$EnvironmentSatisfaction)
data1$JobSatisfaction <- as.factor(data1$JobSatisfaction)
data1$RelationshipSatisfaction <- as.factor(data1$RelationshipSatisfaction)


resMCA.sym <- epMCA(data1 ,make_data_nominal = TRUE ,DESIGN = my_data$Department ,make_design_nominal = TRUE,graphs = FALSE, symmetric = TRUE)

resMCA.asym <- epMCA(data1 ,make_data_nominal = TRUE ,DESIGN = my_data$Department ,make_design_nominal = TRUE,graphs = FALSE, symmetric = FALSE)

resMCA.inf <- epMCA.inference.battery(data1, make_data_nominal = TRUE, DESIGN = my_data$Department ,make_design_nominal =TRUE,graphs = FALSE)

DESIGN <- list()
DESIGN$rows$Region$labels <- unique(my_data$Department) 
DESIGN$rows$Region$vec <- my_data$Department

#Convert the vector to a matrix
DESIGN$rows$Region$mat <- makeNominalData(as.matrix(DESIGN$rows$Region$vec))
automatic_colors <- createColorVectorsByDesign(DESIGN$rows$Region$mat)
DESIGN$rows$Region$color_groups <- c("red","blue","yellow")
DESIGN$rows$Region$color_observ <- as.matrix(DESIGN$rows$Region$vec) 

DESIGN$rows$Region$color_observ[which(DESIGN$rows$Region$vec=="Research & Development")]    <- DESIGN$rows$Region$color_groups[1]
DESIGN$rows$Region$color_observ[which(DESIGN$rows$Region$vec=="Human Resources")]       <- DESIGN$rows$Region$color_groups[2]
DESIGN$rows$Region$color_observ[which(DESIGN$rows$Region$vec=="Sales")]     <- DESIGN$rows$Region$color_groups[3]

PlotScree(ev = resMCA.sym$ExPosition.Data$eigs, 
          p.ev =  resMCA.inf$Inference.Data$components$p.vals,
          title = 'IBM-No-Attririon data Set. Eigenvalues Inference',
          plotKaiser = TRUE
)

MonthlyIncome1 <- "red"
DailyRate1 <- "pink"
HourlyRate1 <- "blue"
MonthlyRate1 <- "skyblue"
DistanceFromHome1 <- "green"
PerformanceRating1 <- "navyblue"
Education1 <- "darkolivegreen4"
JobInvolvement1 <- "darkgoldenrod3"
JobLevel1 <- "brown"
StockOptionLevel1 <- "orange"
NumCompaniesWorked1 <- "cornflowerblue"
PercentSalaryHike1 <- "chartreuse3"
TotalWorkingYears1 <- "peachpuff3"
TrainingTimesLastYear1 <- "mediumorchid2"
WorkLifeBalance1 <- "turquoise3"
YearsAtCompany1 <- "wheat4"
YearsInCurrentRole1 <-"slategray2"
YearsSinceLastPromotion1 <- "purple"
YearsWithCurrManager1 <- "magenta"
EnvironmentSatisfaction1 <- "teal"
JobSatisfaction1 <- "chromeyellow"
RelationshipSatisfaction1 <- "yellow"
Age1 <- "black"

col4J <- dplyr::recode(resMCA.sym$Plotting.Data$fj.col,
                       'JobLevel1' = JobLevel1 , 'MonthlyIncome1' = MonthlyIncome1, 'DailyRate1' = DailyRate1, 'HourlyRate'=HourlyRate1, 'MonthlyRate' =MonthlyRate1, 'DistanceFromHome'= DistanceFromHome1,
                       'PerformanceRating'= PerformanceRating1, 'Education' =Education1, 'JobInvolvement'=JobInvolvement1, 'StockOptionLevel'=StockOptionLevel1,
                      'NumCompaniesWorked'= NumCompaniesWorked1 ,  'PercentSalaryHike'= PercentSalaryHike1 ,'TotalWorkingYears'= TotalWorkingYears1, 'TrainingTimesLastYear'=TrainingTimesLastYear1,
                     'WorkLifeBalance'=WorkLifeBalance1, 'YearsAtCompany'= YearsAtCompany1, 'YearsInCurrentRole'= YearsInCurrentRole1,
                      'YearsSinceLastPromotion'= YearsSinceLastPromotion1, 'YearsWithCurrManager'=YearsWithCurrManager1 ,'EnvironmentSatisfaction'= EnvironmentSatisfaction1,
                     'JobSatisfaction'=JobSatisfaction1 ,'RelationshipSatisfaction'=RelationshipSatisfaction1,'Age'= Age1 )


## Biplot for symmetrical plot for component 1 and 2 for Department datatype

col4I <- prettyGraphsColorSelection(NCOL(resMCA.sym$Plotting.Data$fj.col))
symMap1  <- createFactorMapIJ(resMCA.sym$ExPosition.Data$fi,resMCA.sym$ExPosition.Data$fj,
                              col.points.i = DESIGN$rows$Region$color_observ,
                              col.points.j = col4I,
                              col.labels.i =  DESIGN$rows$Region$color_observ ,
                              col.labels.j = col4I ,
                              cex.i = 2.5, pch.i = 20, 
                              pch.j = 21,  cex.j = 2.5,text.cex.j =2, axis1 = 1,axis2 = 2, title = "Symmetrical plot for components 1 and 2 for Department Type",
                              alpha.axes = 0.2,alpha.points.i = 1)


labels4MCA1 <- createxyLabels(resCA = resMCA.sym, x_axis = 1,y_axis = 2)

map.IJ.sym1 <- symMap1$baseMap + symMap1$I_points 
map.IJ.sym2 <- symMap1$baseMap + symMap1$J_labels + symMap1$J_points + labels4MCA1
map.IJ.sym3 <- symMap1$baseMap + symMap1$I_points +
  symMap1$J_labels + symMap1$J_points + labels4MCA1
print(map.IJ.sym1)
print(map.IJ.sym2)
print(map.IJ.sym3)

## Biplot for symmetrical plot for component 2 and 3 for Department datatype

symMap2  <- createFactorMapIJ(resMCA.sym$ExPosition.Data$fi,resMCA.sym$ExPosition.Data$fj,
                              col.points.i = DESIGN$rows$Region$color_observ,
                              col.points.j = "black",
                              col.labels.i = DESIGN$rows$Region$color_observ ,
                              col.labels.j = "black" ,
                              cex.i = 5, pch.i = 20, 
                              pch.j = 21,  text.cex.j =2, axis1 = 2,axis2 = 3, title = "Symmetrical plot for components 2 and 3 for Department Type",
                              alpha.axes = 0.2,alpha.points.i = 1)

labels4MCA2 <- createxyLabels(resCA = resMCA.sym, x_axis = 2, y_axis = 3)

map.IJ.sym11 <- symMap2$baseMap + symMap2$I_points 
map.IJ.sym21 <- symMap2$baseMap + symMap2$J_labels + symMap2$J_points + labels4MCA2
map.IJ.sym31 <- symMap2$baseMap + symMap2$I_points +
  symMap2$J_labels + symMap2$J_points + labels4MCA2
print(map.IJ.sym11)
print(map.IJ.sym21)
print(map.IJ.sym31)

## Biplot for symmetrical plot for component 1 and 3 for Department datatype

symMap3  <- createFactorMapIJ(resMCA.sym$ExPosition.Data$fi,resMCA.sym$ExPosition.Data$fj,
                              col.points.i = DESIGN$rows$Region$color_observ ,
                              col.points.j = "black",
                              col.labels.i = DESIGN$rows$Region$color_observ  ,
                              col.labels.j = "black" ,
                              cex.i = 5, pch.i = 20, 
                              pch.j = 21,  text.cex.j =2, axis1 = 1,axis2 = 3, title = "Symmetrical plot for components 1 and 3 for Department Type",
                              alpha.axes = 0.2,alpha.points.i = 1)

labels4MCA3 <- createxyLabels(resCA = resMCA.sym, x_axis = 1,y_axis = 3)

map.IJ.sym31 <- symMap3$baseMap + symMap3$I_points 
map.IJ.sym32 <- symMap3$baseMap + symMap3$J_labels + symMap3$J_points + labels4MCA3
map.IJ.sym33 <- symMap3$baseMap + symMap3$I_points +
  symMap3$J_labels + symMap3$J_points + labels4MCA3
print(map.IJ.sym31)
print(map.IJ.sym32)
print(map.IJ.sym33)

# color for gender 
DESIGN1 <- list()
DESIGN1$rows$Region$labels <- unique(my_data$Gender) 
DESIGN1$rows$Region$vec <- my_data$Gender

#Convert the vector to a matrix
DESIGN1$rows$Region$mat <- makeNominalData(as.matrix(DESIGN1$rows$Region$vec))
automatic_colors <- createColorVectorsByDesign(DESIGN1$rows$Region$mat)
DESIGN1$rows$Region$color_groups <- c("skyblue","pink")
DESIGN1$rows$Region$color_observ <- as.matrix(DESIGN1$rows$Region$vec) 

DESIGN1$rows$Region$color_observ[which(DESIGN1$rows$Region$vec=="Male")]    <- DESIGN1$rows$Region$color_groups[1]
DESIGN1$rows$Region$color_observ[which(DESIGN1$rows$Region$vec=="Female")]       <- DESIGN1$rows$Region$color_groups[2]

# inference 

resMCA.sym1 <- epMCA(data1 ,make_data_nominal = TRUE ,DESIGN = my_data$Gender ,make_design_nominal = TRUE,graphs = FALSE, symmetric = TRUE)

resMCA.asym1 <- epMCA(data1 ,make_data_nominal = TRUE ,DESIGN = my_data$Gender ,make_design_nominal = TRUE,graphs = FALSE, symmetric = FALSE)

resMCA.inf1 <- epMCA.inference.battery(data1, make_data_nominal = TRUE, DESIGN = my_data$Gender ,make_design_nominal =TRUE,graphs = FALSE)

## Biplot for symmetrical plot for component 1 and 2 for Gender datatype

symMa  <- createFactorMapIJ(resMCA.sym1$ExPosition.Data$fi,resMCA.sym1$ExPosition.Data$fj,
                              col.points.i = DESIGN1$rows$Region$color_observ,
                              col.points.j = "black",
                              col.labels.i = DESIGN1$rows$Region$color_observ ,
                              col.labels.j = "black" ,
                              cex.i = 2.5, pch.i = 20, 
                              pch.j = 21,  cex.j = 2.5,text.cex.j =2, axis1 = 1,axis2 = 2, title = "Symmetrical plot for components 1 and 2 for Gender Type",
                              alpha.axes = 0.2,alpha.points.i = 1)


labels4 <- createxyLabels(resCA = resMCA.sym1, x_axis = 1,y_axis = 2)

map.IJ.sym111 <- symMa$baseMap + symMa$I_points 
map.IJ.sym222 <- symMa$baseMap + symMa$J_labels + symMa$J_points + labels4
map.IJ.sym333 <- symMa$baseMap + symMa$I_points +
  symMa$J_labels + symMa$J_points + labels4
print(map.IJ.sym111)
print(map.IJ.sym222)
print(map.IJ.sym333)

## Biplot for symmetrical plot for component 2 and 3 for Gender datatype

symMap21  <- createFactorMapIJ(resMCA.sym1$ExPosition.Data$fi,resMCA.sym1$ExPosition.Data$fj,
                              col.points.i = DESIGN1$rows$Region$color_observ,
                              col.points.j = "black",
                              col.labels.i = DESIGN1$rows$Region$color_observ ,
                              col.labels.j = "black" ,
                              cex.i = 5, pch.i = 20, 
                              pch.j = 21,  text.cex.j =2, axis1 = 2,axis2 = 3, title = "Symmetrical plot for components 2 and 3 for Gender Type",
                              alpha.axes = 0.2,alpha.points.i = 1)

labels4MCA22 <- createxyLabels(resCA = resMCA.sym1, x_axis = 2, y_axis = 3)

map.IJ.sym1111 <- symMap21$baseMap + symMap21$I_points 
map.IJ.sym211 <- symMap21$baseMap + symMap21$J_labels + symMap21$J_points + labels4MCA22
map.IJ.sym311 <- symMap21$baseMap + symMap21$I_points +
  symMap21$J_labels + symMap21$J_points + labels4MCA22
print(map.IJ.sym1111)
print(map.IJ.sym211)
print(map.IJ.sym311)

## Biplot for symmetrical plot for component 1 and 3 for Gender datatype

```{r}
symMap31  <- createFactorMapIJ(resMCA.sym1$ExPosition.Data$fi,resMCA.sym1$ExPosition.Data$fj,
                              col.points.i = DESIGN1$rows$Region$color_observ,
                              col.points.j = "black",
                              col.labels.i = DESIGN1$rows$Region$color_observ ,
                              col.labels.j = "black" ,
                              cex.i = 5, pch.i = 20, 
                              pch.j = 21,  text.cex.j =2, axis1 = 1,axis2 = 3, title = "Symmetrical plot for components 1 and 3 for Gender Type",
                              alpha.axes = 0.2,alpha.points.i = 1)

labels4MCA31 <- createxyLabels(resCA = resMCA.sym1, x_axis = 1,y_axis = 3)

map.IJ.sym311 <- symMap31$baseMap + symMap31$I_points 
map.IJ.sym321 <- symMap31$baseMap + symMap31$J_labels + symMap31$J_points + labels4MCA31
map.IJ.sym331 <- symMap31$baseMap + symMap31$I_points +
  symMap31$J_labels + symMap31$J_points + labels4MCA31
print(map.IJ.sym311)
print(map.IJ.sym321)
print(map.IJ.sym331)

## Biplot for asymmetrical plot for component 1

asymMap1  <- createFactorMapIJ(resMCA.asym1$ExPosition.Data$fi,resMCA.asym1$ExPosition.Data$fj,
                               col.points.i = DESIGN1$rows$Region$color_observ,
                               col.points.j = "black",
                               col.labels.i = DESIGN1$rows$Region$color_observ ,
                               col.labels.j = "black" ,
                               cex.i = 5, pch.i = 20, 
                               pch.j = 21,  text.cex.j =2, 
                               alpha.axes = 0.2,alpha.points.i = 1, axis1 = 1, axis2 = 2, title = " Asymmetrical plot for components 1 and 2")

alabels4MCA1 <- createxyLabels(resCA = resMCA.asym1)

map.IJ.asym1 <- asymMap1$baseMap + asymMap1$I_points +
  asymMap1$J_labels + asymMap1$J_points + alabels4MCA1
print(map.IJ.asym1)

## Bootstrap Interval 

constraints.mca <- minmaxHelper(mat1 = resMCA.sym$ExPosition.Data$fi, mat2  = resMCA.sym$ExPosition.Data$fj)

color <- prettyGraphsColorSelection(NCOL(data1))
baseMap.i1 <- createFactorMap(resMCA.sym$ExPosition.Data$fi,constraints = constraints.mca,
                              col.points =  DESIGN$rows$Region$color_observ , axis1 = 1, axis2 = 2,
                              cex = 1, pch = 20,
                              display.labels = FALSE
)
label4Map1 <- createxyLabels.gen(1,2,
                                 lambda =resMCA.sym$ExPosition.Data$eigs,
                                 tau = resMCA.sym$ExPosition.Data$t)

a1 <- baseMap.i1$zeMap + baseMap.i1$zeMap_dots +label4Map1

BootCube.Gr <- Boot4Mean(resMCA.sym$ExPosition.Data$fi, 
                         design = my_data$Department,
                         niter = 100,
                         suppressProgressBar = TRUE)
#_____________________________________________________________________
# Bootstrap ratios ----
bootRatios.Gr <- boot.ratio.test(BootCube.Gr$BootCube)
#*********************************************************************
# Mean Map
#  create the map for the means
#  get the means by groups

dataMeans <- getMeans(resMCA.sym$ExPosition.Data$fi, my_data$Department)
# a vector of color for the means
col4data <- DESIGN$rows$Region$color_observ
col4Means <- unique(col4data)
# the map
MapGroup <- createFactorMap(dataMeans,
                            # use the constraint from the main map
                            constraints = constraints.mca,
                            col.points = col4Means,
                            cex = 7,  # size of the dot (bigger)
                            col.labels = col4Means,
                            text.cex = 6)
# The map with observations and group means
a003.Map.I.withMeans <- a1 +
  MapGroup$zeMap_dots + MapGroup$zeMap_text
print(a003.Map.I.withMeans)
#_____________________________________________________________________
# Create the ellipses
# Bootstrapped CI ----
#_____________________________________________________________________
# Create Confidence Interval Plots
# use function MakeCIEllipses from package PTCA4CATA
GraphElli <- MakeCIEllipses(BootCube.Gr$BootCube[,1:2,],
                            names.of.factors = c("Dimension 1","Dimension 2"),
                            col = col4Means,
                            p.level = .95
)
#_____________________________________________________________________
# create the I-map with Observations, means and confidence intervals
#
a004.Map.I.withCI <-  a1 + MapGroup$zeMap_text +  GraphElli
#_____________________________________________________________________
# plot it!
dev.new()
print(a004.Map.I.withCI)

GraphTI.Hull <- PTCA4CATA::MakeToleranceIntervals(resMCA.sym$ExPosition.Data$fi,
                                                   design = my_data$Department,
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
a005.Map.I.withTIHull <-a1 +
  GraphTI.Hull + MapGroup$zeMap_dots +
  MapGroup$zeMap_text + MapGroup$zeMap_dots
#_____________________________________________________________________
# plot it
print(a005.Map.I.withTIHull)



# for gender 

constraints.mca1 <- minmaxHelper(mat1 = resMCA.sym1$ExPosition.Data$fi, mat2  = resMCA.sym1$ExPosition.Data$fj)
baseMap.i2 <- createFactorMap(resMCA.sym1$ExPosition.Data$fi,constraints = constraints.mca1,
                              col.points = DESIGN1$rows$Region$color_observ, axis1 = 1, axis2 = 2,
                              cex = 1, pch = 20,
                              display.labels = FALSE
)
label4Map2 <- createxyLabels.gen(1,2,
                                 lambda =resMCA.sym1$ExPosition.Data$eigs,
                                 tau = resMCA.sym1$ExPosition.Data$t)

a2 <- baseMap.i2$zeMap + baseMap.i2$zeMap_dots +label4Map2

BootCube.Gr1 <- Boot4Mean(resMCA.sym1$ExPosition.Data$fi, 
                         design = my_data$Gender,
                         niter = 100,
                         suppressProgressBar = TRUE)
#_____________________________________________________________________
# Bootstrap ratios ----
bootRatios.Gr1 <- boot.ratio.test(BootCube.Gr1$BootCube)
#*********************************************************************
# Mean Map
#  create the map for the means
#  get the means by groups

dataMeans1 <- getMeans(resMCA.sym1$ExPosition.Data$fi, my_data$Gender)
# a vector of color for the means
col4data1 <- DESIGN1$rows$Region$color_observ
col4Means1 <- unique(col4data1)
# the map
MapGroup1 <- createFactorMap(dataMeans1,
                            # use the constraint from the main map
                            constraints = constraints.mca1,
                            col.points = col4Means1,
                            cex = 7,  # size of the dot (bigger)
                            col.labels = col4Means1,
                            text.cex = 6)
# The map with observations and group means
a003.Map.I.withMeans1 <- a2 +
  MapGroup1$zeMap_dots + MapGroup1$zeMap_text
print(a003.Map.I.withMeans1)
#_____________________________________________________________________
# Create the ellipses
# Bootstrapped CI ----
#_____________________________________________________________________
# Create Confidence Interval Plots
# use function MakeCIEllipses from package PTCA4CATA
GraphElli1 <- MakeCIEllipses(BootCube.Gr1$BootCube[,1:2,],
                            names.of.factors = c("Dimension 1","Dimension 2"),
                            col = col4Means1,
                            p.level = .95
)
#_____________________________________________________________________
# create the I-map with Observations, means and confidence intervals
#
a004.Map.I.withCI1 <-  a2 + MapGroup1$zeMap_text +  GraphElli1
#_____________________________________________________________________
# plot it!
print(a004.Map.I.withCI1)

GraphTI.Hull1 <- PTCA4CATA::MakeToleranceIntervals(resMCA.sym1$ExPosition.Data$fi,
                                                  design = my_data$Gender,
                                                  # line below is needed
                                                  names.of.factors =  c("Dim1","Dim2"), # needed 
                                                  col = col4Means1,
                                                  line.size = .50, 
                                                  line.type = 3,
                                                  alpha.ellipse = .2,
                                                  alpha.line    = .4,
                                                  p.level       = .75)
#_____________________________________________________________________
# Create the map:
a005.Map.I.withTIHull1 <-a2  +
  GraphTI.Hull1 + MapGroup1$zeMap_dots +
  MapGroup1$zeMap_text + MapGroup1$zeMap_dots
#_____________________________________________________________________
# plot it
print(a005.Map.I.withTIHull1)

## Contribution for variables 

signed.ctrJ <- resMCA.sym$ExPosition.Data$cj * sign(resMCA.sym$ExPosition.Data$fj)
b003.ctrJ.s.1 <- PrettyBarPlot2(signed.ctrJ[,1],
                                threshold = 1 / NROW(signed.ctrJ),
                                font.size = 5,
                                # color4bar = gplots::col2hex(col4J.ibm), # we need hex code
                                main = 'MCA on the IBM-No-Attririon data Set: Variable Contributions (Signed)',
                                ylab = 'Contributions',
                                ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
)
print(b003.ctrJ.s.1)
b004.ctrJ.s.2 <- PrettyBarPlot2(signed.ctrJ[,2],
                                threshold = 1 / NROW(signed.ctrJ),
                                font.size = 5,
                                # color4bar = gplots::col2hex(col4J.ibm), # we need hex code
                                main = 'MCA on the IBM-No-Attririon dataSet: Variable Contributions (Signed)',
                                ylab = 'Contributions',
                                ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
)
print(b004.ctrJ.s.2)
b004.ctrJ.s.3 <- PrettyBarPlot2(signed.ctrJ[,3],
                                threshold = 1 / NROW(signed.ctrJ),
                                font.size = 5,
                                # color4bar = gplots::col2hex(col4J.ibm), # we need hex code
                                main = 'MCA on the IBM-No-Attririon dataSet: Variable Contributions (Signed)',
                                ylab = 'Contributions',
                                ylim = c(1.2*min(signed.ctrJ), 1.2*max(signed.ctrJ))
)
print(b004.ctrJ.s.3)


## Bootstrap Ratios for Variables 

BR <- resMCA.inf$Inference.Data$fj.boots$tests$boot.ratios
laDim = 1
ba001.BR1 <- PrettyBarPlot2(BR[,laDim],
                            threshold = 2,
                            font.size = 5,
                            #color4bar = gplots::col2hex(col4J.ibm),
                            main = paste0( 'MCA on the IBM-NoAttrition data Set: Bootstrap ratio ',laDim),
                            ylab = 'Bootstrap ratios'
                            #ylim = c(1.2*min(BR[,laDim]), 1.2*max(BR[,laDim]))
)
print(ba001.BR1)
#
laDim = 2
ba002.BR2 <- PrettyBarPlot2(BR[,laDim],
                            threshold = 2,
                            font.size = 5,
                            #color4bar = gplots::col2hex(col4J.ibm),
                            main = paste0(
                              'MCA on the IBM-NoAttrition data Set: Bootstrap ratio ',laDim),
                            ylab = 'Bootstrap ratios'
)
print(ba002.BR2)
laDim = 3
ba002.BR3 <- PrettyBarPlot2(BR[,laDim],
                            threshold = 2,
                            font.size = 5,
                            main = paste0(
                              'MCA on the IBM-NoAttrition data Set: Bootstrap ratio ',laDim),
                            ylab = 'Bootstrap ratios'
)
print(ba002.BR3)
