# Part 1 of class 10
# create nominal variables
# Illustrate with the iris data set.
#

# Current version is October 15,  2018.
# Last edit: .
# HA. October 15, 2018.
## clean start
rm(list = ls())
graphics.off()
# Get there with an Rproject
# if not an Rproject set the directory here
# e.g.,
# setwd('~/Box Sync/RM3-CurrentYear/R4RM3/r-Class09-10/')

library(ExPosition)
library(InPosition)
# use the last version of PTCA
# devtools::install_github('HerveAbdi/PTCA4CATA')
library(PTCA4CATA)
library(corrplot)
library(ggplot2)
# install.packages('gplots')
# also install data4PCCAR
# devtools::install_github('HerveAbdi/data4PCCAR')
library(data4PCCAR)

#
#here we load the classic iris data set
data(iris)
# The iris data set ----
#_____________________________________________________________________
# Have a look at the iris data set
summary(iris)
#_____________________________________________________________________
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

#  MCA ----


# # ```{r BinQuant, echo = CodePrint,eval =TRUE}
#' 
#'  recode a quantitative variable
#'   as a binned factor.
#' \code{BinQuant} a function to recode a quantitative variable
#'   as a binned factor.
#' @param x A vector (of numbers)
#' @param nclass = 4 number of bins for the recoded factor
#' @param   stem = 'A' Stem for the level of the recoded factor 
#' if the number of class does not match: reevaluate it
#' @author Hervé Abdi
#' @export
BinQuant <- function(x, nclass = 4, stem = 'A'){
  x <- as.numeric(x)  
  qFact = quantile(x,probs = seq(0, 1, 1/nclass))
  nclass = length(qFact)-1
  CodedFact = cut(x, breaks= qFact, 
                  include.lowest = TRUE,
                  labels=paste0(stem,1:nclass))
  return(CodedFact)                
}
# ```


## Look at the variables ----

hist.SL <- hist(mesIris[,1], breaks = 20) # 4
hist.SW <- hist(mesIris[,2], breaks = 20) # 3
hist.PL <- hist(mesIris[,3], breaks = 20) # 3
hist.PW <- hist(mesIris[,4], breaks = 20) # 3

# Initialized recoded df
mesIrisRecoded <- data.frame(row.names = row.names(mesIris))

# Sepal Length: Recode as 4 quartiles
irec = which(colnames(mesIris)=='Sepal.Length')
mesIrisRecoded[,colnames(mesIris)[irec]] <- BinQuant(
                                 mesIris[,irec],nclass = 4,stem = 'SL')
irec = which(colnames(mesIris)=='Sepal.Width')
mesIrisRecoded[,colnames(mesIris)[irec]] <- BinQuant(
                                 mesIris[,irec],nclass = 3,stem = 'SW')
irec = which(colnames(mesIris)=='Petal.Length')
mesIrisRecoded[,colnames(mesIris)[irec]] <- BinQuant(
                                 mesIris[,irec],nclass = 3,stem = 'PL')
irec = which(colnames(mesIris)=='Petal.Width')
mesIrisRecoded[,colnames(mesIris)[irec]] <- BinQuant(
                                 mesIris[,irec],nclass = 3,stem = 'PW')




# Run the MCA
#_____________________________________________________________________
# PCA ----
resMCA <- epMCA(DATA = mesIrisRecoded,
                DESIGN = grIris,
                graphs = FALSE # TRUE first pass only
)
#_____________________________________________________________________
#_____________________________________________________________________
# Inference battery ----
resMCA.inf <- InPosition::epMCA.inference.battery(DATA = mesIris,
                                DESIGN = grIris,
                                graphs =  FALSE # TRUE first pass only
)
#_____________________________________________________________________


#_____________________________________________________________________
# Scree ----
PlotScree(ev = resMCA$ExPosition.Data$eigs)
a001a.screePlot <- recordPlot()
#_____________________________________________________________________
# I-set map ----
# a graph of the observations
iris.Imap <- PTCA4CATA::createFactorMap(
  title = 'MCA: Iris Data Set',
  resMCA$ExPosition.Data$fi,
  col.points = resMCA$Plotting.Data$fi.col,
  display.labels = FALSE,
  alpha.points = .5
)

#_____________________________________________________________________
# make labels ----
label4Map <- createxyLabels.gen(1,2,
                             lambda = resMCA$ExPosition.Data$eigs,
                             tau = resMCA$ExPosition.Data$t)
#_____________________________________________________________________
a002.Map.I <- iris.Imap$zeMap + label4Map
#_____________________________________________________________________

# The J-set
#
#_____________________________________________________________________
# J-set Map ----
#_____________________________________________________________________
# color 4 J
# create a color scheme for the variables
# Width is darker than length: Sepal is orange. Petal is red
col4Var <- c('orange','orange4','red','red4')
#_____________________________________________________________________
cJ <- resMCA$ExPosition.Data$cj
varNames <- colnames(mesIris)
 nM   <- nrow(cJ)
 nVar <- ncol(mesIris)
 col4Labels <- c(rep(col4Var[1],4), rep(col4Var[2],3),
                 rep(col4Var[3],3), rep(col4Var[4],3))
# col4Labels <- rep("",nM)
# 
# for (i in 1:nVar){
#   lindex <- varNames %in% rownames(cJ)[i]
#   col4Labels[lindex] <- col4Var[i]
# }
# 

 #```{r createFjMap}
 axis1 = 1
 axis2 = 2
 Fj <- resMCA$ExPosition.Data$fj
 # generate the set of maps
 BaseMap.Fj <- createFactorMap(X = Fj , # resMCA$ExPosition.Data$fj,
                               axis1 = axis1, axis2 = axis2,
                               title = 'MCA. Variables', 
                               col.points = col4Labels, cex = 1,
                               col.labels = col4Labels, text.cex = 2.5,
                               force = 2)
 # add labels
 labels4MCA <- createxyLabels.gen(x_axis = axis1, y_axis = axis2,
                                  lambda = resMCA$ExPosition.Data$eigs,
                                  tau = resMCA$ExPosition.Data$t)
 # make the maps
 aa.1.BaseMap.Fj <- BaseMap.Fj$zeMap + labels4MCA 
 aa.2.BaseMapNoDot.Fj  <- BaseMap.Fj$zeMap_background +
   BaseMap.Fj$zeMap_text + labels4MCA 
# ```
 
# ```{r plotaMap, fig.width= 8 , fig_width = '100%'}
 print(aa.1.BaseMap.Fj)


