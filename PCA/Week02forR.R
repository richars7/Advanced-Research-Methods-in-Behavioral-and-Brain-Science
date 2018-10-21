## Class 02
## Author: Ju-Chi Yu
## Date: 8-27-2018
## Objectives:
##     o  read data
##     o  check the data first
##     o  create function
##     o  PCA
##     o  present results
##===========================================================
# Install packages
install.packages("devtools")
install.packages("ExPosition")
install.packages("ggplot2")
install.packages("corrplot")

# Read packages
library("devtools")
library("ExPosition")
library("ggplot2")
library("corrplot")


# Basic functions ---------------------------------------------------------
## Add
1+2
## Name it
name.it = 1+2
name.it <- 1+2
## Subtract
1-2
## Multiplication
22*3
## Division
50/4
## Build vector
a.vector <- c(2,3,4,1,42,5,6,23)
b.vector <- c(1,4,6,1,6,3,5,10)
## Add, subtract, multiply, divide
a.vector + b.vector
a.vector - b.vector
a.vector * b.vector
a.vector/b.vector
## Build matrix
a.matrix <- matrix(a.vector)
b.matrix <- matrix(b.vector)
## check dimension
dim(a.matrix)
## change dimension of a matrix
a.matrix <- matrix(a.vector, nrow = 4, ncol = 2)
b.matrix <- matrix(b.vector, nrow = 4, ncol = 2)
## multiply them
## Recall operations for matrices
a.matrix*b.matrix
a.matrix %*% t(b.matrix)
t(a.matrix) %*% b.matrix
a.matrix %o% b.matrix ## how to multiply b.matrix with each element of a.matrix


# Read data ---------------------------------------------------------------
## data frame
devtools::install_github('HerveAbdi/data4PCCAR')
library(data4PCCAR)
data("twentyWines")
twentyWines$df.active
## name it
first.data <- twentyWines$df.active

# Check the data first ----------------------------------------------------
## checking number of rows and columns
dim(first.data)
nrow(first.data)
ncol(first.data)
rownames(first.data)
colnames(first.data)
row.names(first.data)
## you can change it...with cautious
colnames(first.data) <- c("yes","no")
## find the mean
mean(first.data)
## find the standard deviation
sd(first.data)
## check the type
str(first.data)
### now...do this again
mean(first.data$Sugar)
sd(first.data$Sugar)
## use it as matrix
mean(as.matrix(first.data))
mean(as.matrix(first.data[,1]))
## summary!!
summary(first.data)
## summation of rows and columns and everything
rowSums(first.data)
colSums(first.data)
sum(first.data)
## other similar functions
rowMeans(first.data)
colMeans(first.data)
## but not...
rowSd(first.data)
colSd(first.data)
## correlation
cor(first.data)

# Create functions --------------------------------------------------------
## Let compute the sums of squares of each column, and the sum of cross product
ss.col <- colSums(first.data^2)
scp.col <- sum(first.data$Sugar*first.data$Astringent)
## Show ss and scp with one line
ssANDscp <- function(data = first.data){
  ss.col <- colSums(first.data^2)
  scp.col <- sum(first.data[,1]*first.data[,2])
  
  # return(ss.col)
  # return(scp.col)
  return(list(SS = ss.col, SCP = scp.col))
}
## Try
ssANDscp(first.data)

## Can you build a function to compute correlation?

## Plot correlation
?corrplot
corrplot(cor(first.data), method = "number")
corrplot(cor(first.data), method = "ellipse", addCoef.col = TRUE)

# Let's do PCA ------------------------------------------------------------
epPCA(first.data)
?epPCA
## Get the elements by naming the results and call them later
pca.res <- epPCA(first.data)
## Check the results
pca.res
## What "design" does?
pca.res <- epPCA(first.data,DESIGN = twentyWines$supplementary.variables$Origin)


# Plot the results --------------------------------------------------------
## With function in the package
?prettyPlot
prettyPlot(pca.res$ExPosition.Data$fi,
           col = pca.res$Plotting.Data$fi.col,
           xlab = sprintf("First component: %.2f %% of variance",pca.res$ExPosition.Data$t[1]),
           ylab = sprintf("Second component: %.2f %% of variance",pca.res$ExPosition.Data$t[2]))
## How to plot component 2 and 3
## How to plot with contribution

## correlationcircle with ExPosition
correlationPlotter(data_matrix = first.data, factor_scores = pca.res$ExPosition.Data$fi)

## With ggplot2
plot.data <- as.data.frame(pca.res$ExPosition.Data$fi)
colnames(plot.data) <- c("PC1","PC2")
fig <- ggplot(data = plot.data,
       aes(x = PC1,y = PC2))
fig + geom_point(col = pca.res$Plotting.Data$fi.col) + theme_bw()
## add names
install.packages("ggrepel")
library(ggrepel)

fig <- ggplot(data = plot.data,
              aes(x = PC1,y = PC2,label = rownames(pca.res$ExPosition.Data$fi)))

## build information for colors
color.info <- as.matrix(twentyWines$supplementary.variables$Origin,rownames = rownames(twentyWines$supplementary.variables))
## try geom_text

fig + 
  geom_point(aes(color = color.info)) +
  theme_classic() + 
  geom_text()

## try geom_repel
fig + 
  geom_point(aes(color = color.info)) +
  theme_classic() + 
  geom_text_repel(aes(label = rownames(pca.res$ExPosition.Data$fi), color = color.info),
                  size = 3) +
  scale_color_manual(values = unique(pca.res$Plotting.Data$fi.col))

## Can you try to plot correlation
