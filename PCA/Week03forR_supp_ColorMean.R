#=========================
# Supplementary materials 
#=========================
# Author: Ju-Chi Yu
# Date: 9-12-2018
# Objecitves:
#     o How to create color from a vector of categorical data in ExPosition?
#     o How to create a design matrix from a vector of categoriacl data?
#     o How to compute the mean of a group.
#===========================================
# install.packages('devtools')
# devtools::install_github('HerveAbdi/data4PCCAR')
# Make sure that you have the very last version of PTCA4CATA => re-install it
# devtools::install_github('HerveAbdi/PTCA4CATA')
# You can comment Line 13 after you installed the package
library(ExPosition)
library(data4PCCAR)

### Example data______________________________________________________
ExpData <- data('twentyWines')
data('twentyWines')
df <- twentyWines$df.active
colnames(df) <- c("sweet","astringent")
#_____________________________________________________________________
# compute PCA ----

resPCA <-  epPCA(df, scale = FALSE,
                 DESIGN = twentyWines$supplementary.variables$Origin,
                 graph = FALSE) # graphs for your eyes only

#_____________________________________________________________________
### Create color from a vector of categorical information
#***Note***: The way of doing this with PTCA4CATA is in the code for
#            Week03 class. Here, I present the way of doing it in
#            ExPosition.

## First, find the information you want to plot with
twentyWines$supplementary.variables$Origin
# Then, use the function 'makeNominalData' to create the design matrix
# It has to be a matrix.
OriginAsDesignMat <- makeNominalData(as.matrix(twentyWines$supplementary.variables$Origin))
# Take a look at it
OriginAsDesignMat

## Next, use the function 'createColorVectorsByDesign' to create a color vector
NewColors <- createColorVectorsByDesign(OriginAsDesignMat, hsv = FALSE, offset = 20)
# This is the color for each row (observations)
NewColors$oc
# This is the color for each group of the rows 
NewColors$gc
rownames(NewColors$gc) <- c("France","USA")

## Now, Let's put it into prettyPlot_________________________________
fig <- prettyPlot(resPCA$ExPosition.Data$fi,
           col = NewColors$oc,
           display_names = TRUE
             )
legend("bottomright", pch = 19, legend = rownames(NewColors$gc), 
       col = NewColors$gc, text.col = NewColors$gc)
#____________________________________________________________________
### Compute the mean of each group
#***Note***: Recall matrix multiplication

## First, find the information you want to compute your means. E.g.,
## the factor scores
Fi2plot <- resPCA$ExPosition.Data$fi
# I change the column names to show the results in a clearer way
colnames(Fi2plot) <- c("Component1", "Component2") 
Fi2plot
## Then find the group design matrix you want to compute means
## accordingly.
OriginAsDesignMat
colnames(OriginAsDesignMat) <- c("France","USA")
## If you multiply your first column of factor scores by this design
## matrix...
#***Recall %*% is matrix multiplication
fi_1.sum <- t(Fi2plot[,1]) %*% OriginAsDesignMat 
fi_1.sum
# This is the sum of the first factor scores for both groups
# To compute the mean, you need to divide these sums by the counts 
# for each group.
fi_1.count <- colSums(OriginAsDesignMat)
fi_1.count
# Now compute the mean factor scores
fi_1.mean <- fi_1.sum/fi_1.count
t(fi_1.mean)

## You can do this for all factor scores together
fi_all.sum <- t(Fi2plot) %*% OriginAsDesignMat
fi_all.sum
fi_all.count <- colSums(OriginAsDesignMat)
fi_all.count
fi_all.mean <- t(fi_all.sum)/fi_all.count # remember to transpose the fi_all.sum first
fi_all.mean

## Now, let's plot it
fig <- prettyPlot(resPCA$ExPosition.Data$fi,
                  col = NewColors$oc, # color for each row
                  display_names = FALSE
) # Here, maybe we don't want to plot the names
prettyPlot(fi_all.mean, # Check the shape of this, and see how it's related to the fi in PCA results
           col = NewColors$gc, # color for each group of rows
           display_names = TRUE,
           pch = 22, # Google pch
           cex = 1.5,
           dev.new = FALSE,
           new.plot = FALSE) # Remember to set dev.new and new.plot to FALSE to 
                             # add to the same plot.
legend("bottomright", pch = 19, legend = rownames(NewColors$gc), 
       col = NewColors$gc, text.col = NewColors$gc)
