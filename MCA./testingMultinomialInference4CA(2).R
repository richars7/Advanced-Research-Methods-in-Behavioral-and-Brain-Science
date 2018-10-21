# Inferences for class 8. Fast Permutations and Bootstrap for CA
# Example on how to use the Fast
# Inference functions for CA in R
# Hervé Abdi.
# October 15, 2017. For RM3.

# Clean start
#
rm(list = ls())
graphics.off()

# This is the name of my directory where my function-file is stored
# You will have to change this name
#
dir4functions <- '~/Box Sync/RM3-2017/R-Script/Malinvaud-Class8/'
#                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#                 Change that to your own path
#
# This is the name of my directory where my R-file is stored
workingDir    <- '~/Box Sync/RM3-2017/R-Script/Malinvaud-Class8/'
#                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#                 Change that to your own path
# the functions are saved in the file: InferencesMultinom4CA.R
# If  you have used another name change this name below:
file4functions <- 'InferencesMultinom4CA.R'
#                 ^^^^^^^^^^^^^^^^^^^^^^^^^
#              Change that if you used another name



# need ExPosition
library(ExPosition)
# load the functions by "sourcing" the file
source(paste0(dir4functions,file4functions))
#

# We use the authors example
data(authors)
X <-  authors$ca$data

#--------------------------------------------------------------------
# run a plain CA with ExPosition
resCA4Authors <- epCA(X, graphs = FALSE)

#--------------------------------------------------------------------
# get the permutation test:
res4PermTest <- Perm4MultCA(X = X, nIter = 1000)
#
# To know what is in there:
print(res4PermTest)

# To have a look at the Omnibus test
print(res4PermTest$pOmnibus)
# to have a look at p(eigenvalues)
#
print(res4PermTest$pEigenvalues)

#--------------------------------------------------------------------
# Now to find the "significant eigenvalues"
# use the Malinvaud-Saporta test.
#
#
#
Res4Malinvaud <- MalinvaudQ4CA(Data = X,
                               permutedEigenValues =
                                 res4PermTest$permEigenvalues)

#
#
#--------------------------------------------------------------------
# Get the boostrap estimates
#
res4Boot <- Boot4MultCA(X = X,
                            Fi = resCA4Authors$ExPosition.Data$fi,
                            Fj = resCA4Authors$ExPosition.Data$fj,
                            delta = resCA4Authors$ExPosition.Data$pdq$Dv,
                            nf2keep = 3,
                            nIter = 1000,
                            critical.value = 2,
                            eig = TRUE,
                            alphaLevel = .05)
#
#

# To know what is in there:
print(res4Boot)


# To look at the I-set Bootratios:
print(res4Boot$bootRatios.i)
#
#










