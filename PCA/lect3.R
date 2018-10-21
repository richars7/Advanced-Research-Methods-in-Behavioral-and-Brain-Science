getwd()

setwd("/Users/savita/Desktop/Dr. Abdi /")

getwd()

# Class 3
# September 10, 2018
# 

devtools::install_github('HerveAbdi/data4PCCAR')
data("twentyWines")
df <- twentyWines$df.active

resPCA <- epPCA(df,scale=TRUE,center = TRUE, DESIGN = df$Sugar,graphs = TRUE)
# CI IS THE CONTIRBUTION 