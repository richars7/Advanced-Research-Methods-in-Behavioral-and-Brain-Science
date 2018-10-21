library(InPosition)
library(corrplot)

state.x77
state.region

# plot correlation
corrplot(cor(state.x77), method = "ellipse")

# do inference PCA
res <- epPCA.inference.battery(state.x77, DESIGN = state.region, make_design_nominal = TRUE)

