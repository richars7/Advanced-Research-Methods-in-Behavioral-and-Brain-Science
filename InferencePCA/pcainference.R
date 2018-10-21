
getwd()
setwd("/Users/savita/Desktop/Dr. Abdi //")
getwd()
my_data <- read.csv("IBM-HR-Emplyee-NoAttrition.csv")
rownames(my_data) <- my_data$Subj
head(my_data)

sapply(my_data, class)
my_data1 <- my_data[sapply(my_data, function(x) !is.factor(x))]
my_data1 <- subset(my_data1, select=-Subj)
cor.res <- cor(my_data1)
corrplot(cor.res, method = "ellipse")

#Create an empty list, called DESIGN
DESIGN <- list()
DESIGN$rows$Region$labels <- unique(my_data$Department) 
DESIGN$rows$Region$vec <- my_data$Department

#Convert the vector to a matrix
DESIGN$rows$Region$mat <- makeNominalData(as.matrix(DESIGN$rows$Region$vec))
automatic_colors <- createColorVectorsByDesign(DESIGN$rows$Region$mat)
DESIGN$rows$Region$color_groups <- c("red","yellow","black")
plot(c(1:3), pch=15, cex=4, col=DESIGN$rows$Region$color_groups)

DESIGN$rows$Region$color_observ <- as.matrix(DESIGN$rows$Region$vec) 

DESIGN$rows$Region$color_observ[which(DESIGN$rows$Region$vec=="Research & Development")]         <- DESIGN$rows$Region$color_groups[1]
DESIGN$rows$Region$color_observ[which(DESIGN$rows$Region$vec=="Human Resources")]          <- DESIGN$rows$Region$color_groups[2]
DESIGN$rows$Region$color_observ[which(DESIGN$rows$Region$vec=="Sales")]     <- DESIGN$rows$Region$color_groups[3]


res_pca <- epPCA.inference.battery(my_data1, DESIGN = NULL, make_design_nominal = TRUE)

#Scree Plot
name_the_scree <- plot(res_pca$ExPosition.Data$eigs,
                       ylab = "Eigenvalues",
                       xlab = "Components",
                       type = "l",
                       main = "Scree Plot of the IBM-HR Employee NoAttrition Dataset",
                       ylim = c(-1, max(res_pca$ExPosition.Data$eigs)))
points(res_pca$ExPosition.Data$eigs, cex = 1.5, pch = 8, col = "dark red")
points(res_pca$ExPosition.Data$eigs, cex = 1, pch = 20, col = "black")

#FACTOR SCORES
OriginAsDesignMat <- makeNominalData(as.matrix(my_data$Department))
Fi2plot <- res_pca$ExPosition.Data$fi
fi_all.sum <- t(Fi2plot) %*% OriginAsDesignMat
fi_all.count <- colSums(OriginAsDesignMat)
fi_all.mean <- t(fi_all.sum)/fi_all.count

#pretty plot between component1 and 2 
name_the_plot <- prettyPlot(data_matrix = res_pca$ExPosition.Data$fi,  
                            dev.new=FALSE,
                            main = "IBM Employees Factor Scores",
                            x_axis = 1, y_axis = 2, 
                            contributionCircles = FALSE, contributions = res_pca$ExPosition.Data$ci, 
                            display_points = TRUE, pch = 21, cex = 1.2, col = DESIGN$rows$Region$color_observ, 
                            display_names = FALSE, 
                            xlab = paste0("Component 1 Inertia: ", round(res_pca$ExPosition.Data$t[1],3), "%"),
                            ylab = paste0("Component 2 Inertia: ", round(res_pca$ExPosition.Data$t[2],3), "%")
)
legend(x="bottomright", pch = 19, legend = DESIGN$rows$Region$labels, col=DESIGN$rows$Region$color_groups)
prettyPlot(fi_all.mean, 
           col = DESIGN$rows$Region$color_groups, 
           display_names = FALSE,
           pch = 15, 
           cex = 5.0,
           dev.new = FALSE,
           new.plot = FALSE)


correlationCircle <- correlationPlotter(data_matrix = my_data1 , factor_scores =res_pca$ExPosition.Data$fi , x_axis = 1, y_axis = 2, col = NULL,pch = NULL, xlab = NULL, ylab = NULL, main = "Correlation Circle", asp = 1, dev.new = FALSE) 


name_another_plot <- prettyPlot(data_matrix = res_pca$ExPosition.Data$fj,  
                                dev.new=FALSE,
                                main = "IBM Emoployees Column Loadings",
                                x_axis = 1, y_axis = 2, 
                                contributionCircles = FALSE, contributions = res_pca$ExPosition.Data$cj, 
                                display_points = TRUE, pch = 21, cex = 1.2, col = res_pca$Plotting.Data$fj.col, 
                                display_names = TRUE, 
                                xlab = paste0("Component 1 Inertia: ", round(res_pca$ExPosition.Data$t[1],3), "%"),
                                ylab = paste0("Component 2 Inertia: ", round(res_pca$ExPosition.Data$t[2],3), "%")
)
