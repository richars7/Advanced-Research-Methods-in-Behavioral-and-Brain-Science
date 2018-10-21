
getwd()
setwd("/Users/savita/Desktop/Dr. Abdi //")
getwd()
library(readxl)
my_data <- read.csv("IBM-HR-Emplyee-NoAttrition.csv")
my_data <- as.data.frame(my_data)
head(my_data)
rownames(my_data) <- my_data$Subj
#my_data$Attrition <- as.numeric(as.factor(my_data$Attrition))
#my_data$BusinessTravel <- as.numeric(as.factor(my_data$BusinessTravel))
#my_data$Department <- as.numeric(as.factor(my_data$Department))
#my_data$EducationField <- as.numeric(as.factor(my_data$EducationField))
#my_data$Gender <- as.numeric(as.factor(my_data$Gender))
#my_data$JobRole <- as.numeric(as.factor(my_data$JobRole))
#my_data$MaritalStatus <- as.numeric(as.factor(my_data$MaritalStatus))
#my_data$OverTime <- as.numeric(as.factor(my_data$OverTime))
head(my_data)

sapply(my_data, class)
my_data1 <- my_data[sapply(my_data, function(x) !is.factor(x))]

cor.res <- cor(my_data)
corrplot(cor.res, method = "circle", na.label = "square", na.label.col = "orange")

#sum(is.na(my_data))
#sum(is.nan((my_data)))

#is.nan.data.frame <- function(x)
#do.call(cbind, lapply(x, is.nan))
#sum(is.nan(my_data))
#sum(is.nan(as.matrix(my_data)))

#any( !is.finite( as.matrix( my_data ) ) )



res_pca <- epPCA(my_data1, center = TRUE, scale = TRUE, DESIGN = my_data$Education , graphs = FALSE)

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
OriginAsDesignMat <- makeNominalData(as.matrix(my_data$JobInvolvement))
NewColors <- createColorVectorsByDesign(OriginAsDesignMat, hsv = FALSE, offset = 25)
rownames(NewColors$gc) <- c("1","2","3","4","5")

Fi2plot <- res_pca$ExPosition.Data$fi
Fi2plot
colnames(OriginAsDesignMat) <- c("1","2","3","4","5")
fi_all.sum <- t(Fi2plot) %*% OriginAsDesignMat
fi_all.count <- colSums(OriginAsDesignMat)
fi_all.mean <- t(fi_all.sum)/fi_all.count

#pretty plot between component1 and 2 
name_the_plot <- prettyPlot(data_matrix = res_pca$ExPosition.Data$fi,  
                            dev.new=FALSE,
                            main = "IBM Employees Row Factor Scores",
                            x_axis = 1, y_axis = 2, 
                            contributionCircles = FALSE, 
                            contributions = res_pca$ExPosition.Data$ci, 
                            display_points = TRUE, pch = 21, cex = 2.0, 
                            col = NewColors$oc,
                            fg.line.width = 2,
                            bg.line.width= 3,
                            display_names = FALSE,
                            xlab = paste0("Component 1 Inertia: ", round(res_pca$ExPosition.Data$t[1],3), "%"),
                            ylab = paste0("Component 2 Inertia: ", round(res_pca$ExPosition.Data$t[2],3), "%"))
prettyPlot(fi_all.mean, # Check the shape of this, and see how it's related to the fi in PCA results
           col = NewColors$gc, # color for each group of rows
           display_names = TRUE,
           pch = 15, # Google pch
           cex = 5.0,
           dev.new = FALSE,
           new.plot = FALSE)
legend("bottomright", pch = 19, legend = rownames(NewColors$gc), 
       col = NewColors$gc, text.col = NewColors$gc)




#pretty plot between component2 and 3
name_the_plot <- prettyPlot(data_matrix = res_pca$ExPosition.Data$fi,  
                            dev.new=FALSE,
                            main = "IBM Employees Row Factor Scores",
                            x_axis = 2, y_axis = 3, 
                            contributionCircles = FALSE, 
                            contributions = res_pca$ExPosition.Data$ci, 
                            display_points = TRUE, pch = 21, cex = 2.0, 
                            col = NewColors$oc,
                            fg.line.width = 2,
                            bg.line.width= 3,
                            display_names = FALSE,
                            xlab = paste0("Component 1 Inertia: ", round(res_pca$ExPosition.Data$t[1],3), "%"),
                            ylab = paste0("Component 2 Inertia: ", round(res_pca$ExPosition.Data$t[2],3), "%"))
legend("bottomright", pch = 19, legend = rownames(NewColors$gc), 
       col = NewColors$gc, text.col = NewColors$gc)



correlationCircle <- correlationPlotter(data_matrix = my_data , factor_scores =res_pca$ExPosition.Data$fi , x_axis = 1, y_axis = 2, col = NULL,pch = NULL, xlab = NULL, ylab = NULL, main = "Correlation Circle", asp = 1, dev.new = FALSE) 


OriginAsDesignMat1 <- makeNominalData(as.matrix(my_data$Department))
NewColors1 <- createColorVectorsByDesign(OriginAsDesignMat1, hsv = FALSE, offset = 15)
rownames(NewColors1$gc) <- c("Sales","Human Resources", "Research & Development")

Fi2plot1 <- res_pca$ExPosition.Data$fi
Fi2plot1
colnames(OriginAsDesignMat1) <- c("Sales","Human Resources", "Research & Development")
fi_all.sum1 <- t(Fi2plot1) %*% OriginAsDesignMat1
fi_all.count1 <- colSums(OriginAsDesignMat1)
fi_all.mean1 <- t(fi_all.sum1)/fi_all.count1


name_another_plot <- prettyPlot(data_matrix = res_pca$ExPosition.Data$fj,  
                                dev.new=FALSE,
                                main = "IBM Emoployees Column Loadings",
                                x_axis = 1, y_axis = 2, 
                                contributionCircles = FALSE, 
                                contributions = res_pca$ExPosition.Data$cj, 
                                display_points = TRUE, pch = 21, cex = 2.0, 
                                col = res_pca$Plotting.Data$fj.col, 
                                display_names = TRUE, 
                                fg.line.width = 2,
                                bg.line.width= 3,
                                xlab = paste0("Component 1 Inertia: ", round(res_pca$ExPosition.Data$t[1],3), "%"),
                                ylab = paste0("Component 2 Inertia: ", round(res_pca$ExPosition.Data$t[2],3), "%")
)


name_the_plot <- prettyPlot(data_matrix = res_pca$ExPosition.Data$fi,  
                            dev.new=FALSE,
                            main = "IBM Employees Row Factor Scores",
                            x_axis = 1, y_axis = 2, 
                            contributionCircles = FALSE, 
                            contributions = res_pca$ExPosition.Data$ci, 
                            display_points = TRUE, pch = 21, cex = 2.0, 
                            col = NewColors1$oc,
                            fg.line.width = 2,
                            bg.line.width= 3,
                            display_names = FALSE,
                            xlab = paste0("Component 1 Inertia: ", round(res_pca$ExPosition.Data$t[1],3), "%"),
                            ylab = paste0("Component 2 Inertia: ", round(res_pca$ExPosition.Data$t[2],3), "%"))

prettyPlot(fi_all.mean1, # Check the shape of this, and see how it's related to the fi in PCA results
           col = NewColors1$gc, # color for each group of rows
           display_names = FALSE,
           pch = 15, # Google pch
           cex = 3.0,
           dev.new = FALSE,
           new.plot = FALSE)
legend("bottomright", pch = 19, legend = rownames(NewColors1$gc), 
       col = NewColors1$gc, text.col = NewColors1$gc)
