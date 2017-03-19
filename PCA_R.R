#
# Load data
data(iris)
head(iris, 3)

# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]

#set center and scale. equal to TRUE in the call to prcomp to 
#standardize the variables prior to the application of PCA
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,
                 center = TRUE,
                 scale. = TRUE) 

# print method
print(ir.pca)

# plot method
plot(ir.pca, type = "l")

# summary method
summary(ir.pca)


# Predict PCs
predict(ir.pca, 
        newdata=tail(log.ir, 2))

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
              groups = ir.species, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

biplot(ir.pca)

require(ggplot2)

theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
p <- ggplot(circle,aes(x,y)) + geom_path()

loadings <- data.frame(ir.pca$rotation, 
                       .names = row.names(ir.pca$rotation))
p + geom_text(data=loadings, 
              mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
  coord_fixed(ratio=1) +
  labs(x = "PC1", y = "PC2")

#PCA on caret package
install.packages("caret")
install.packages('e1071', dependencies=TRUE)
require(caret)
require(e1071)
trans = preProcess(iris[,1:4], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC = predict(trans, iris[,1:4])

# Retained PCs
head(PC, 3)
# Loadings
trans$rotation
