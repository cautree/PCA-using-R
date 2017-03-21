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

#########################################################
#more PCA
iris_2 = iris[,1:4]
#center =TRUE is highly recommended
pr.iris_2 = prcomp(x=iris_2, scale=FALSE, center =TRUE)
summary(pr.iris_2) # explains how much each component explains the variance

pr.iris = prcomp(x=iris[-5], scale = FALSE, center=TRUE)
#biplot() function plots both the principal components loadings and the mapping of the observations 
#to their first two principal component values. 
biplot(pr.iris)

#get proportions of variance for a scree plot
pr.var = pr.iris$sdev^2
pve = pr.var/sum(pr.var)

plot(pve, xlab= "Principle Component",
     ylab="proportion of Variance explained",
     ylim=c(0,1), type="b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cummulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")


# Mean of each variable
colMeans(pokemon)

# Standard deviation of each variable
apply(pokemon, 2, sd)

# PCA model with scaling: pr.with.scaling
pr.with.scaling=prcomp(pokemon, scale = TRUE, center=TRUE)

# PCA model without scaling: pr.without.scaling
pr.without.scaling = prcomp(pokemon, scale=FALSE)

# Create biplots of both for comparison
par(mfrow = c(1, 2))
biplot(pr.with.scaling)
biplot(pr.without.scaling)