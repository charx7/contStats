# Clear the workspace
rm(list=ls())
cat("\014") 

# Set random seed
set.seed(42)

# Load the MASS dataset
library(MASS)
library(stats)
library(devtools)

iris = data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]), Sp = rep(c("s","c","v"), rep(50,3)))


# Get the indexes of a training subset
train = sample(1:150, 75)

# Get the variables on a table format
irisTable = table(iris$Sp[train])

# Perform a lda on the data set
# the . means we use all the remaining values as covariates of the model
ldaModel = lda(Sp ~ ., iris, prior = c(1,1,1)/3, subset = train)

ldaModel
ldaModel$svd

# What is the proportion of between-group variance explained by each linear disc
prop = ldaModel$svd^2/sum(ldaModel$svd^2)
prop

############## MODEL EVALUATION ##########################################
# use the predict unction on everything that is not on the training subset
predictedSeries = predict(ldaModel, iris[-train, ])$class
realSeries      = iris[-train, ]$Sp
# Make a table to compare the results of the classification
confusion_table <- table(realSeries, predictedSeries)
diag(prop.table(confusion_table, 1))
# total percent correct
sum(diag(prop.table(confusion_table)))

plot(ldaModel) # fit from lda

################# Now into PCA ###########################################
# Grab everything but the reponse variable from the dataset
pcaData = iris[, 1:4]

# Perform PCA
pcaData.pca = prcomp(pcaData, center = TRUE, scale. = TRUE)
# Print Results
print(pcaData.pca)
summary(pcaData.pca)
# As we can see the PC1 and PC2 are sufficient to explain 95% of the variance
biplot(pcaData.pca)

# Nicer plots using the ggbiplot package
library(ggbiplot)
# Select the response variable
pcaData.species = iris[, 5]
# Plot
g = ggbiplot(pcaData.pca, obs.scale = 1, var.scale = 1, groups = pcaData.species, ellipse = TRUE, circle = TRUE)
g = g + scale_color_discrete(name = '')
g = g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)






