#loading packages
library(ClustOfVar)

crime_assualt <- read.csv("../data/crime_assualt_data.csv")
dim(crime_assualt)

features <- crime_assualt[,6:183]
features_scaled = scale(features)

## PCA
pca <- prcomp(features, center = TRUE, scale. = TRUE)
plot(pca, type = "l")
summary(pca)



## K-means clustering
k.means <- kmeansvar(X.quanti=features_scaled, init=10)
summary(k.means)
k.means$cluster


## Hierachical clustering
hclus <- hclustvar(X.quanti=features_scaled)
summary(hclus)
stab <- stability(hclus, B=50)
plot(stab)
