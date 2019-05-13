#loading packages
library(ClustOfVar)

crime_assualt <- read.csv("../../data/crime_assualt_data.csv")
dim(crime_assualt)

features <- crime_assualt[,6:181]
features_scaled = scale(features)

## PCA
pca <- prcomp(features, center = TRUE, scale. = TRUE)
plot(pca, type = "l")
summary(pca)



## K-means clustering
k.means <- kmeansvar(X.quanti=features_scaled, init=20)
summary(k.means)
variableCluster <- sort(k.means$cluster)
write.csv(variableCluster, file="../../data/variable20Clusters.csv")


## Hierachical clustering
hclus <- hclustvar(X.quanti=features_scaled)
summary(hclus)
stab <- stability(hclus, B=50)
plot(stab)
