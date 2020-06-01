# -- Wine Dataset --

wines <- read.csv(choose.files()) # Importing Wines Data
View(wines)

colnames(wines) # Column Names
attach(wines)

library(DataExplorer)
plot_str(wines) # Structure of the data
plot_histogram(wines) # Histogram
plot_density(wines) # Density Plot
plot_correlation(wines) # Correlation Plot

cor(wines) # Correlation between columns

wine_pca <- princomp(wines, cor = TRUE, scores = TRUE, covmat = NULL) # Principal Component Function
summary(wine_pca) # Summary
loadings(wine_pca)
plot(wine_pca) # Vizualization

plot(cumsum(wine_pca$sdev * wine_pca$sdev) *100 / (sum(wine_pca$sdev * wine_pca$sdev)),type="b")
wines <- cbind(wines,wine_pca$scores[,1:3]) # Adding new Comp columns
View(wines)

comp_data <- wines[,15:17] # Selecting only Comp Data and storing it into a variable


comp_norm <- scale(comp_data) # Normalization of comp data

# Hierarchical Clustering
dist_euclidean <- dist(comp_norm, method = "euclidean") # Computing Euclidean Distance Matrix
str(dist_euclidean)

complete_cluster <- hclust(dist_euclidean, method = "complete") # Calculating the clusters using complete linkage
plot(complete_cluster, hang = 1) # Dendogram
wines_grouping <- cutree(complete_cluster, k=3) # Cutting dendogram with k value 3
rect.hclust(complete_cluster, k=3, border="red") # Vizualization
wines_new <- cbind(wines, wines_grouping) # Adding Group value column
View(wines_new)
View(aggregate(wines_new[,-c(1,15:17)], by = list(wines_new$wines_grouping), FUN = mean))

# K Means Clustering
wss <- (nrow(comp_norm) - 1) * sum(apply(comp_norm, 2, var)) # Within Sum of Squares value
for(i in 2:15) wss[i] <- sum(kmeans(wines, centers = i)$withinss) 
plot(1:15, wss, type = "b", xlab = "No. of Clusters", ylab = "Avg Distance") # Plotting graph to find optimal K value
# Plot forms a elbow curve at 3, Thus K value equals 3.
library(animation)
km <- kmeans.ani(comp_norm, 3) # Clustering Animation
