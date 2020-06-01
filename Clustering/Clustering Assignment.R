# -- Airlines --

library(readxl)
airlines <- read_excel(choose.files(), sheet = "data") # Importing Airlines Data in sheet data
View(airlines)
colnames(airlines)
airlines_input <- airlines[,2:12] # No need for 1st column during Clustering

airline_norm <- scale(airlines_input) # Normalizing Data

# Hierarchical  Clustering

# Euclidean Distance
distance_euclidean <- dist(airline_norm, method="euclidean") # Euclidean Distance Matrix
str(distance_euclidean) # Structure of the Euclidean Distance Matrix
 
  # Single Linkage
  single_cluster <- hclust(distance_euclidean, method = "single") 
  airline_grouping <- cutree(single_cluster, k=5)
  airlines_new <- cbind(airlines, airline_grouping)
  aggregate(airlines_new[,2:12], by = list(airlines_new$airline_grouping), FUN = mean)

  # Complete Linkage
  complete_cluster <- hclust(distance_euclidean, method = "complete")
  airline_grouping <- cutree(complete_cluster, k=5)
  airlines_new <- cbind(airlines, airline_grouping)
  aggregate(airlines_new[,2:12], by = list(airlines_new$airline_grouping), FUN = mean)
  
  # Average Linkage
  average_cluster <- hclust(distance_euclidean, method = "average")
  airline_grouping <- cutree(average_cluster, k=5)
  airlines_new <- cbind(airlines, airline_grouping)
  aggregate(airlines_new[,2:12], by = list(airlines_new$airline_grouping), FUN = mean)
  

# Manhattan Distance
distance_manhattan <- dist(airline_norm, method="manhattan") # Manhatten Distance Matrix
str(distance_manhattan) # Structure of the Manhatten Distance Matrix
  
  # Single Linkage
  single_cluster <- hclust(distance_manhattan, method = "single")
  airline_grouping <- cutree(single_cluster, k=5)
  airlines_new <- cbind(airlines, airline_grouping)
  aggregate(airlines_new[,2:12], by = list(airlines_new$airline_grouping), FUN = mean)
  
  # Complete Linkage
  complete_cluster <- hclust(distance_manhattan, method = "complete")
  airline_grouping <- cutree(complete_cluster, k=5)
  airlines_new <- cbind(airlines, airline_grouping)
  aggregate(airlines_new[,2:12], by = list(airlines_new$airline_grouping), FUN = mean)
  
  # Average Linkage
  average_cluster <- hclust(distance_manhattan, method = "average")
  airline_grouping <- cutree(average_cluster, k=5)
  airlines_new <- cbind(airlines, airline_grouping)
  aggregate(airlines_new[,2:12], by = list(airlines_new$airline_grouping), FUN = mean)
  
  
# K Means Clustering
  
wss <- (nrow(airline_norm) - 1) * sum(apply(airline_norm, 2, var)) # Within Sum of Squares value
for(i in 2:15) wss[i] <- sum(kmeans(airlines, centers = i)$withinss) 
plot(1:15, wss, type = "b", xlab = "No. of Clusters", ylab = "Avg Distance") # Plotting graph to find optimal K value
  # Plot forms a elbow curve at 5, Thus K value equals 5.

library(animation)
km <- kmeans.ani(airline_norm, 5) # Clustering Animation
km$centers


# -- Crime Data --

crimes <- read.csv(choose.files()) # Importing Data
View(crimes)
crimes <- crimes[,2:5] # No need for 1st Column during Clustering

crimes_norm <- scale(crimes)# Normalizing Data

# Hierarchical  Clustering

# Euclidean Distance
distance_euclidean <- dist(crimes_norm, method="euclidean") # Euclidean Distance Matrix
str(distance_euclidean) # Sturcture of the Euclidean Distance Matrix

  # Single Linkage
  single_cluster <- hclust(distance_euclidean, method = "single")
  crimes_grouping <- cutree(single_cluster, k=5)
  crimes_new <- cbind(crimes, crimes_grouping)
  aggregate(crimes_new, by = list(crimes_new$crimes_grouping), FUN = mean)
  
  # Complete Linkage
  complete_cluster <- hclust(distance_euclidean, method = "complete")
  crimes_grouping <- cutree(complete_cluster, k=5)
  crimes_new <- cbind(crimes, crimes_grouping)
  aggregate(crimes_new, by = list(crimes_new$crimes_grouping), FUN = mean)
  
  # Average Linkage
  average_cluster <- hclust(distance_euclidean, method = "average")
  crimes_grouping <- cutree(average_cluster, k=5)
  crimes_new <- cbind(crimes, crimes_grouping)
  aggregate(crimes_new, by = list(crimes_new$crimes_grouping), FUN = mean)


# Manhattan Distance
distance_manhattan <- dist(crimes_norm, method="manhattan") # Manhatten Distance Matrix
str(distance_manhattan) # Structure of the Manhatten Distance Matrix

  # Single Linkage
  single_cluster <- hclust(distance_manhattan, method = "single")
  crimes_grouping <- cutree(single_cluster, k=5)
  crimes_new <- cbind(crimes, crimes_grouping)
  aggregate(crimes_new, by = list(crimes_new$crimes_grouping), FUN = mean)
  
  # Complete Linkage
  complete_cluster <- hclust(distance_manhattan, method = "complete")
  crimes_grouping <- cutree(complete_cluster, k=5)
  crimes_new <- cbind(crimes, crimes_grouping)
  aggregate(crimes_new, by = list(crimes_new$crimes_grouping), FUN = mean)
  
  # Average Linkage
  average_cluster <- hclust(distance_manhattan, method = "average")
  crimes_grouping <- cutree(average_cluster, k=5)
  crimes_new <- cbind(crimes, crimes_grouping)
  aggregate(crimes_new, by = list(crimes_new$crimes_grouping), FUN = mean)

