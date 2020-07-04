library("cluster")
library("bootcluster")
library("fossil")
library("caret")
setwd("C:/Users/Flynn/Desktop/SDM HW2")

######################################################################################################
########### QUESTION 1 ########## ISLR CHAPTER 10 QUESTION 9 #########################################
######################################################################################################

data("USArrests") #loading USArrests data
summary(USArrests)
sum(is.na(USArrests))

#dissimilarity matrix using eucledian distance
arrestdist <- dist(USArrests, method = "euclidean")  
dim(as.matrix(arrestdist))

#building hierarchial clustering using hclust and complete linkage method
arresthierclust <- hclust(arrestdist, method = "complete")
plot(arresthierclust, hang = -1, main = "Raw data dendrogram")

#cutting the dendogram at height = 120 to form three clusters
cut_arrest_complete <- cutree(arresthierclust, h = 120)
plot(silhouette(cut_arrest_complete, dist = arrestdist), main = "Unscaled data")

#finding what countries are present in each cluster
group1 <- (names(cut_arrest_complete[cut_arrest_complete == 1]))
group2 <- (names(cut_arrest_complete[cut_arrest_complete == 2])) 
group3 <- (names(cut_arrest_complete[cut_arrest_complete == 3]))


#scaling to one standard deviation
scaled_usarrests_data <- as.data.frame(scale(USArrests))

#dissimilarity matrix with eucledian distance for scaled data
arrestdist_scaled <- dist(scaled_usarrests_data, method = "euclidean")
dim(as.matrix(arrestdist_scaled))

#building hierarchial clustering using hclust and complete linkage method
arresthierclust_scaled <- hclust(arrestdist_scaled, method = "complete")
plot(arresthierclust_scaled, hang = -1, main = "Scaled data dendogram")

#cutting the dendogram to form three clusters
cut_arrest_complete_scaled <- cutree(arresthierclust_scaled, k = 3)
plot(silhouette(cut_arrest_complete_scaled, dist = arrestdist_scaled),main = "Scaled data")

#finding what countries are present in each cluster
group1_scaled <- (names(cut_arrest_complete_scaled[cut_arrest_complete_scaled == 1]))
group2_scaled <- (names(cut_arrest_complete_scaled[cut_arrest_complete_scaled == 2])) 
group3_scaled <- (names(cut_arrest_complete_scaled[cut_arrest_complete_scaled == 3]))


###########################################################################################################
############# QUESTION 2 ############## ISLR CHAPTER 10 Question 11 #######################################
###########################################################################################################

#reading data
q2 <- read.csv("Ch10Ex11.csv", header = F)

summary(q2)
str(q2)
sum(is.na(q2))

#dissimilarity matrix using correlation based distance

q2dist <- as.dist(1 - cor(q2)) 

dim(as.matrix(q2dist))

#applying hierarchial clustering

q2clust_single <- hclust(q2dist, method = "single")

q2clust_complete <- hclust(q2dist, method = "complete")

q2clust_average <- hclust(q2dist, method = "average")

#dev.off()

plot(q2clust_single, main = "Single linkage")
plot(q2clust_complete, main = "Complete linkage")
plot(q2clust_average, main = "Average linkage")

#applying PCA to find which genes differ from each other.

gene_pca <- prcomp(t(q2), center = T, scale = T)

loadings <- apply(gene_pca$rotation, 1, sum)

indexes <- order(abs(loadings), decreasing = T)

indexes[1:15]  #looking at top 15 genes that differ the most between two groups

######################################################################################################
########### QUESTION 3 ###############################################################################
######################################################################################################

#reading data from text file
seeds <- read.table("seeds.txt", header = T, sep = "\t")
summary(seeds)
sum(is.na(seeds))

#removing the original reference variable
seedsdata <- seeds[,-ncol(seeds)]
summary(seedsdata)


#################################
## Hierarichaial clustering
#################################


#dissimilarity matrix using eucledian distance
distancebwpoints <- dist(seedsdata, method = "euclidean") 
dim(as.matrix(distancebwpoints))

#Hclustering using single linkage
hclustering_single <- hclust(distancebwpoints, method = "single") 

#Hclustering using average linkage
hclustering_average <- hclust(distancebwpoints, method = "average") 

#Hclustering using complete linkage
hclustering_complete <- hclust(distancebwpoints, method = "complete") 

#Dendogram for single average and complete
plot(hclustering_single, hang = -1, main = "Single linkage")

plot(hclustering_average, hang = -1, main = "Average linkage")  

plot(hclustering_complete, hang = -1, main = "Complete linkage")  

#bootstrapping cluster stability to estimate number of clusters
k.select(seedsdata, range = 2:10, B = 100, r = 20, scheme_2 = T)
k.select(seedsdata, range = 2:10, B = 100, r = 20, scheme_2 = F)

######## we can see the optimal cluster as 3
######## Hierarcial clustering using 3 clusters

cut_single_3clust <- cutree(hclustering_single, k = 3)
cut_average_3clust <- cutree(hclustering_average, k = 3)
cut_complete_3clust <- cutree(hclustering_complete, k = 3)

#plotting the silhoutte of all three methods
plot(silhouette(cut_single_3clust, dist = distancebwpoints), main = "Single linkage")
plot(silhouette(cut_average_3clust, dist = distancebwpoints), main = "Average linkage")
plot(silhouette(cut_complete_3clust, dist = distancebwpoints), main = "Complete linkage")

# Evaluating perfomance using confusion matrix from caret package
confusionMatrix(as.factor(cut_single_3clust) ,as.factor(as.numeric(seeds$Seed.Group)))
confusionMatrix(as.factor(cut_average_3clust) ,as.factor(as.numeric(seeds$Seed.Group)))
confusionMatrix(as.factor(cut_complete_3clust) ,as.factor(as.numeric(seeds$Seed.Group)))

# evaluating the perfomance using rand and adjusted rand index
#single linkage
rand.index(cut_single_3clust, as.numeric(seeds$Seed.Group))
adj.rand.index(cut_single_3clust, as.numeric(seeds$Seed.Group))

#average linkage
rand.index(cut_average_3clust, as.numeric(seeds$Seed.Group))
adj.rand.index(cut_average_3clust, as.numeric(seeds$Seed.Group))

#complete linkage
rand.index(cut_complete_3clust, as.numeric(seeds$Seed.Group))
adj.rand.index(cut_complete_3clust, as.numeric(seeds$Seed.Group))

################################
## K means clustering                     
################################

#bootstrapping cluster stability to estimate number of clusters
k.select(seedsdata, range = 2:10, B = 100, r = 20, scheme_2 = T)
k.select(seedsdata, range = 2:10, B = 100, r = 20, scheme_2 = F)

#estimating optimal clusters using gap statistic
gap_kmeans <- clusGap(seedsdata, kmeans, nstart = 20, K.max = 10, B = 200)
plot(gap_kmeans, main = "Gap statistic")

#we find that the optimal number of clusters is 3 from both the methods (bootstrapping and gap statistic)

#applying k means clustering with optimal number of clusters
set.seed(12324)
kmeansclustering <- kmeans(seedsdata, centers = 3, nstart = 30)

#confusion matrix
confusionMatrix(as.factor(as.numeric(seeds$Seed.Group)), as.factor(kmeansclustering$cluster))

#evaluating perfomance
rand.index(kmeansclustering$cluster, as.numeric(seeds$Seed.Group))

adj.rand.index(kmeansclustering$cluster, as.numeric(seeds$Seed.Group))

plot(silhouette(kmeansclustering$cluster, dist = distancebwpoints), main = "K-Means")

