######################################################################################################################
############################################ ABINESH SENTHIL KUMAR ###################################################
################################################### 50320934 #########################################################

library(ElemStatLearn)

#loading nci data
data(nci)

#install.packages("kohonen")
library(kohonen)

ncidata <- nci

#plotting the boxplot of the variables we can see the scale of each variable
boxplot(ncidata)

#running som on the whole data
set.seed(1100)

som.grid <- somgrid(xdim = 8, ydim = 8, topo = "hexagonal" ) 

nci_som <- som(ncidata, grid = som.grid, rlen = 3000)

#SOM plots
plot(nci_som)
plot(nci_som, type = "counts")
plot(nci_som, type = "mapping")
plot(nci_som, type = "changes")

#creating new colour palette and plotting the neighbourhood distance plot
coolblue <- function(n, alpha = 1){rainbow(n, end = 4/6, alpha = alpha)[n:1]}
plot(nci_som, type = "dist.neighbours", palette.name = coolblue )

#getting the codes from the som
codes1 <- as.data.frame(nci_som$codes[[1]])

#since we want to cluster based on the variables we transpose the codes and calculate the distance
clustdist <- dist(t(codes1))

#performing hierarichial clustering using complete linkage
hclus <- hclust(clustdist, method = "complete")

#plotting the hierarichial clustering
plot(hclus)

#cutting the dendrogram to from three clusters
som_cluster <- cutree(hclus, k = 3)

# plot the SOM with the found clusters

my_palette <- c("green", "red", "yellow")
my_bgcol <- my_palette[som_cluster]


plot(nci_som, type = "mapping", col = "black", bgcol = my_bgcol)
add.cluster.boundaries(nci_som, som_cluster)


######################################################################################################################
################################################# QUESTION 2 #########################################################
library(ISLR)

#loading usarrests data
data("USArrests")

usarrestdata <- USArrests

str(usarrestdata)

#scaling data
scaled.data <- scale(usarrestdata)

#fitting hierarchial clustering with complete linkage and eucledian distance
hclustdist <- dist(scaled.data, method = "euclidean" )

hcluster <- hclust(hclustdist, method = "complete")

#plottig the dendrogram
plot(hcluster, hang = -1)

#cutting the dendrogram to find three clusters
threeclusters <- cutree(hcluster, k = 3)

library(cluster)
plot(silhouette(threeclusters, dist = hclustdist), main = "Three clusters")
#From the silhoutte plot we can see that having three clusters is not stable

#cutting the dendrogram at height 5 to get two clusters because it doesnt make sense to get three clusters from the dendrogram
twoclusters <- cutree(hcluster, h = 5)

#silhoutte plot
plot(silhouette(twoclusters, dist = hclustdist), main = "Two clusters")


#som with grid size 7 since we cant go beyond seven prototypes and it will become more than the available data 
library(kohonen)

set.seed(12)
sevengrid <- somgrid(xdim = 7, ydim = 7, topo = "hexagonal")
somdataseven <- som(as.matrix(scaled.data), grid = sevengrid, rlen = 3000)

#visualizing for seven grid som

plot(somdataseven, type = "codes")
plot(somdataseven, type = "changes")
plot(somdataseven, type = "mapping")
plot(somdataseven, type = "counts")  #we can see a lot of empty prototypes so we reduce the grid size

#som with grid size six
set.seed(1)
sixgrid <- somgrid(xdim = 6, ydim = 6, topo = "hexagonal")
somdatasix <- som(as.matrix(scaled.data), grid = sixgrid, rlen = 3000)

plot(somdatasix, type = "codes")
plot(somdatasix, type = "changes")
plot(somdatasix, type = "mapping")
plot(somdatasix, type = "counts")  #we still see empty prototypes so we reduce the grid size

#som with grid size five
set.seed(11)
fivegrid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
somdatafive <- som(as.matrix(scaled.data), grid = fivegrid, rlen = 3000)

plot(somdatafive, type = "codes")
plot(somdatafive, type = "changes")
plot(somdatafive, type = "mapping")
plot(somdatafive, type = "counts")

#creating colour palette for visualization
coolblue <- function(n, alpha = 1){rainbow(n, end = 4/6, alpha = alpha)[n:1]}

plot(somdatafive, type = "dist.neighbours", palette.name = coolblue )

codes <- somdatafive$codes[[1]]

par(mfrow = c(2,2))

for (i in 1:4){
  #  quartz()
  plot(somdatafive, type = "property", property=codes[,i], main = colnames(codes)[i])
}

dev.off()

#which countries belong to which prototypes
rownames(usarrestdata)
somdatafive$unit.classif

#calculating eucledian distance for hierarchial clustering 
distance <- dist(codes)

#hierarchical clustering usnig complete linkage
hclusting <- hclust(distance, method = "complete")

#plotting the dendrogram
plot(hclusting, hang = -1)

#cutting the tree at height 5 to result in two clusters
som_clusty <- cutree(hclusting, h = 5)

# plot the SOM with the found clusters
my_palette1 <- c("red", "green")
my_bgcol1 <- my_palette1[som_clusty]

#plotting the mapping of som with respect to found clusters
plot(somdatafive, type = "mapping", col = "black", bgcol = my_bgcol1)
#adding cluster boundaries
add.cluster.boundaries(somdatafive, som_clusty)

#####################################################################################################################
############################################### QUESTION 3 ##########################################################

swiss <- SwissBankNotes

str(swiss)
sum(is.na(swiss))

#scaling the data
scaledswiss <- as.data.frame(scale(swiss))

#creating reference for original (0) and counterfeit vaiables (1)
ref <- data.frame(1, 1:100)
ref2 <- data.frame(2, 1:100)

c <- rbind(ref$X1,ref2$X2)
c <- as.factor(sort(c))

#merging the data whether the notes are fake or genuine
scaledswiss$type <- c
scaledswiss$type <- ifelse(scaledswiss$type == 1,"Genuine","Fake")

#running principal component analysis on combined data
princip <- prcomp(scaledswiss[,1:6], center = FALSE, scale = FALSE)

#plotting the variance explained plot of all the principal components.
plot(princip, col = "red", main = "Principal components plot")

#finding the percent of variation contributed by each principal component from the summary.
summary(princip)

#getting all the six  principal components 
loading <- as.data.frame(princip$x)

dev.off()
par(mfrow = c(1,2))

#plotting the first four principal components on scatterplot
plot(loading$PC1, loading$PC2, col = as.factor(c), main = "Scatterplot of first two Principal components")
plot(loading$PC3, loading$PC4, col = as.factor(c), main = "Scatterplot of 3rd and 4th Principal components")

dev.off()
#biplot of the first two principal components
biplot(princip, main = "Biplot of the PC1 and PC2")

library(ggfortify)
autoplot(princip,data = scaledswiss, colour = "type", shape= "type", loadings.colour='black', loadings.label = TRUE,loadings.label.colour = "black", loadings.label.size = 4,main='Biplot of first two principal components')


dev.off()
############################################
#PCA for top 100 bank notes (genuine)

genuine <- swiss[1:100,]

scaledgenuine <- as.data.frame(scale(genuine))

#running pca on genuine notes data
principgenuine <- prcomp(scaledgenuine, center = F, scale = F)

#plotting the variance explained by pcs
plot(principgenuine, main = "variance explained by principal components in original notes", col = "red")

#finding the percent of variation contributed by each principal component from the summary.
summary(principgenuine)

#getting all the six principal components
loadinggenuine <- as.data.frame(principgenuine$x)

par(mfrow = c(1,2))
#scatterplot of first two principal componnets
plot(loadinggenuine$PC1, loadinggenuine$PC2, col = "red", main = "scatterplot of first two principal components")
plot(loadinggenuine$PC3, loadinggenuine$PC4, col = "red", main = "Scatterplot of 3rd and 4th Principal components")

dev.off()
#biplot of first two principal components
biplot(principgenuine, main = "genuine")

###########################################
#pca for last 100 notes (fake)

fake <- swiss[101:200,]

scaledfake <- as.data.frame(scale(fake))

#running pca on fake data
principfake <- prcomp(scaledfake, center = F, scale = F)

#plotting variance of all pcs
plot(principfake, main = "variance explained by principal components in fake notes", col = "red")

#getting the percentage of variance contribution by each principal component from summary
summary(principfake)

#getting all six principal component values
loadingfake <- as.data.frame(principfake$x)

par(mfrow = c(1,2))
#scatterplot of first four principal components
plot(loadingfake$PC1, loadingfake$PC2, col = "blue", main = "Scatterplot of first two PCs")
plot(loadingfake$PC3, loadingfake$PC4, col = "blue", main = "Scatterplot of third and fourth PCs")

dev.off()
#biplot of first two principal component values
biplot(principfake, main = "fake")

#####################################################################################################################

