#####################################################################
## This code demonstrates SOM in R
## 
## Rachael Blair 
## Created: March 19, 2019
## Edited:
#####################################################################

rm(list = ls())
library(kohonen)

data(wines)
wines.scaled <- scale(wines)

# fit an SOM
set.seed(123)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
wine.som <- som(wines.scaled, grid = som_grid, rlen = 3000)

codes <- wine.som$codes[[1]]
?plot.kohonen

quartz()
plot(wine.som, main = "Wine Data")

quartz()
plot(wine.som, type = "changes", main = "Wine Data")

quartz()
plot(wine.som, type = "count")

quartz()
plot(wine.som, type = "mapping")

coolBlueHotRed <- function(n, alpha = 1){rainbow(n, end=4/6, alpha = alpha)[n:1]}

quartz()
plot(wine.som, type = "dist.neighbours", palette.name = coolBlueHotRed)

# component plane plots
for (i in 1:13){
    quartz()
    plot(wine.som, type = "property", property=codes[,i], main = colnames(codes)[i])
}

d <- dist(codes)
hc <- hclust(d)

quartz()
plot(hc)

som_cluster <- cutree(hc, h = 7)

# plot the SOM with the found clusters

my_pal <- c("red", "blue", "yellow")
my_bhcol <- my_pal[som_cluster]

graphics.off()

quartz()
plot(wine.som, type = "mapping", col = "black", bgcol = my_bhcol)
add.cluster.boundaries(wine.som, som_cluster)











