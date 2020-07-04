#######################################
# This code will: grow, and prune, 
# Classification and # Regression trees
# Rachael Blair 
# Created: October 22, 2014
# Edited:
#######################################

rm(list=ls())
setwd("~/Desktop/STA_545_Fall2014/Comp_Labs/trees")

library("rpart") #install.packages("rpart")
library("MMST")

data(iris)
model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
fit.iris <- rpart(type~., data = iris, method = "class", control = model.control)

x11()
plot(fit.iris, uniform = T, compress = T)
text(fit.iris, cex = 1)

x11()
plot(fit.iris, uniform = T, compress = T)
text(fit.iris, use.n = T, all = T, cex = 1)

x11()
plot(fit.iris, branch = .4, uniform = T, compress = T)
text(fit.iris, use.n = T, all = T, cex = 1)

#############################################
## grow a classification tree and prune it.
#############################################
load("digging_data.RData") #dig_dats
model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
fit.dig <- rpart(Y~., data = dig_dats, method = "class", control = model.control)

x11()
plot(fit.dig$cptable[,4], main = "Cp for model selection", ylab = "cv error")

min_cp = which.min(fit.dig$cptable[,4])
pruned_fit_dig <- prune(fit.dig, cp = fit.dig$cptable[min_cp,1])

## plot the full tree and the pruned tree
x11()
plot(pruned_fit_dig, branch = .3, compress=T, main = "Pruned Tree")
text(pruned_fit_dig, cex = .5)

x11()
plot(fit.dig, branch = .3, compress=T, main = "Full Tree")
text(fit.dig, cex = .5)

#############################
### Grow regression trees
#############################
data(boston)
model.controls <- rpart.control(minbucket = 2, minsplit = 4, xval = 10, cp = 0)
fit_boston <- rpart(medv~., data = boston, control = model.controls)

min_cp = which.min(fit_boston$cptable[,4])
pruned_fit_boston <- prune(fit_boston, cp = fit_boston$cptable[min_cp, 1])

x11()
plot(fit_boston$cptable[,4], main = "Cp for model selection", ylab = "cv error")

x11()
plot(pruned_fit_boston, branch = .3, compress=T, main = "Pruned Tree")
text(pruned_fit_boston, cex = .5)

x11()
plot(fit_boston, branch = .3, compress=T, main = "Full Tree")
text(fit_boston, cex = .5)


pred_train <- predict(pruned_fit_boston, newdata = boston)




























