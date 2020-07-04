# This is an optional lab concerning "tree"

library("tree")
library("caret")

#load the data
load("pima.RData")
my_pima <- pima[,-8]

# divide into test and training
set.seed(12345)
test_indis <- sample(1:nrow(my_pima), 2/3*nrow(my_pima))
test <- my_pima[test_indis,]
training <- my_pima[-test_indis,]

# fit a tree
fit <- tree(class~., data = training, split = "gini")
summary(fit)

pred.test <- predict(fit, newdata = test, type = "class") # 2017: small syntax change for'type'
confusionMatrix(pred.test, test$class)

cv.fit = cv.tree(fit, FUN = prune.misclass)
summary(cv.fit)
cv.fit

pruned_tree <- prune.misclass(fit, best = 6)

x11()
plot(pruned_tree)
text(pruned_tree)


x11()
plot(fit)
text(fit)










