##### R code for Question 2 in homework 1

##### Prepared by ABINESH SENTHIL KUMAR

##### 50320934

library(MASS)

bostondata <- Boston

str(bostondata)

summary(bostondata)

############################ QUESTION 2.a

#histogram of variables

hist(bostondata$crim)
hist(bostondata$zn)
hist(bostondata$indus)
hist(bostondata$chas)
hist(bostondata$nox)
hist(bostondata$rm)
hist(bostondata$age)
hist(bostondata$dis)
hist(bostondata$rad)
hist(bostondata$tax)
hist(bostondata$ptratio)
hist(bostondata$black)
hist(bostondata$lstat)
hist(bostondata$medv)

dev.off()

#par(mfrow=c(4,4))

#for (i in 1:ncol(bostondata)) {
# hist(bostondata[,i], xlab=colnames(bostondata[i]), ylab = "range", main = colnames(bostondata[i]))
#}
#dev.off()

########################################################################################

#converting continuous variables to categorical 

range(bostondata$crim)
bostondata$crim <- ordered(cut(bostondata$crim, c(0, 30, 60, 90)), labels = c("low", "medium", "high"))

range(bostondata$indus)
bostondata$indus <- ordered(cut(bostondata$indus, c(0, 10, 20, 30)), labels = c("low","medium","high"))

bostondata$chas <- as.factor(bostondata$chas)
bostondata$chas <- as.ordered(bostondata$chas)

library("car") # I am using recode function from car package, since cut function treats '0's as NA  
range(bostondata$zn)
bostondata$zn <- ordered(recode(bostondata$zn, "0:30 = 'low'; 30:60 = 'medium'; 60:100 = 'high'")) 
bostondata$zn <- factor(bostondata$zn, levels = c("low", "medium", "high"))  #I am using this because the levels are not ordered in right sequence
bostondata$zn <- as.ordered(bostondata$zn)

range(bostondata$nox)
bostondata$nox <- ordered(cut(bostondata$nox, c(0.3, 0.6, 0.9)), labels = c("low", "high"))

range(bostondata$rm)
bostondata$rm <- ordered(cut(bostondata$rm, c(3, 6, 9)), labels = c("less", "more"))

#bostondata$age <- Boston$age
range(bostondata$age)
bostondata$age <- ordered(cut(bostondata$age, c(0, 30, 60, 100)), labels = c("new", "not-so-old", "old"))

range(bostondata$dis)
bostondata$dis <- ordered(cut(bostondata$dis, c(1,4,8,13)), labels = c("near", "far", "farthest"))

range(bostondata$rad)
bostondata$rad <- ordered(cut(bostondata$rad, c(0, 8, 25)), labels = c("less accesible", "more accesible"))

range(bostondata$tax)
bostondata$tax <- ordered(cut(bostondata$tax, c(180, 300, 500, 720)), labels = c("less tax", "medium tax", "high tax"))

range(bostondata$ptratio)
bostondata$ptratio <- ordered(cut(bostondata$ptratio, c(11, 15, 19, 23)), labels = c("low", "medium", "high"))

range(bostondata$black)
bostondata$black <- ordered(cut(bostondata$black, c(0, 100, 250, 400)), labels = c("low", "medium", "high"))

range(bostondata$lstat)
bostondata$lstat <- ordered(cut(bostondata$lstat, c(1, 20, 40)), labels = c("very low", "low"))

range(bostondata$medv)
bostondata$medv <- ordered(cut(bostondata$medv, c(0, 15, 35, 50)), labels = c("low", "medium", "high"))

str(bostondata)
summary(bostondata)

#converting to binary incedence matrix

library(arules)

bostondata_transac <- as(bostondata, "transactions")

summary(bostondata_transac)

##################### QUESTION 2.b

#visualizing using itemfrequency plot 

itemFrequencyPlot(bostondata_transac, support = 0.001)

#Applying Apriori algorithm

summary(itemFrequency(bostondata_transac))  #to find maximum single item support
set.seed(1345)
rules  <- apriori(bostondata_transac, parameter = list(support = 0.05, confidence = 0.6))  

summary(rules)

##################### QUESTION 2.c

#Student interested in low crime area and less distance

rulesinterestcrim <- subset(rules, subset = rhs %in% "crim=low")

inspect(head(sort(rulesinterestcrim, by = 'confidence'), n = 5))
inspect(head(sort(rulesinterestcrim, by = 'lift'), n = 5))

rulesinterestdis <- subset(rules, subset = rhs %in% "dis=near")

inspect(head(sort(rulesinterestdis, by = 'lift'), n = 5))
inspect(head(sort(rulesinterestdis, by = 'confidence'), n = 5))

########## inspecting with both the constrains at once

rulesinterest_crim_dis <- subset(rules, subset = lhs %in% "dis=near" & rhs %in% "crim=low" )

inspect(head(sort(rulesinterest_crim_dis, by = 'confidence'), n = 5))
inspect(head(sort(rulesinterest_crim_dis, by = 'lift'), n = 5))

##################### QUESTION 2.d

#schools with low pupil teacher ratio

rulesinterestptratio <- subset(rules, subset = rhs %in% "ptratio=low")

#summary(rulesinterestptratio)

inspect(head(sort(rulesinterestptratio, by = 'confidence'), n = 5))
inspect(head(sort(rulesinterestptratio, by = 'lift'), n = 5))

###################### QUESTION 2.e

bostondata_reg_y <- as.data.frame(Boston$ptratio)
bostondata_reg_x <- bostondata[,-c(11)]

bostondata_regr <- cbind(bostondata_reg_x,bostondata_reg_y)

bostondata_regr$crim <- factor(bostondata_regr$crim, ordered = F)

bostondata_regr$zn <- factor(bostondata_regr$zn, ordered = F)

bostondata_regr$indus <- factor(bostondata_regr$indus, ordered = F)

bostondata_regr$chas <- factor(bostondata_regr$chas, ordered = F)

bostondata_regr$nox <- factor(bostondata$nox, ordered = F)

bostondata_regr$rm <- factor(bostondata_regr$rm, ordered = F)

bostondata_regr$age <- factor(bostondata_regr$age, ordered = F)

bostondata_regr$dis <- factor(bostondata_regr$dis, ordered = F)

bostondata_regr$rad <- factor(bostondata_regr$rad, ordered = F)

bostondata_regr$tax <- factor(bostondata_regr$tax, ordered = F)

bostondata_regr$black <- factor(bostondata_regr$black, ordered = F)

bostondata_regr$lstat <- factor(bostondata_regr$lstat, ordered = F)

bostondata_regr$medv <- factor(bostondata_regr$medv, ordered = F)

str(bostondata_regr)

#linear regression model with ptratio(Target) as continuous data and the rest as categorical
set.seed(12346)
linearmod <- lm(bostondata_regr$`Boston$ptratio` ~., bostondata_regr)

summary(linearmod)

######################################################################################################################

##### R code for Question 3 in homework 1

##### Prepared by ABINESH SENTHIL KUMAR

##### 50320934

library(ElemStatLearn)

marketing_data <- marketing

summary(marketing_data)

str(marketing_data)

#Clustering the data in appropriate factor levels

range(marketing_data$Income)
marketing_data$Income <- ordered(cut(marketing_data$Income, c(0,3,7,10)), labels = c("low", "medium", "high"))

marketing_data$Sex <- ordered(cut(marketing_data$Sex, c(0,1,2)), labels = c("Male", "Female"))

range(marketing_data$Marital, na.rm = T)
marketing_data$Marital <- ordered(cut(marketing_data$Marital, c(0,1,2,3,4,5)), labels = c("Married", "Living together, not married", "Divorced or separated", "Widowed", "Single,never married"))

range(marketing_data$Age)
marketing_data$Age <- ordered(cut(marketing_data$Age, c(0,2,5,7)), labels = c("young", "middle aged", "old"))

range(marketing_data$Edu, na.rm = T)
marketing_data$Edu <- ordered(cut(marketing_data$Edu, c(0,1,2,3,4,5,6)), labels = c("Grade 8 or less", "Grades 9 to 11","Graduated high school","1 to 3 years of college","College graduate","Grad Study"))

range(marketing_data$Occupation, na.rm = T)
marketing_data$Occupation <- ordered(cut(marketing_data$Occupation, c(0,1,2,3,4,5,6,7,8,9)), labels = c("Professional/Managerial","Sales Worker","Factory Worker/Laborer/Driver","Clerical/Service Worker","Homemaker", "Student, HS or College","Military", "Retired", "Unemployed"))

range(marketing_data$Lived, na.rm = T)
marketing_data$Lived <- ordered(cut(marketing_data$Lived, c(0,1,2,3,4,5)), labels = c("Less than one year",  "One to three years",  "Four to six years",  "Seven to ten years","More than ten years"))

range(marketing_data$Dual_Income)
marketing_data$Dual_Income <- ordered(cut(marketing_data$Dual_Income, c(0,1,2,3)), labels = c("Not Married", "Yes", "No"))

range(marketing_data$Household, na.rm = T)
marketing_data$Household <- ordered(cut(marketing_data$Household, c(0,1,2,3,4,5,6,7,8,9)), labels = c("One", "Two", "Three", "Four", "Five", "Six","Seven","Eight", "Nine or more"))

range(marketing_data$Householdu18)
marketing_data$Householdu18 <- ordered(cut(marketing_data$Householdu18, c(-1,0,1,2,3,4,5,6,7,8,9)), labels = c("None", "One", "Two", "Three", "Four", "Five", "Six","Seven","Eight", "Nine or more"))

range(marketing_data$Status, na.rm = T)
marketing_data$Status <- ordered(cut(marketing_data$Status, c(0,1,2,3)), labels = c("Own", "Rent", "Live with Parents/Family"))

range(marketing_data$Home_Type, na.rm = T)
marketing_data$Home_Type <- ordered(cut(marketing_data$Home_Type, c(0,1,2,3,4,5)), labels = c("House","Condominium","Apartment", "Mobile Home","Other"))

range(marketing_data$Ethnic, na.rm = T)
marketing_data$Ethnic <- ordered(cut(marketing_data$Ethnic, c(0,1,2,3,4,5,6,7,8)), labels = c("American Indian","Asian", "Black","East Indian","Hispanic","Pacific Islander","White","Other"))

range(marketing_data$Language, na.rm = T)
marketing_data$Language <- ordered(cut(marketing_data$Language, c(0,1,2,3)), labels = c("English","Spanish","Other"))


summary(marketing_data)


marketing_data_1 <- marketing_data #marketing_data_1 is the training sample

#creating a reference sample

data1 <- marketing_data_1 #data1 is the reference sample

set.seed(19)

for (i in 1:ncol(data1)) {
  data1[,i] <- sample(data1[,i], nrow(data1), replace = T)
  
}

#assigning class of 1 to existing training sample and class of 1 to created reference sample

data1$class <- 0
marketing_data_1$class <- 1

#combining the training and reference data

newdata_for_tree <- rbind(marketing_data_1, data1)

newdata_for_tree$class <- as.factor(newdata_for_tree$class)

str(newdata_for_tree)

############################


#for (i in 1:ncol(newdata_for_tree)) {
# newdata_for_tree[,i] <- factor(newdata_for_tree[,i], ordered = T)

#}

#creating a tree model using rpart with class as the target variable

library(rpart)

set.seed(28)

model.control <- rpart.control(maxdepth = 5, xval = 10, cp = 0)

fit.tree <- rpart(newdata_for_tree$class ~., data = newdata_for_tree, method = "class", control = model.control)

#plotting unpruned tree model
#library(rpart.plot)

plot(fit.tree, uniform = T, compress = T)
text(fit.tree)

#plotting the cp with respect to cv and finding the minimum cp

plot(fit.tree$cptable[,4])
mincp <- which.min(fit.tree$cptable[,4])

#creating a pruned tree

pruned.tree <- prune(fit.tree, cp = fit.tree$cptable[mincp,1])

#visualizing pruned tree

plot(pruned.tree, uniform = T, compress = T)
text(pruned.tree)

library(rpart.plot)
prp(pruned.tree,extra = 6, cex = 0.7)

#dev.off()
##########

summary(newdata_for_tree)

