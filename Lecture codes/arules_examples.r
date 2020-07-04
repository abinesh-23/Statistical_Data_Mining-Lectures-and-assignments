#############################################################################
## This code is used to explore some of the techniques in the arules package
## 
## Rachael Blair 
## Created: February 7, 2014
## Edited: 2/1/2017
#############################################################################
rm(list = ls())
graphics.off()

library("arules")

########################
## 
##    Example 1 
########################
data("Epub")
?Epub

summary(Epub)
image(Epub, pch = 20)

?transactionInfo

year <- strftime(as.POSIXlt(transactionInfo(Epub)[["TimeStamp"]]), "%Y")

# Focus on 2003
Epub2003 <- Epub[year == "2003"]
length(Epub2003)


# A closer look at the transactions
quartz()  # x11()
image(Epub2003, pch = 20)

transactionInfo(Epub2003[size(Epub2003)>5])
transactionInfo(Epub2003[size(Epub2003)>10])
transactionInfo(Epub2003[size(Epub2003)>25])

# Convert transaction style data to a list
Epub_list <- as(Epub2003, "list")
names(Epub_list)
Epub_list[[1]]
Epub_list[[2]]
Epub_list[[3]]
Epub_list[[4]]

pubs <- as(Epub2003, "transactions")

quartz()
itemFrequencyPlot(pubs, support = 0.01, cex.names = 0.8)


quartz()
itemFrequencyPlot(pubs, support = 0.02, cex.names = 0.8)


rules <- apriori(pubs, parameter = list(support = 0.01, confidence = 0.005))


########################
## 
##    Example 2 
########################
data("AdultUCI")

# Lets look at the first five rows
head(AdultUCI)
AdultUCI[1:5,]

# Eliminate Variables
AdultUCI[["education-num"]] <- NULL
AdultUCI[["fnlwgt"]] <- NULL

# Deal with ordered variables 
AdultUCI[["age"]] <- ordered(cut(AdultUCI[["age"]], c(15, 25, 45, 65, 100)), labels = c("Young", "Middle-aged", "Senior", "Elderly"))

AdultUCI[["hours-per-week"]] <- ordered( cut(AdultUCI[["hours-per-week"]], c(0,25,40,60,100)), labels = c("Part-time", "Full-Time", "Over-time", "Workaholic"))

AdultUCI[["capital-gain"]] <- NULL
AdultUCI[["capital-loss"]] <- NULL

#Convert to a binary incidence matrix
Adult <- as(AdultUCI, "transactions")
summary(Adult)

quartz()
itemFrequencyPlot(Adult, support = 0.05, cex.names = 0.8)

# Apply the apriori algorithm
rules  <- apriori(Adult, parameter = list(support = 0.001, confidence = 0.6))

# Take a closer look at the different rules
summary(rules)
rulesIncomeSmall <- subset(rules, subset = rhs %in% "income=small" & lift>1.2)
rulesIncomeLarge <- subset(rules, subset = rhs %in% "income=large" & lift>1.2)

rulesIncomeSmall
rulesIncomeLarge

inspect(head(sort(rulesIncomeSmall, by = "confidence"), n = 3))
inspect(head(sort(rulesIncomeLarge, by = "confidence"), n = 3))

inspect(head(sort(rulesIncomeSmall, by = "lift"), n = 3))
inspect(head(sort(rulesIncomeLarge, by = "lift"), n = 3))

setwd("~/Desktop/DataMining_Spring2014/Computational Labs")
write(rulesIncomeLarge, file = "large_rules.csv", sep = ",", col.names = NA)










