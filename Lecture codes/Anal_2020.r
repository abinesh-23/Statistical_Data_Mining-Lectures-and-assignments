###########################################################################
## This code is to explore some data using different visualizations (EDA)
## Rachael Blair 
## Created: May 30, 2012
## Edited: Feb 2, 2020
###########################################################################


# set the current working directory
setwd("~/Dropbox/EAS 507: STA 546/week0")

# install some CRAN libararies 
#install.packages("DAAG")
install.packages("lattice")
install.packages("MASS")

# load libarary 
#library(DAAG)
library(lattice)
library(MASS)

load('~/Dropbox/EAS 507: STA 546/week0/DAAG_datasets.RData') ## new addition to code to bypass need for DAAG package

#####################################################
#########   Histogram and Density Plots
#####################################################
# get information about the possum data set
dim(possum)
head(possum)

#grab the females
fossum <- subset(possum, sex == "f")

#plot histograms ... 
par(mfrow = c(1,4))
attach(fossum)
hist(totlngth, breaks = 72.5 + (0:5)*5, ylim = c(0,22), xlab = "total length (cm)", main = "A: breaks at 72.5, 77.5, ... ")
hist(totlngth, breaks = 75 + (0:5)*5, ylim = c(0,22), xlab = "total length (cm)", main = "B: breaks at 75, 80, ... ")
# The only difference is the choice in breakpoints... one suggests the distribution is asymmetric and one suggests symmetry.  Histograms are crude approximations of density functions.  

#plot density functions
dens <- density(totlngth)
xlim <- range(dens$x)
ylim <- range(dens$y)
hist(totlngth, breaks = 72.5 + (0:5)*5, probability = T, xlim = xlim, ylim = ylim, xlab = "total length (cm)", main = "A: breaks at 72.5, 77.5, ... ")
lines(dens)
hist(totlngth, breaks = 75 + (0:5)*5, probability = T, xlim = xlim, ylim = ylim, xlab = "total length (cm)", main = "B: breaks at 75, 80, ... ")
lines(dens)
# most of the methods assume a normal distribution.... smooth density estimates are a better alternative for gauging is something is "normal".  

#####################################################
#########   Stem and Leaf Plots
#####################################################
head(ais)

#grab the rowers
rowers <- subset(ais, sport == "Row")

# Make a stem and leaf plot
stem(rowers$ht)

#####################################################
#########   Boxplots
#####################################################
#one version
quartz()
boxplot(fossum$totlngth, horizontal = TRUE)

#alternative version
quartz()
bwplot(~totlngth, data = fossum)

#####################################################
#########   Patterns in Univariate Time Series
#####################################################
par(mfrow = c(2,1))
#Panel A
plot(log10(measles), xlab = "", ylim = log10(c(1,5000*1000)), ylab = "Deaths; Poplation log-scale", yaxt = "n")
ytiks <- c(1, 10, 100, 1000, 1000000, 5000000)

londonpop <- ts(c(1088, 1258, 1504, 1778, 2073, 2491, 2921, 3336, 3881, 4266, 4563, 4541, 4498, 4408), start = 1801, end = 1931, deltat=10)
points(log10(londonpop*1000), pch=16, cex = .5)
axis(2, at=log10(ytiks), labels = paste(ytiks), las = 2)

#Panel B
plot(window(measles, start = 1840, end = 1882), ylim = c(0, 4600), ylab = "Deaths in 1000s", yaxt = "n")
axis(2, at = (0:4)*1000, labels = paste(0:4), las=2)
points(londonpop, pch=16, cex = .5)

# Saw tooth pattern is clearly evident ... 
# Panel A shows a clear trend over time, but no real idea on what is going on year to year.  Regular scale is not possible.
# Panel B subset of the data on a linear scale, together with the population.  

#####################################################
#########   Patterns in Bivariate Data
#####################################################
head(milk) #17 panelists assessed the sweeness of two milk samples
xyrange <- range(milk)
quartz()
plot(four ~ one, data = milk, xlim = xyrange, ylim = xyrange, main = "Milk Sweetness", pch = 16, pty = "s") 
rug(milk$one)
rug(milk$four)
abline(0,1)

#####################################################
#########   Fitting a smooth trend curve
#####################################################
head(fruitohms)
# plot ohms vs juice 
quartz()
plot(ohms ~ juice, xlab = "Apparent Juice Content (%)", ylab = "Resistance (ohms)", data = fruitohms)
# add a smooth curve
with(fruitohms, lines(lowess( juice, ohms), lwd=2))
#local weighted polynomial regression.

#######################################################
#########  What is the appropriate scale? #############
#######################################################
# plot brain weight against body weight
par(mfrow = c(1,2), pty = "s") #pty = "s" is square plotting region
plot(brain ~ body, data = Animals)
plot(log(brain) ~ log(body), data = Animals)

######################################################################
######### Patterns in grouped data - lengths of cuckoo eggs ##########
######################################################################
# Cuckoo birds lay eggs in the nests of other birds.  the other birds 
# raise the eggs and hatch them
par(mfrow = c(1,2), pty = "s") #pty = "s" is square plotting region
quartz()
stripplot(species ~ length, xlab = "Length of egg (mm)", data = cuckoos)
quartz()
bwplot(species ~ length, xlab = "Length of egg (mm)", data = cuckoos)

######################################################################
######### Comparing Density Plots                           ##########
######################################################################
# Lattice stype density plots useful for getting an indication of how 
# several species differ across different groups of data.  
densityplot(~earconch | sex, groups = Pop, data = possum, auto.key = list(space = "right"))

##########################################################################################
######### Scatterplots broken down by multiple factors                          ##########
##########################################################################################
head(tinting)
quartz()
xyplot(csoa ~it | sex*agegp, data=tinting, groups = target, auto.key = list(columns=2))
quartz()
xyplot(csoa ~it | sex*agegp, data=tinting, type = c("p", "smooth"), groups = target, auto.key = list(columns=2))
