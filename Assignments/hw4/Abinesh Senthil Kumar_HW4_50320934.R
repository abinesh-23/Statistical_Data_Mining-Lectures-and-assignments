####################################### Abinesh Senthil Kumar ######################################################
############################################ 50320934  HW4 #########################################################

######################################## question 1 #################################################################

######################

library(igraph)
library(igraphdata)
library(ape)

#loading the data

data(kite)
data(karate)

###################### for Karate network ###########################################################################

layoutkarate <- layout_nicely(karate)
plot(karate, layout = layoutkarate, main = "Original karate network")

#fitting hierarchial dendrogram
set.seed(123)
karatehrg <- fit_hrg(karate)
plot_dendrogram(karatehrg, main = "Dendrogram of original karate network")

#plotting full hierarchy as igraph
ihrg <- as.igraph(karatehrg)
ihrg$layout <- layout.reingold.tilford
plot(ihrg, vertex.size=5, edge.arrow.size=0.2, main = "Full hierarchy of original karate network")

##################### for Kite network ##############################################################################


layoutkite <- layout_nicely(kite)
plot(kite, layout = layoutkite, main = "Original Kite network")

#fitting hierarchial dendrogram
set.seed(1)
kitehrg <- fit_hrg(kite)
plot_dendrogram(kitehrg, main = "Dendrogram of original kite network")

#plotting full hierarchy as igraph
ihrgkite <- as.igraph(kitehrg)
ihrgkite$layout <- layout.reingold.tilford
plot(ihrgkite, vertex.size=12, edge.arrow.size=0.2, main = "Full hierarchy of original kite network")

######################

#randomly deleting specific percent edge from kite and prediction of them ########################################
#function which takes data and percent as input and predicts the deleted percentage of input edges with probabilities

predictprobedge <- function(data,percent) {
  set.seed(11234)
  samplesize <- sample(length(E(data)), round(((percent/100)*length(E(data)))))
  deletededges <- delete.edges(data, samplesize)
  
  #tracking the deleted edge
  
  trackdeleted <- function(data,samplesizez) {
    a <- (get.vertex.attribute(data)$name)
    b <- 1:length(a)
    
    ab <- data.frame(a,b)
    colnames(ab) <- c('V1', 'V2')
    edgelist <- as.data.frame(get.edgelist(data))
    edgelist <- edgelist[samplesizez,]
    
    newab <- merge(edgelist, ab, by = 'V1')
    
    newab <- newab[,-c(1)]
    
    colnames(newab) <- c('V1', 'V2')
    
    newab1 <- merge(newab, ab, by = 'V1')
    
    newab1 <- newab1[,-c(1)]
    colnames(newab1) <- c('V1', 'V2')
    return(newab1)
  }
  
  deleted <- trackdeleted(data, samplesize)
  
  #predicting the deleted edge
  
  predictdata <- predict_edges(deletededges)
  mcmc <- plot_dendrogram(predictdata$hrg, 
                          main = ifelse(length(E(data)) == length(E(karate)), 
                                        sprintf("HRG of noisy Karate data (%s percent deleted edges)", percent),sprintf("HRG of noisy Kite data (%s percent deleted edges)", percent)))
  predictedfulledges <- as.data.frame(predictdata$edges)
  predictedfulledges$prob <- predictdata$prob
  
  finalans <- (merge(deleted, predictedfulledges))
  
  a <- plot(predictdata$prob, col = (ifelse(predictdata$prob %in% finalans$prob , "red", "black")),
            main = ifelse(length(E(data)) == length(E(karate)), sprintf("Karate data and probabilities of predicted %s percent of deleted data",percent),sprintf("Kite data and probabilities of predicted %s percent of deleted data",percent))
            , xlab = "Predicted edges index", ylab = "Probabilities", pch = 15)
  
  b <- plot(predictdata$prob, col = (ifelse(predictdata$prob %in% finalans$prob , "red", "white")),
            main = ifelse(length(E(data)) == length(E(karate)), sprintf("Karate data and probabilities of predicted %s percent of deleted data",percent),sprintf("Kite data and probabilities of predicted %s percent of deleted data",percent))
            , xlab = "Predicted edges index", ylab = "Probabilities", cex = 1, pch = 15)
  
  
  return (c(finalans,a,b,mcmc))
  
}

####################### randomly deleting 5, 15, 40 percent of edges from kite ########################################
#dev.off()

#calling the function to give out the hrg of noisy dataset and the predicted(deleted) edges with probabilities

#par(bg = "grey")
kite5p <- predictprobedge(kite,5) #gives out three plots 
kite15p <- predictprobedge(kite,15) #gives out three plots
kite40p <- predictprobedge(kite,40) #gives out three plots

#dev.off()
#probabilities of predicted(deleted) edges

list(kite5p$V1,kite5p$V2,kite5p$prob) #Predicting 5% deleted edges and the probability for kite data
list(kite15p$V1,kite15p$V2,kite15p$prob) #Predicting 15% deleted edges and the probability for kite data
list(kite40p$V1,kite40p$V2,kite40p$prob) #Predicting 40% deleted edges and the probability for kite data


####################### randomly deleting 5, 15, 40 percent of edges from karate ########################################

#calling the function to give out the hrg of noisy dataset and the predicted(deleted) edges with probabilities

#par(bg = "grey")
karate5p <- predictprobedge(karate,5) #gives out three plots
karate15p <- predictprobedge(karate,15) #gives out three plots
karate40p <- predictprobedge(karate,40) #gives out three plots

#dev.off()
#probabilities of predicted(deleted) edges
list(karate5p$V1, karate5p$V2, karate5p$prob) #Predicting 5% deleted edges and the probability for karate data
list(karate15p$V1, karate15p$V2, karate15p$prob) #Predicting 15% deleted edges and the probability for karate data
list(karate40p$V1, karate40p$V2, karate40p$prob) #Predicting 40% deleted edges and the probability for karate data

######################################################################################################################
############################### Question 2 ############################################################################

library(gRbase)
library(ggm)

graph <- list(~A, ~B, ~C|A, ~D|A:B,
              ~E|B, ~F|C:A:E, ~G|D:E, ~H|F:G)

q2graph <- dagList(graph)

plot(q2graph)

#dsep
dSep(as(q2graph,"matrix"), "C","G", NULL) #False
#dsep
dSep(as(q2graph,"matrix"), "C","E", NULL) #True
#dcon
dSep(as(q2graph,"matrix"), "C","E", "G") #True (since the question is asking d-connected)
#dcon
dSep(as(q2graph,"matrix"), "A","G", c("D","E")) #False (since the question is asking d connected)
#dcom
dSep(as(q2graph,"matrix"), "A","G", "D") #True (since the question is asking d connected)


###############################################################################################################################
############################################################## Question 3 #####################################################
library(gRbase)
library(gRain)
library(ggm)

#loading data
data("cad1")

#creating the DAG
cadgraph <- list(~Sex, ~Smoker|Sex, ~SuffHeartF, ~Inherit|Smoker, ~Hyperchol|Smoker:SuffHeartF,
                 ~CAD|Inherit:Hyperchol)

q3graph <- dagList(cadgraph)

#viaualizing created graph
plot(q3graph)

#extracting the conditional probability tables
a <- extractCPT(cad1, q3graph)

cpt <- compileCPT(a)

cpt$Sex
cpt$Smoker
cpt$SuffHeartF
cpt$Inherit
cpt$Hyperchol
cpt$CAD

#finding dseperations in graph

dSep(as(q3graph, "matrix"),"Sex","SuffHeartF", NULL) #True
dSep(as(q3graph, "matrix"),"Sex","Inherit","Smoker") #True
dSep(as(q3graph, "matrix"),"Sex","Hyperchol","CAD") #False
dSep(as(q3graph, "matrix"),"Sex","Hyperchol","Smoker") #True
dSep(as(q3graph, "matrix"),"Sex","SuffHeartF","CAD") #False
dSep(as(q3graph, "matrix"), "Inherit","SuffHeartF","Smoker") #True
dSep(as(q3graph, "matrix"),"Smoker","CAD",c("Inherit","Hyperchol")) #True
dSep(as(q3graph, "matrix"),"Sex","SuffHeartF",c("Inherit","Hyperchol")) #False


#creating and propogating network
propogatedcad <- propagate(compile(grain(cpt)))

#absorbing evidence
neweviednce <- setFinding(propogatedcad, nodes=c("Sex","Hyperchol"), states = c("Female","Yes")) 
getFinding(neweviednce)

#Conditional probability before and after absorbing evidence

#without evidence
condprobwithoutevd <- querygrain(propogatedcad, nodes = c("SuffHeartF","CAD"), type="conditional") 
condprobwithoutevd
#with evidence
condprobwithevd <- querygrain(neweviednce, nodes = c("SuffHeartF","CAD"), type="conditional") 
condprobwithevd

#joint probability before and after absorbing evidence

#without evidence
jointprobwithoutevd <- querygrain(propogatedcad, nodes = c("SuffHeartF","CAD"), type="joint") 
jointprobwithoutevd
#with evidence
jointprobwithevd <- querygrain(neweviednce, nodes = c("SuffHeartF","CAD"), type="joint") 
jointprobwithevd

#marginal probability before and after absorbing evidence

#without evidence
marginalprobwithoutevd <- querygrain(propogatedcad, nodes = c("SuffHeartF","CAD"), type="marginal") 
marginalprobwithoutevd
#with evidence
marginalprobwithevd <- querygrain(neweviednce, nodes = c("SuffHeartF","CAD"), type="marginal") 
marginalprobwithevd


#simulate 25 observations
set.seed(1234)
simulated25 <- simulate(neweviednce, nsim = 25)
simpred25 <- predict(neweviednce,c("Smoker","CAD"), newdata = simulated25) 
simpred25
save(simulated25, file = 'simulated25obs.RData')

#simulate 500 observations
set.seed(12)
simulated500 <- simulate(neweviednce, nsim = 500)
simpred500 <- predict(neweviednce,c("Smoker","CAD"), newdata = simulated500) 
simpred500
save(simulated500, file = 'simulated500obs.RData')

##############################################################################################################################################

