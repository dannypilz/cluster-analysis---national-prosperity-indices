#read the german csv-file
NWM <- read.csv2("D:/Nationale Wohlfahrtsmessung.csv")

#ranks assign
GiniRang<- rank(NWM$Gini.Index, na.last = "keep")
BIPRang<- rank(NWM$BIP.Skaliert, na.last = "keep")
HappinessRang<- rank(NWM$Happiness, na.last = "keep")

#add column with ranks to the dataset
NWM<-cbind(NWM, GiniRang)
NWM<-cbind(NWM, BIPRang)
NWM<-cbind(NWM, HappinessRang)

#libraray laden
library("cluster", lib.loc="C:/Program Files/R/R-3.2.2/library")

#data frame for indices without ranks
cluster<-data.frame(NWM$Happiness,NWM$Gini.Index,NWM$BIP.Skaliert)

#data frame for indices with ranks
clusterR<-data.frame(NWM$HappinessRang,NWM$GiniRang,NWM$BIPRang)

#assign distance matrix (with euclidean distance) without ranks
dist.euclid<-daisy(cluster,metric="euclidean",stand=TRUE)

#assign distance matrix (with euclidean distance) with ranks
dist.euclidR<-daisy(clusterR,metric="euclidean",stand=TRUE)

#hierarchical cluster analysis without ranks
dendogramm<-hclust(dist.euclid,method="average")

#hierarchical cluster analysis with ranks
dendogrammR<-hclust(dist.euclidR,method="average")

#Klassifizierung der Daten in 3 Clusterohne Rang
cluster.hierarch_3<-cutree(dendogramm,k=3)

#classification in 3 cluster with ranks  
cluster.hierarch_3R<-cutree(dendogramm,k=3)

#add to the data frame (without ranks)
NWM<-cbind(NWM, cluster.hierarch_3)

#add to the data frame (with ranks)
NWM<-cbind(NWM, cluster.hierarch_3R)

#mean of the cluster-help-tables for remove NA-value
Hilfstabelle <-data.frame(NWM$BIP.Skaliert,NWM$cluster.hierarch_3) 
Hilfstabelle<-na.omit(Hilfstabelle)	

#happiness index
tapply(Hilfstabelle$NWM.Happiness,Hilfstabelle$NWM.cluster.hierarch_3,mean)

tapply(Hilfstabelle$NWM.Happiness,Hilfstabelle$cluster.hierarch_3R,mean)

#GDP scaled
tapply(Hilfstabelle$NWM.BIP.Skaliert,Hilfstabelle$NWM.cluster.hierarch_3,mean)

tapply(Hilfstabelle$NWM.BIP.Skaliert,Hilfstabelle$cluster.hierarch_3R,mean)

#gini-index
tapply(NWM$Gini.Index, cluster.hierarch_3, mean)

tapply(NWM$Gini.Index, cluster.hierarch_3R, mean)

#number of countries in each cluster
table(cluster.hierarch_3)
cluster.hierarch_3

table(cluster.hierarch_3R)
cluster.hierarch_3R

#plot for cluster analysis without ranks
plot(dendogramm,xlab="Objekte",ylab="Distanzen",main="Dendogramm der Clusteranalyse (Average)")
