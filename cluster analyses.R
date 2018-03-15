#Daten einlesen (deutsche csv-Datei)
NWM <- read.csv2("D:/Nationale Wohlfahrtsmessung.csv")

#Ränge zuweisen
GiniRang<- rank(NWM$Gini.Index, na.last = "keep")
BIPRang<- rank(NWM$BIP.Skaliert, na.last = "keep")
HappinessRang<- rank(NWM$Happiness, na.last = "keep")

#Spalten mit Rängen der Tabelle hinzuzufügen
NWM<-cbind(NWM, GiniRang)
NWM<-cbind(NWM, BIPRang)
NWM<-cbind(NWM, HappinessRang)

#libraray laden
library("cluster", lib.loc="C:/Program Files/R/R-3.2.2/library")

#Data Frame für die Indizes erstellenohne Rang
cluster<-data.frame(NWM$Happiness,NWM$Gini.Index,NWM$BIP.Skaliert)

#Data Frame für die Indizes erstellenmit Rang
clusterR<-data.frame(NWM$HappinessRang,NWM$GiniRang,NWM$BIPRang)

#Erstellen einer Distanzmatrix (mit Euklidischer Distanz)ohne Rang
dist.euclid<-daisy(cluster,metric="euclidean",stand=TRUE)

#Erstellen einer Distanzmatrix (mit Euklidischer Distanz)mit Rang
dist.euclidR<-daisy(clusterR,metric="euclidean",stand=TRUE)

#Durchführen der hierachischen Clusteranalyse mit der Distanzmatrixohne Rang
dendogramm<-hclust(dist.euclid,method="average")

#Durchführen der hierachischen Clusteranalyse mit der Distanzmatrixmit Rang
dendogrammR<-hclust(dist.euclidR,method="average")

#Klassifizierung der Daten in 3 Clusterohne Rang
cluster.hierarch_3<-cutree(dendogramm,k=3)

#Klassifizierung der Daten in 3 Clustermit Rang  
cluster.hierarch_3R<-cutree(dendogramm,k=3)

#Anfügen an die Tabelleohne Rang
NWM<-cbind(NWM, cluster.hierarch_3)

#Anfügen an die Tabellemit Rang
NWM<-cbind(NWM, cluster.hierarch_3R)

#Mittelwert der jeweiligen Cluster-Hilfstabelle für das auslöschen von NA Werten
Hilfstabelle <-data.frame(NWM$BIP.Skaliert,NWM$cluster.hierarch_3) 
Hilfstabelle<-na.omit(Hilfstabelle)	

#Happiness
tapply(Hilfstabelle$NWM.Happiness,Hilfstabelle$NWM.cluster.hierarch_3,mean)

tapply(Hilfstabelle$NWM.Happiness,Hilfstabelle$cluster.hierarch_3R,mean)

#BIP Skaliert
tapply(Hilfstabelle$NWM.BIP.Skaliert,Hilfstabelle$NWM.cluster.hierarch_3,mean)

tapply(Hilfstabelle$NWM.BIP.Skaliert,Hilfstabelle$cluster.hierarch_3R,mean)

#Gini-Index
tapply(NWM$Gini.Index, cluster.hierarch_3, mean)

tapply(NWM$Gini.Index, cluster.hierarch_3R, mean)

#Anzahl der Länder in den jeweiligen Clustern
table(cluster.hierarch_3)
cluster.hierarch_3

table(cluster.hierarch_3R)
cluster.hierarch_3R

#Plot für die Clusteranalyse ohne Rang 
plot(dendogramm,xlab="Objekte",ylab="Distanzen",main="Dendogramm der Clusteranalyse (Average)")
