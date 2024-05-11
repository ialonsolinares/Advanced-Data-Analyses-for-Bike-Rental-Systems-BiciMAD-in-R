
#Leemos los datos

ts<-read.csv("transpose.csv", sep=";")
ts<-ts[,-c(9,23,146,147,174)]
ts<-data.frame(ts[,-1], row.names=ts[,1])

#Matplot, plots every station
matplot(y = ts, type = 'l', lty = 1, col="black", xlab="Hour", ylab="Occupation Percentage",axes=F)

axis(2)
axis(side=1,at=1:nrow(ts))

#Plots for each station individually, producing 173 graphs (one per station)
individual <- function(col) 
  plot(ts[ , col], ylab = names(ts[col]), type = "l")
par(ask = TRUE)
sapply(seq(1, length(ts), 1), individual)

#Loading Original Dataset
ts_original<-read.csv("notranspose.csv", sep=";")

ts_original<-data.frame(ts_original[,-1], row.names=ts_original[,1])

#Time Series Dendogram
library(dtw)

distance<-dist(ts_original, method="DTW")
hc<-hclust(distance,method="average")
plot(hc, cex=0.7)

#optimal clusters
library(factoextra)

i<-decompose(ts_original)
plot(i)

#Criteria used for deciding optimal cluster number
fviz_nbclust(x = ts_original, FUNcluster = hcut, method = "wss", verbose = FALSE, nstart = 50)
fviz_nbclust(x = ts_original, FUNcluster = hcut, method = "silhouette", verbose = FALSE, nstart = 50)


