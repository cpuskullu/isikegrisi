setwd("C:/Akademik/PROJE-MFAG113F353/KeplerLCs/KOI2_K2")
veri <- 
  read.table("C:/Akademik/PROJE-MFAG113F353/KeplerLCs/KOI2_K2/INITFLUX.tbl", 
  header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
summary(veri)
library(lattice, pos=14)
xyplot(V2 ~ V1, type="p", pch=16, auto.key=list(border=TRUE), 
  par.settings=simpleTheme(pch=16), scales=list(x=list(relation='same'), 
  y=list(relation='same')), data=veri)
T0=132.383
P=2.20473
with(veri, V1 <- (V1-T0)/P - as.integer((V1-T0)/P))

xyplot(V2 ~ V1, type="p", pch=16, auto.key=list(border=TRUE), 
  par.settings=simpleTheme(pch=16), scales=list(x=list(relation='same'), 
  y=list(relation='same')), data=veri)

bins <- 10
cutpoints<-quantile(veri$V1,(0:bins)/bins)
binned <-cut(veri$V1,cutpoints,include.lowest=TRUE)
summary(binned)

