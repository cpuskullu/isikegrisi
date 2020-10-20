#####################################################################################
#                    -- ExoLC-BirlestirCiz.R - 05.12.2015 --                        #
# HESAP:: YOK                                                                       #
# CIZIM:: Dizindeki .cikti uzantili isik egrisi verisi dosyalarini okur             #
# ve ust uste cizdirir.                                                             #
#####################################################################################

### --BUYUK HARFLERLE-- YAZILAN BASLIKLAR ICINDEKI GIRDILER DUZENLENMELIDIR.      ###
### Kucuk Harflerle yazilan basliklar altinda islemler yapilir.                   ###

####################
### Kutuphaneler ###
library(scales) # renk saydamligi -alpha() icin kullaniliyor

###################
### --AYARLAR-- ###
setwd("D:/Akademik/TEZ/DR/ExoLCA/HAT-P-36")
# Girdi Sutunlari: ObsPhase ObsFlux/Mag Error ModPhase ModFlux/Mag ResPhase ResFlux/Mag
# Disarida birakilacak dosyanin [.cikti] uzantisi silinir.
dosyalar <- list.files(path = ".", pattern = "*.cikti", all.files = FALSE)
#dosyalar <- ""; dosyalar[1] <- "#BirlestirilmisBinned5-qatar1b_R.dat"
### --AYARLAR-- ###
###################

##########################
### --GRAFIGI YAZDIR-- ###
# pdf(), png(), postscript(): saydamligi desteklemez: pdf'i eps'ye cevir.
yazdir <- 0 
if(yazdir){
pdf(paste0("mean_qatar1b_R",".pdf"))#, width = 40, height = 30)
# En altta dev.off komutunu kapatmayi unutma
}
### --GRAFIGI YAZDIR-- ###
##########################

###################
### --GORUNUM-- ###
# Metin
bicem = "serif"  # "serif", "sans", "mono", "symbol"
gorun = 1 # 1:plain, 2:bold, 3: italic
metinboyut <- 18

# Grafik
graftur <- "p"
sadecemodeller <- T
geometriciz <- F
icerikboyut <- 1
kalinlik <- 2
noktatur <- 16
noktarenk <- alpha("black",1)
modelboyut <- 2
modelrenk <- alpha("red",1)
modeltur <- "solid"

# Eksen
xi <- -0.08
xs <- 0.08
dx_ana <- 0.04
dx_ara <- 0.01
ustbosluk <- 0.08
yi <- 0.96
ys <- 1.00
dy_ana <- 0.02
dy_ara <- 0.005
anaciz <- 0.8
araciz <- 0.4
kesisme <- "i" # Eksenlerin kesisme durumu:  "r" (regular), "i" (internal)
eksenbas_y <- "Normalised Flux"   # "Normalised Flux"
eksenbas_x <- "Time from mid-transit (day)"  # "Phase"
kenaralt <- 5
kenarsol <- 6
kenarust <- 1
kenarsag <- 1
### --GORUNUM-- ###
###################

par(mar=c(kenaralt,kenarsol,kenarust,kenarsag),
      
     cex=icerikboyut, ps=metinboyut, font=gorun, family=bicem, 
     pch=noktatur, bg=NA, las=1, lwd=kalinlik, xaxs=kesisme, yaxs=kesisme)

# Once bos grafik cizilir:
plot(0, type="n", xlab="", ylab="", axes=F,   
     xlim=c(xi, xs), ylim=c(yi, ys+ustbosluk), col=noktarenk)

axis(side=1, at=seq(xi,xs,by=dx_ana), tcl=anaciz, lwd=par("lwd"))
axis(side=1, at=seq(xi,xs,by=dx_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=2, at=seq(yi,ys,by=dy_ana), tcl=anaciz, lwd=par("lwd"))
axis(side=2, at=seq(yi,ys,by=dy_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=3, at=seq(xi,xs,by=dx_ana), tcl=anaciz, lwd=par("lwd"), labels=FALSE)
axis(side=3, at=seq(xi,xs,by=dx_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=4, at=seq(yi,ys,by=dy_ana), tcl=anaciz, lwd=par("lwd"), labels=FALSE)
axis(side=4, at=seq(yi,ys,by=dy_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)

mtext(eksenbas_x, side=1, line=3, cex=par("cex")+0.1)
mtext(eksenbas_y, side=2, line=4, cex=par("cex")+0.1, las=0)
box()

if(FALSE){
# Resim ekleme
library(png) # png, jpeg
img <- readPNG("tres3b_geometry.png") 
rasterImage(img,-0.03,1.01,0.03,1.035)
}

for(i in 1:length(dosyalar)){

################
### Veri Oku ###
gozlem <- read.table(dosyalar[i], header=FALSE, sep="\t")

if(sadecemodeller){
############################                     
### Gecis Model Egrileri ###
par(new=TRUE)
plot(gozlem$V4, gozlem$V5, type="l", xlab="", ylab="", lwd=modelboyut, axes=F,
     xlim=c(xi, xs), ylim=c(yi, ys), col=alpha(rainbow(length(dosyalar))[i],0.7))
legend("top", inset=.04, dosyalar, bty = "o", box.lty=0,     
     cex=0.6, text.font=1, text.col="black",  bg='white', y.intersp=1.4,
     lty=1, col=rainbow(length(dosyalar)) )
# Cizim yaparken belli bir sure bekle
library(NCmisc); wait(2,unit = "s")
### Gecis Model Egrileri ###
############################
modelrenk <- "black"
modeltur <- "dashed"
} 
else {
####################                     
### Geçiþ Eðrisi ###
par(new=TRUE)
plot(gozlem$V1, gozlem$V2, type=graftur, xlab="", ylab="", axes=F,
     xlim=c(xi, xs), ylim=c(yi, ys+ustbosluk), col=noktarenk)
#legend("top", inset=.02, dosyalar, bty = "o", box.lty=0, cex=0.6, text.font=1, text.col="black",  bg='white', y.intersp=1.4, pch=noktatur, pt.cex=1, col=rainbow(length(dosyalar)), pt.lwd=1 )

### Geçiþ Eðrisi ###
####################
}}
###########################                     
### Ortalama Model Egri ###
model <- read.table(paste0(getwd(),"/WinFitter/mean_hatp36b_R-Run1.mod"),
                     header=FALSE, skip=3, sep="", na.strings="-99.0000000")
lines(model$V1/360, model$V2+(1-model$V2[1]), col=modelrenk, lwd=modelboyut, lty=modeltur)  
#print(1-model$V2[1])
### Ortalama Model Egri ###
###########################                        

########################                     
### Gecis Geometrisi ###
if(geometriciz){
# Eksen
olcek <- 1
# Dikey eksende kaydirma parametreleri
ggyi <- -.4
ggys <- 0.08
# Geometrik parametreler
Tgecis <- 84.006 # dakika
b <- 0.83898
r1 <- 0.16866
r2 <- 0.02920

library(shape)
par(new=TRUE, fig=c(0, 1, 0, 1))
# x ekseni evreye uyduruldu: .25 evre 1'e karsilik geldigi icin 4 kati yazilir.
emptyplot(c(4*xi, 4*xs), c(ggyi, ggys))
axis(side=2, at=seq(0,0.4,by=0.1), las=1, tcl=anaciz, lwd=par("lwd"))
axis(side=2, at=seq(0,0.4,by=0.025), las=1, tcl=araciz, lwd=par("lwd"), labels=F)

plotellipse(rx=olcek, ry=(1-b)*olcek, angle=0, from=0, to=pi, lwd=2, lty="solid")
plotcircle(r=r1*olcek, col="gray80", lwd=0)
plotellipse(rx=olcek, ry=(1-b)*olcek, angle=0, from=pi, to=0, lwd=2, lty="solid")
plotcircle(r=r2*olcek, mid=c(0, -(1-b)*olcek), col="black", lwd=0)
lines(c(-olcek,olcek), c(0,0), lty="dotted")
}                                        
### Gecis Geometrisi ###
########################
if(yazdir){
dev.off()
}
