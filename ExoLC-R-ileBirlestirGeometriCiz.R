#####################################################################################
#                    -- ExoLC-BirlestirCiz.R - 05.12.2015 --                        #
# HESAP:: YOK                                                                       #
# CIZIM:: Dizindeki .ciz uzantili isik egrisi verisi dosyalarini okur               #
# ve veriyi yada sadece model egrileri ust uste cizer.                              #
# Ayrica gecis geometrisini de cizer.                                               #
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
# Disarida birakilacak dosyanin [.ciz] uzantisi silinir.
dosyalar <- list.files(path = ".", pattern = "*.ciz", all.files = FALSE)
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
metinboyut <- 16

# Grafik
graftur <- "p"
sadecemodeller <- F
geometriciz <- T
icerikboyut <- 0.7
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
yi <- 0.96
ys <- 1.02
dy_ana <- 0.02
dy_ara <- 0.005
anaciz <- 0.8
araciz <- 0.4
bosluk <- 0
kesisme <- "i" # Eksenlerin kesisme durumu:  "r" (regular), "i" (internal)
eksenbas_y <- "Normalised Flux"   # "Normalised Flux"
eksenbas_x <- "Time from mid-transit (day)"  # "Phase"
kenaralt <- 5
kenarsol <- 6
kenarust <- 1
kenarsag <- 1

if(geometriciz){
par(mfrow=c(2,1), oma=c(6,6,3,1))
kenaralt <- 0
kenarsol <- 0
kenarust <- 0
kenarsag <- 0 
bosluk <- (1-ys) }
### --GORUNUM-- ###
###################

par(mar=c(kenaralt,kenarsol,kenarust,kenarsag), 
     cex=icerikboyut, ps=metinboyut, font=gorun, family=bicem, 
     pch=noktatur, bg=NA, las=1, lwd=kalinlik, xaxs=kesisme, yaxs=kesisme)
     
########################                     
### Gecis Geometrisi ###
if(geometriciz){
# Eksen
olcek <- 1
# Dikey eksende kaydirma parametreleri
ggyi <- 0
ggys <- 0
# Geometrik parametreler: HAT-P-36 b
olcek <- 1
a <- 5.123  # [Ro] biriminde
b <- 0.2855
r1 <- 0.2041
r2 <- 0.0247

library(shape)
# x ekseni evreye uyduruldu: .25 evre birim eksen uzunluguna karsilik geldigi icin 4 kati yazilir (4xi), (birim uzunluk x gercek yari-buyuk eksen) degerinden gezegenin kendisi (cap uzunlugu) cikarilir.
# Hesap yanlis, dogru hesap Birlestir-Bin-Ciz.R kodunda
emptyplot(c(4*xi*a-(2*r2*a), 4*xs*a+(2*r2*a)), c(ggyi, ggys))

plotellipse(rx=a*olcek, ry=(b*r1*a)*olcek, angle=0, from=0, to=pi, lwd=2, lty="solid")
plotcircle(r=(r1*a)*olcek, col="gray80", lwd=0)
plotellipse(rx=a*olcek, ry=(b*r1*a)*olcek, angle=0, from=pi, to=0, lwd=2, lty="solid")
plotcircle(r=(r2*a)*olcek, mid=c(0, -(b*r1*a)*olcek), col="black", lwd=0)
lines(c(-olcek*a,olcek*a), c(0,0), lty="dotted")

#axis(side=1, pos=0, at=seq(-10,10,by=0.2))
axis(side=2, at=seq(-10,10,by=0.25), lwd=par("lwd"), lwd.ticks=0, labels=F)
axis(side=2, at=seq(0,1,by=0.5), las=1, tcl=anaciz, lwd=par("lwd"))
axis(side=2, at=seq(0,1,by=0.25), las=1, tcl=araciz, lwd=par("lwd"), labels=F)
axis(side=3, at=seq(round(4*xi*a),round(4*xs*a),by=round(4*a*dx_ana)), tcl=anaciz, lwd=par("lwd"), labels=T)
axis(side=3, at=seq(round(4*xi*a),round(4*xs*a),by=round(4*a*dx_ara,2)), tcl=araciz, lwd=par("lwd"), labels=F)
axis(side=3, at=seq(-10,10,by=0.25), lwd=par("lwd"), lwd.ticks=0, labels=F)
axis(side=4, at=seq(0,1,by=0.5), las=1, tcl=anaciz, lwd=par("lwd"))
axis(side=4, at=seq(0,1,by=0.25), las=1, tcl=araciz, lwd=par("lwd"), labels=F)
axis(side=4, at=seq(-10,10,by=0.25), lwd=par("lwd"), lwd.ticks=0, labels=F)
}                                        
### Gecis Geometrisi ###
########################

# Once bos grafik cizilir:
plot(0, type="n", xlab="", ylab="", axes=F,   
     xlim=c(xi, xs), ylim=c(yi, ys), col=noktarenk)

axis(side=1, at=seq(xi,xs,by=dx_ana), tcl=anaciz, lwd=par("lwd"))
axis(side=1, at=seq(xi,xs,by=dx_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=2, at=seq(yi,ys+bosluk,by=dy_ana), tcl=anaciz, lwd=par("lwd"))
axis(side=2, at=seq(yi,ys+bosluk,by=dy_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=2, at=seq(yi,ys,by=dy_ara), lwd=par("lwd"), lwd.ticks=0, labels=FALSE)
if(!geometriciz){
axis(side=3, at=seq(xi,xs,by=dx_ana), tcl=anaciz, lwd=par("lwd"), labels=FALSE)
axis(side=3, at=seq(xi,xs,by=dx_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)}
axis(side=4, at=seq(yi,ys+bosluk,by=dy_ana), tcl=anaciz, lwd=par("lwd"), labels=FALSE)
axis(side=4, at=seq(yi,ys+bosluk,by=dy_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=4, at=seq(yi,ys,by=dy_ara), lwd=par("lwd"), lwd.ticks=0, labels=FALSE)

mtext(eksenbas_x, side=1, line=3, cex=par("cex")+0.1)
mtext(eksenbas_y, side=2, line=4, cex=par("cex")+0.1, las=0)
#box()

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
library(NCmisc); wait(1,unit = "s")
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
     xlim=c(xi, xs), ylim=c(yi, ys), col=noktarenk)
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

if(yazdir){
dev.off()
}
