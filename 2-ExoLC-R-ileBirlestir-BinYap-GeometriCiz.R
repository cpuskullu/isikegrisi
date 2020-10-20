#####################################################################################
#                    -- ExoLC-BirlestirCiz.R - 05.12.2015 --                        #
# HESAP:: YOK                                                                       #
# CIZIM:: Dizindeki .cikti uzantili isik egrisi verisi dosyalarini okur, once       #
# siralar; sonra iki yontemden birine gore BIN (ortaklama) uygular,                 #
# ve ust uste cizdirir.                                                             #
#####################################################################################

### --BUYUK HARFLERLE-- YAZILAN BASLIKLAR ICINDEKI GIRDILER DUZENLENMELIDIR.      ###
### Kucuk Harflerle yazilan basliklar altinda islemler yapilir.                   ###

####################
### Kutuphaneler ###
library(scales) # renk saydamligi -alpha() icin kullaniliyor

setwd("D:/Akademik/TEZ/DR/ExoLCA/TrES-3")
# GIRDI LISTESI: Disarida birakilacak dosyanin [.cikti] uzantisi silinir.
dosyalar <- list.files(path = ".", pattern = "*.cikti", all.files = FALSE)
# Hedeflenen nokta sayisi girdisi icin bin_to kullailmali; zaman araligina gore bin yapilacaksa bin_size!=0 olmalidir (bin_to yok sayilir)
bin_to = 100  # per point
bin_size = 50  # in minute

##########################
### --GRAFIK CIKTISI-- ###
# pdf(), png(), postscript(): saydamligi desteklemez: pdf'i eps'ye cevir.
yazdir <- 0 
if(yazdir){
pdf(paste0("mean_tres3b_R",".pdf"))#, width = 40, height = 30)
# En altta dev.off komutunu kapatmayi unutma
}

###################
### --AYARLAR-- ###
# Metin
bicem = "serif"  # "serif", "sans", "mono", "symbol"
gorun = 1 # 1:plain, 2:bold, 3: italic
metinboyut <- 18

# Grafik
graftur <- "p"
geometriciz <- T
icerikboyut <- 0.7
kalinlik <- 2
noktatur <- 16
noktarenk <- alpha("black",1)
modelboyut <- 2
modelrenk <- alpha("red",1)

# Eksen
xi <- -0.08
xs <- 0.08
dx_ana <- 0.04
dx_ara <- 0.01
ustbosluk <- 0.04
yi <- 0.96
ys <- 1.00
dy_ana <- 0.02
dy_ara <- 0.005
anaciz <- 0.8
araciz <- 0.4
kesisme <- "i" # Eksenlerin kesisme durumu:  "r" (regular), "i" (internal)

# Eksen basliksiz cizim: 0 ---Bu kisim tekli cizim yapan kodlara aktarilip buradan kaldirilacak
grafikbasligi <- 0
eksenbasligi <- 1
if(eksenbasligi){
eksenbas_y <- "Normalised Flux"   # "Normalised Flux"
eksenbas_x <- "Time from mid-transit (day)"  # "Phase"
kenaralt <- 5
kenarsol <- 6
kenarust <- 1
kenarsag <- 1
} else {
eksenbas_y <- ""
eksenbas_x <- ""
kenaralt <- 2
kenarsol <- 4
kenarust <- 1
kenarsag <- 1
}
### --AYARLAR-- ###
###################

####################
### --Veri Oku-- ###  
rm(tumveri); tumveri <- NA
for(i in 1:length(dosyalar)){
veri <- read.table(dosyalar[i], header=FALSE, sep="\t")[ ,1:3] # 1-3 arasi sutunlar okunuyor
tumveri <- rbind(tumveri, veri)
rm(veri) 
}
# Ilk sutuna gore tum veriyi sirala     
tumveri <- na.omit(tumveri[order(tumveri$V1),])

if(bin_size){
bin_to <- length(tumveri[,1])/(bin_size*(tumveri[length(tumveri[,1]),1]-tumveri[1,1])*24*60/length(tumveri[,1]))}
x <- na.omit(tumveri[,1])
y <- na.omit(tumveri[,2])
e <- na.omit(tumveri[,3])
x <- colMeans(matrix(x, round(length(x)/bin_to)), na.rm=TRUE)
y <- colMeans(matrix(y, round(length(y)/bin_to)), na.rm=TRUE)
e <- colMeans(matrix(e, round(length(e)/bin_to)), na.rm=TRUE)
 
### --Veri Oku-- ###
####################

########################                     
### --Geçiþ Eðrisi-- ###
par(mar=c(kenaralt,kenarsol,kenarust,kenarsag), 
     cex=icerikboyut, ps=metinboyut, font=gorun, family=bicem, 
     pch=noktatur, bg=NA, las=1, lwd=kalinlik, xaxs=kesisme, yaxs=kesisme)

# Ilk siradaki cizilir:
plot(x, y, type=graftur, xlab="", ylab="", axes=F,   
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

if(grafikbasligi){
# Grafik Basligi
text(x=0, y=1.0150, label="Combined data of TrES-3 b")
text(x=0, y=1.0125, label="Bessel R")
} 
### --Geçiþ Eðrisi-- ###
########################

######################                     
### --Model Eðri-- ###
model <- read.table(paste0(getwd(),"/WinFitter/mean_tres3b_R-Run1.mod"),
                     header=FALSE, skip=3, sep="", na.strings="-99.0000000")
lines(model$V1/360, model$V2+(1-model$V2[1]), col=modelrenk, lwd=modelboyut)  
print(1-model$V2[1])
### --Model Eðri-- ###                        
######################

########################                     
### Gecis Geometrisi ###
if(geometriciz){
# Eksen
olcek <- 1
# Dikey eksende kaydirma parametreleri
ggyi <- -3.0
ggys <- 0.0
# Geometrik parametreler
olcek <- 1
a <- 5.03867
b <- 0.83898
r1 <- 0.16866
r2 <- 0.02920

library(shape)
par(new=TRUE, fig=c(0, 1, 0, 1))
# x ekseni evreye uyduruldu: .25 evre birim eksen uzunluguna karsilik geldigi icin 4 kati yazilir (4xi), (birim uzunluk x gercek yari-buyuk eksen) degerinden gezegenin kendisi (cap uzunlugu) cikarilir.
emptyplot(c(4*xi*a-(2*r2*a), 4*xs*a+(2*r2*a)), c(ggyi, ggys))

plotellipse(rx=a*olcek, ry=(b*r1*a)*olcek, angle=0, from=0, to=pi, lwd=2, lty="solid")
plotcircle(r=(r1*a)*olcek, col="gray80", lwd=0)
plotellipse(rx=a*olcek, ry=(b*r1*a)*olcek, angle=0, from=pi, to=0, lwd=2, lty="solid")
plotcircle(r=(r2*a)*olcek, mid=c(0, -(b*r1*a)*olcek), col="black", lwd=0)
lines(c(-olcek*a,olcek*a), c(0,0), lty="dotted")

#axis(side=1, pos=0, at=seq(-10,10,by=0.04))
axis(side=2, at=seq(0,1,by=0.5), las=1, tcl=anaciz, lwd=par("lwd"))
axis(side=2, at=seq(0,1,by=0.25), las=1, tcl=araciz, lwd=par("lwd"), labels=F)
axis(side=4, at=seq(0,1,by=0.5), las=1, tcl=anaciz, lwd=par("lwd"))
axis(side=4, at=seq(0,1,by=0.25), las=1, tcl=araciz, lwd=par("lwd"), labels=F)
}                                        
### Gecis Geometrisi ###
########################

if(yazdir){
dev.off()
}

#################
### --BILGI-- ###
print("Kullanýlan dosyalar")
dosyalar
### --BILGI-- ###
#################

# DOSYA SONU #