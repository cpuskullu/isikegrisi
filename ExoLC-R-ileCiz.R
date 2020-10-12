#####################################################################################
#                        -- ExoLC-Ciz.R - 05.12.2015 --                             #
# HESAP:: YOK                                                                       #
# CIZIM:: Dizindeki .ciz uzantili isik egrisi verisi dosyalarini okur               #
# ve ayri ayri cizdirir                                                             #
#####################################################################################

### --BUYUK HARFLERLE-- YAZILAN BASLIKLAR ICINDEKI GIRDILER DUZENLENMELIDIR. Kucuk Harflerle yazilan basliklar altinda islemler yapilir. ###

####################
### Kutuphaneler ###
library(scales) # renk saydamligi -alpha() icin kullaniliyor
              
###################
### --AYARLAR-- ###
setwd("D:/Akademik/TEZ/DR/ExoLCA/HATP3/")
dosyaadikisaltmasi <- "HATP3b"
# Girdi Sutunlari: ObsPhase ObsFlux/Mag Error ModPhase ModFlux/Mag ResPhase ResFlux/Mag
# Disarida birakilacak dosyanin [.ciz] uzantisini [.iptal] yapiniz.
dosyalar <- list.files(path = ".", pattern = "*.ciz", all.files = FALSE)
# Girdi Sutunlari: Date/Time Telescope+Dedector
gozlembaslik <- "#HATP3b_GozlemBasliklari.in"
baslik <- read.table(gozlembaslik, header=FALSE, sep = "\t", col.names=c("Tarih", "TelCCD", "Filtre"))
ciktidosya <- paste0("#",dosyaadikisaltmasi,"_R","_Birlestirilmis",".out")
### --AYARLAR-- ###
###################

dosyasayisi <- length(dosyalar)
file.remove(ciktidosya)

# TEK DOSYA CIZDIRMEK ICIN: TRUE
if(T){
dosyalar[1] <- "20140223TUG100_hatp3b_R_SYNFITTER-v32.ciz"
dosyasayisi <- 1
}

###################
### --GORUNUM-- ###
# Metin
bicem = "serif"  # "serif", "sans", "mono", "symbol"
gorun = 1 # 1:plain, 2:bold, 3: italic
metinboyut <- 18

# Grafik
graftur <- "p"
icerikboyut <- 1
kalinlik <- 3
modelkalinlik <- 4
noktatur <- 16
noktarenk <- alpha("black",1)
modelrenk <- alpha("red",1)
hatacubukciz <- "artikta"    # veride, artikta, herkisi, hicbiri
hatacubukrenk <- "gray60"
hatacubukkalinlik <- 1

# Eksen
etiket <- T
xi <- -0.04
xs <- 0.04
dx_ana <- 0.02
dx_ara <- 0.005
dx <- 0.01
yi <- 0.95
ys <- 1.02
dy_ana <- 0.02
dy_ara <- 0.005
ciftsayi <- T
if(ciftsayi){dy <- 0.02} else {dy <- 0.}
anaciz <- 0.8
araciz <- 0.4
kesisme <- "i" # Eksenlerin kesisme durumu:  "r" (regular), "i" (internal)

grafikbasligi <- 1
eksenbasligi <- 1

##############################
### --Satir/Sutun Cizimi-- ###
# Satir-sutun toplu cizim icin T; Tek tek cizim icin F
satsutciz <- F
satirs <- 4
sutuns <- 3
arabosluk <- 0

if(satsutciz){
metinboyut <- 20
icerikboyut <- 0.4
kalinlik <- 1
eksenbasligi <- 1
modelkalinlik <- 2
par(mfrow = c(satirs, sutuns), oma=c(6,6,1,1))  ##  plot.new() skips a position.
etiket <- F; etiket_x <- F; etiket_y <- F
}
### --Satir/Sutun Cizimi-- ###
##############################

if(eksenbasligi){
eksenbas_y <- "Normalised Flux"   # "Normalised Flux"
eksenbas_x <- "Phase"  # "Phase"
kenaralt <- 5
kenarsol <- 6
kenarust <- 3
kenarsag <- 1
} else {
eksenbas_y <- ""
eksenbas_x <- ""
kenaralt <- 2
kenarsol <- 4
kenarust <- 1
kenarsag <- 1
}

if(!etiket){
kenaralt <- 0
kenarsol <- 0
kenarust <- 0
kenarsag <- 0
}

# ArtikEgrisini Ayri Ciz/Cizme
artikegr_ayri <- F
artikegr_orjin <- 0.965
etiket_artik <- "Res."

### --GORUNUM-- ###
###################

for(i in 1:dosyasayisi){

################
### Veri Oku ###
gozlem <- read.table(dosyalar[i], 
                     header=FALSE, sep = "\t", 
                     col.names=c("GE", "GA", "GH", "ME", "MA", "AE", "AA"))

m  <- cbind(paste(gozlem$GE[!is.na(gozlem$GE)]), paste(gozlem$GA[!is.na(gozlem$GA)]), paste(gozlem$GH[!is.na(gozlem$GH)]))
write.table(m, file=ciktidosya, col.names=FALSE, row.names=FALSE, quote=FALSE, append = TRUE, sep = "\t")
##########################
### --GRAFIGI YAZDIR-- ###
# pdf(), png(), postscript(): saydamligi desteklemez: pdf'i eps'ye cevir.
yazdir <- 0 
if(yazdir){
pdf(paste0(dosyalar[i],".pdf"))#, width = 40, height = 30)
# En altta dev.off komutunu kapatmayi unutma
}
### --GRAFIGI YAZDIR-- ###
##########################

########################                     
### --Geçiþ Eðrisi-- ###
par(mar=c(kenaralt+arabosluk,kenarsol+arabosluk,kenarust,kenarsag),
     cex=icerikboyut, ps=metinboyut, font=gorun, family=bicem, cex.axis=1,
     pch=noktatur, bg=NA, las=1, lwd=kalinlik, xaxs=kesisme, yaxs=kesisme)

plot(gozlem$GE, gozlem$GA, type="n", xlab="", ylab="", axes=F,   
     xlim=c(xi, xs), ylim=c(yi, ys), col=noktarenk, main="HAT-P-3b")

if((i %% sutuns)==1) {etiket_y <- T};  if(i>sutuns*(satirs-1)) {etiket_x <- T}
etiket_x <- T
axis(side=1, at=seq(xi+dx,xs,by=dx_ana), tcl=anaciz, lwd=par("lwd"), labels=etiket_x)
axis(side=1, at=seq(xi,xs,by=dx_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=2, at=seq(yi+dy,ys,by=dy_ana), tcl=anaciz, lwd=par("lwd"), labels=etiket_y)
axis(side=2, at=seq(yi,ys,by=dy_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=3, at=seq(xi+dx,xs,by=dx_ana), tcl=anaciz, lwd=par("lwd"), labels=FALSE)
axis(side=3, at=seq(xi,xs,by=dx_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=4, at=seq(yi+dy,ys,by=dy_ana), tcl=anaciz, lwd=par("lwd"), labels=FALSE)
axis(side=4, at=seq(yi,ys,by=dy_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
if(!etiket){etiket_x <- F; etiket_y <- F}

if(!satsutciz){
mtext(eksenbas_x, side=1, line=3)
mtext(eksenbas_y, side=2, line=4, las=0)}
box()

if(hatacubukciz=="veride" || hatacubukciz=="herikisi"){
# Hata Cubuklari: noktalardan once
arrows(gozlem$GE, gozlem$GA-gozlem$GH, gozlem$GE, gozlem$GA+gozlem$GH,  
       code=0, angle=90, col=hatacubukrenk, lwd=hatacubukkalinlik)
}
# Noktalar: hata cubuklarindan sonra
points(gozlem$GE, gozlem$GA, col=noktarenk)

if(grafikbasligi){
# Grafik Basligi
#text(x=0, y=1.0140, label="WASP-43b", cex=1)
text(x=0, y=1.0140, label=baslik[i,1], cex=1)
text(x=0, y=1.0100, label=paste0(baslik[i,2], " (", baslik[i,3],")"), cex=1)
#text(x=0, y=1.0050, label=baslik[i,3])
}

######################                     
### --Model Egri-- ###
lines(gozlem$ME, gozlem$MA, col=modelrenk, lwd=modelkalinlik)

####################                     
### --Artiklar-- ###
if(artikegr_ayri){
par(new=TRUE, mar=c(kenaralt,kenarsol,kenarust+20,kenarsag))
plot(gozlem$AE, gozlem$AA, axes=F, 
     xlim=c(xi, xs), ylim=c(-0.02, 0.02), tcl=anaciz, col=noktarenk)

axis(side=3, at=seq(xi,xs,by=dx_ana), tcl=anaciz, lwd=par("lwd"), labels=FALSE)
axis(side=3, at=seq(xi,xs,by=dx_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
box()
abline(h=0, col=modelrenk, lwd=modelkalinlik)

} else { 
par(new=TRUE)
plot(gozlem$AE, gozlem$AA+artikegr_orjin, type="n", xlab="", ylab="", axes=F,
     xlim=c(xi, xs), ylim=c(yi, ys), col=noktarenk) 

if(hatacubukciz=="artikta" || hatacubukciz=="herikisi"){
# Hata Cubuklari: noktalardan once
arrows(gozlem$AE, gozlem$AA+artikegr_orjin-gozlem$GH, 
     gozlem$AE, gozlem$AA+artikegr_orjin+gozlem$GH, code=0, angle=90, 
     length=0.1, col=hatacubukrenk, lwd=hatacubukkalinlik)
}
# Noktalar: hata cubuklarindan sonra
points(gozlem$AE, gozlem$AA+artikegr_orjin, col=noktarenk)
abline(h=artikegr_orjin, col=modelrenk, lwd=modelkalinlik) 
}

########################                     
### --2.Min Eðrisi-- ###
if(FALSE){
par(fig=c(0.58,0.98,0.16,0.56), new=TRUE)
plot(gozlem$GE2, gozlem$GA2, font=gorun, family=bicem,
     xlab="", ylab="", pch = 16, col="#00000040",
     xlim=c(0.40, 0.60), ylim=c(0.90, 1.10), tcl=0.2, xaxt='n', yaxt='n',
     cex=0.4, cex.lab=0.8, cex.axis=0.6, cex.main=0.5, cex.sub=0.1)

axis(side=1, at = seq(-10, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=1, at = seq(-10, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=2, at = seq(0, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=2, at = seq(0, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=3, at = seq(-10, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=3, at = seq(-10, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=4, at = seq(0, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=4, at = seq(0, 10, by = 0.005), labels=FALSE, tcl=0.1)

#lines(gozlem$ME, gozlem$MA, col="red", lwd="2.4")

smoothingSpline = smooth.spline(gozlem$ME, gozlem$MA, spar=0.75)
lines(smoothingSpline, col="red", lwd="2.4")
}
### --2.Min Eðrisi-- ###
########################                     

if(yazdir){
dev.off() }
}

if(satsutciz){
mtext(eksenbas_x, side=1, line=3, cex=par("cex")+0.2, outer = TRUE)
mtext(eksenbas_y, side=2, line=4, cex=par("cex")+0.2, las=0, outer = TRUE)}
