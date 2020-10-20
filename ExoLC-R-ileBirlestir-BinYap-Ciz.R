#####################################################################################
#             -- ExoLC-Birlestir-BinYap-Ciz.R - 18.11.2016 --                       #
# HESAP:: YOK                                                                       #
# CIZIM:: Dizindeki .ciz uzantili isik egrisi verisi dosyalarini okur, once         #
# siralar; sonra iki yontemden birine gore BIN (ortaklama) uygular,                 #
# ve ust uste cizdirir. Istenirse gecis geometrisi cizdirilebilir.                  #
#                                                                                   #
# UYARI:: .ciz dosyalari, evreli veri icerdigi icin BIN islemi evreli veri          #
# uzerinden uygulanir                                                               #
#####################################################################################
#  sessionInfo()
### --BUYUK HARFLERLE-- YAZILAN BASLIKLAR ICINDEKI GIRDILER DUZENLENMELIDIR.      ###
### Kucuk Harflerle yazilan basliklar altinda islemler yapilir.                   ###

####################
### Kutuphaneler ###
library(scales) # renk saydamligi -alpha() icin kullaniliyor
library(Cairo) # Turkce karakterler icin en uygun aygit.
#library(extrafont)
#loadfonts()
options(scipen=5) # Bilimsel gosterim siniri

winfitter_ver <- 2.6
setwd("D:/Akademik/TEZ/DR/ExoLCA/Kepler491 (KOI201)")
dosyaadikisaltmasi <- "Kepler491b"
# GIRDI LISTESI: Disarida birakilacak dosyanin [.ciz] uzantisi silinir.
dosyalar <- list.files(path = ".", pattern = "*b_SYNFITTER-v32.ciz", all.files = FALSE)
verievreli <- TRUE # Girdi verisi evrelendirilmis mi?

# PARAMETRELER DOSYASI
paramdosyasi <- paste0(dosyaadikisaltmasi,"_Parametreler.in")
parametre <- read.table(paramdosyasi, skip=1, header=TRUE, sep="\t")
T0 <- parametre$T0
P <- parametre$P
a <- parametre$a.Ro.  # [Ro] biriminde
b <- parametre$b
r1 <- parametre$r1
r2 <- parametre$r2
inc <- acos(b*r1) # radyan # derece icin x 180/pi

# BINNING: Veride zamana gore binning yuksek degerlerle yapilmali; aksi halde isik egrisi yapisi bozulmaktadir; her bir veri seti birlestirilmeden tek tek binning yapmak yine isik egrisi yapisini bozmaktadir. Evrelendirilmis veri kullanildiginda ayni zamana karsilik gelen nokta sayisi arttigi icin binning daha saglikli olmaktadir.  
# Hedeflenen nokta sayisi girdisi icin bin_to kullanilmali; zaman araligina gore bin yapilacaksa bin_size!=0 olmalidir (bin_to yok sayilir)
bin_to = 4800  # per point
bin_size = 0  # in minute
min2 <- F # 2.minimum cizilsin mi?
artikegr_ciz <- T
geometriciz <- T

##########################
### --GRAFIK CIKTISI-- ###
# pdf(), png(), postscript(): saydamligi desteklemez: pdf'i eps'ye cevir.
yazdir <- 1
if(yazdir){
  if(geometriciz){
  cairo_pdf(paste0(dosyaadikisaltmasi,"_Geometri",".pdf"), width=8, height=12)}
  #CairoPDF(paste0("mean_tres3b_R",".pdf")) # Helvetica disinda font ayari yapilamiyor!
  else if(bin_to){
    cairo_pdf(paste0(dosyaadikisaltmasi,"_Bin",bin_to,"nokta",".pdf"), width=8, height=10)}
    else{
      cairo_pdf(paste0(dosyaadikisaltmasi,"_Bin",bin_size,"dk",".pdf"), width=8, height=10)}
# En altta dev.off komutunu kapatmayi unutma
}

###################
### --GORUNUM-- ###
# Metin ---
bicem = "serif"  # "serif", "sans", "mono", "symbol"
gorun = 1 # 1:plain, 2:bold, 3: italic
metinboyut <- 24

# Grafik ---
graftur <- "p"
icerikboyut <- 1.1
kalinlik <- 2
noktatur <- 16
noktarenk <- alpha("black",1)
modelboyut <- 3
modelrenk <- alpha("red",1)
binnedmodelrenk <- "grey40"   # ayni zamanda artiklar icin kullanilir

# Eksen
xi <- (-0.04)
xs <- 0.04
dx_ana <- 0.02
dx_ara <- 0.005
yi <- 0.9990
ys <- 1.0003
dy_ana <- 0.0002
dy_ara <- 0.0001
# Birim Cizgisi
anaciz <- 0.8
araciz <- 0.4
kesisme <- "i" # Eksenlerin kesisme durumu:  "r" (regular), "i" (internal)

# Eksen basliksiz cizim: 0 ---Bu kisim tekli cizim yapan kodlara aktarilip buradan kaldirilacak
grafikbasligi <- 0
eksenbasligi <- 1

if(eksenbasligi){
eksenbas_y <- "Normalize Aký"   # "Normalised Flux"
eksenbas_x <- "Evre"  # "Phase"
kenaralt <- 7
kenarsol <- 10
kenarust <- 2
kenarsag <- 3
kenarlar_bosluklu <- c(kenaralt,kenarsol,kenarust,kenarsag)
} else {
eksenbas_y <- ""
eksenbas_x <- ""
kenaralt <- 2
kenarsol <- 4
kenarust <- 1
kenarsag <- 1
}
if(geometriciz){
# mfrow esit bolmeler olusturur
#par(mfrow=c(2,1), oma=c(kenaralt,kenarsol,kenarust,kenarsag))
#kenaralt <- 0; kenarsol <- 0; kenarust <- 0; kenarsag <- 0; 
#kenarlar_bosluksuz <- c(kenaralt,kenarsol,kenarust,kenarsag)
layout(matrix(c(1,1,1,2,2,2,3,3), nrow=8, ncol=1, byrow=TRUE), widths=c(1)) 
kenaralt <- 0
kenarlar_bosluksuz <- c(kenaralt,kenarsol,kenarust,kenarsag)
} else if(artikegr_ciz){
lymtx <- matrix(c(1,1,2), nrow=3, ncol=1, byrow=TRUE); layout(lymtx)
kenaralt <- 0
kenarlar_bosluksuz <- c(kenaralt,kenarsol,kenarust,kenarsag)
}
### --GORUNUM-- ###
###################

####################
### --Veri Oku-- ###  
tumveri <- NULL
for(i in 1:length(dosyalar)){
veri <- read.table(dosyalar[i], header=FALSE, sep="\t")[ ,1:3] # 1-3 arasi sutunlar okunuyor
tumveri <- rbind(tumveri, veri)
print(dosyalar[i])
rm(veri) 
}
# Ilk sutuna gore tum veriyi sirala     
tumveri <- na.omit(tumveri[order(tumveri$V1),])
write.table(tumveri, file=paste0("#",dosyaadikisaltmasi,"_Birlestirilmis.out"), col.names=FALSE, row.names=FALSE, quote=FALSE, sep = "\t")

if(bin_size){
bin_to <- length(tumveri[,1])/(bin_size*(tumveri[length(tumveri[,1]),1]-tumveri[1,1])*24*60/length(tumveri[,1]))}
x <- na.omit(tumveri[,1])
y <- na.omit(tumveri[,2])
e <- na.omit(tumveri[,3])
x <- colMeans(matrix(x, round(length(x)/bin_to)), na.rm=TRUE)
y <- colMeans(matrix(y, round(length(y)/bin_to)), na.rm=TRUE)
e <- colMeans(matrix(e, round(length(e)/bin_to)), na.rm=TRUE)

m  <- cbind(x, y)
if(!verievreli){
    evre <- (m[,1]-T0)/P - as.integer((m[,1]-T0)/P) # as.integer, yapisinin Excel ile farkliligi nedeniyle evre hesabi Excel'den farklidir.
    # Evrelendirme (-0.5 ile 0.5 arasinda)
    x <- round(ifelse(evre<(-0.5), evre+1, ifelse(evre>(0.5), evre-1, evre)),15)
    y <- m[,2]
} 
  if(bin_to){
    write.table(m, file=paste0("#",dosyaadikisaltmasi,"_BirlestirilmisBinned",bin_to,"nokta",".out"), col.names=FALSE, row.names=FALSE, quote=FALSE, sep = "\t") } else { 
    write.table(m, file=paste0("#",dosyaadikisaltmasi,"_BirlestirilmisBinned",bin_size,"dk",".out"), col.names=FALSE, row.names=FALSE, quote=FALSE, sep = "\t") }
    
### --Veri Oku-- ###
####################

par(mar=c(kenaralt,kenarsol,kenarust,kenarsag),
     cex=icerikboyut, ps=metinboyut, font=gorun, family=bicem,
     pch=noktatur, bg=NA, las=1, lwd=kalinlik, xaxs=kesisme, yaxs=kesisme)
opar <- par()

########################
### Gecis Geometrisi ###
if(geometriciz){
# Dikey eksende kaydirma parametreleri
kaydir <- 0 # negatif deger, yukari; pozitif deger asagi yonde kaydirir
olcek <- 1 # gezegen merkezi temel alinarak yapilan zaman olceginde, isik egrisi evresi ile geometri uyumu icin olcek degerinin 0.75 olmasi gerekiyor. Fakat bu durumda yaricap kuculdugu icin, yaricap ekseni uyumsuz gorunuyor. Winn et al. (2011)

library(shape)
# x ekseni evreye uyduruldu: .25 evre birim eksen uzunluguna karsilik geldigi icin 4 kati yazilir (4xi), (birim uzunluk x gercek yari-buyuk eksen) degerinden gezegenin kendisi (cap uzunlugu) cikarilir.
# Bu dogru cizim verir: -0.25-0.25 evre arasinda yorunge tam sinirlarda olmalidir; fakat yakin cizim sorunu olusur.

# HESAP:1 (Calisti: TAMAMI, tam giris-cikis ani isik egrisiyle eslesmiyor)
# xbas <- 4*xi*a-(2*r2*a); xson <- 4*xs*a+(2*r2*a) # ilk hesap
xbas <- 4*xi*a-(2*r2*a/b); xson <- 4*xs*a+(2*r2*a/b)  # giris-cikis uydurma icin (2*r2*a/b) ilk kats.
emptyplot(xlim=c(xbas, xson), ylim=c(0,0)+kaydir)

# Tt <- asin(r1*sqrt((1+(r2/r1))^2-b^2)/sin(inc))/pi # t4-t1 toplam gecis suresi
# geox icinde paydadaki ifade sihirli hesaptir; isik egrisi uyumu, orantiyi saglayip nasil bir iliski icerdigine dair fikrim yok! -0.25-0.25 evre arasi bakildiginda sinirlarla iliskili degildir. Dolayisiyla sadece yorunge icin yanlistir diyebiliriz.

# HESAP:2 (Calisti: Kepler2)
# geox <- (a*b/(4/((xs-xi)/0.04))); emptyplot(xlim=c(-geox, geox), ylim=c(0,0)+kaydir)

plotellipse(rx=a*olcek, ry=(b*r1*a)*olcek, angle=0, from=0, to=pi, lwd=1, lcol="gray90", lty="solid")
plotcircle(r=(r1*a)*olcek, col="gray80", lwd=0)
plotellipse(rx=a*olcek, ry=(b*r1*a)*olcek, angle=0, from=pi, to=0, lwd=1, lcol="gray90", lty="solid")
plotcircle(r=(r2*a)*olcek, mid=c(0, -(b*r1*a)*olcek), col="black", lwd=0)
#lines(c(-olcek*a,olcek*a), c(0,0), col="gray50", lwd=1, lty="dotted")
#box()

axis(side=2, at=seq(-10,10,by=0.25), lwd=par("lwd"), lwd.ticks=0, labels=F)
axis(side=2, at=seq(0,1.5,by=0.5), las=1, tcl=anaciz, lwd=par("lwd"))
axis(side=2, at=seq(0,1.5,by=0.25), las=1, tcl=araciz, lwd=par("lwd"), labels=F)
axis(side=3, at=seq(-10,10,by=0.25), lwd=par("lwd"), lwd.ticks=0, labels=F)
axis(side=3, at=seq(xbas,xson,by=(xson/((xs-xi)/(2*dx_ana)))), tcl=anaciz, lwd=par("lwd"), labels=F)
axis(side=3, at=seq(xbas,xson,by=(xson/((xs-xi)/(2*dx_ara)))), tcl=araciz, lwd=par("lwd"), labels=F)
#axis(side=3, at=seq(-geox,geox,by=(geox/((xs-xi)/(2*dx_ana)))), tcl=anaciz, lwd=par("lwd"), labels=F)
#axis(side=3, at=seq(-geox,geox,by=(geox/((xs-xi)/(2*dx_ara)))), tcl=araciz, lwd=par("lwd"), labels=F)
axis(side=4, at=seq(-10,10,by=0.25), lwd=par("lwd"), lwd.ticks=0, labels=F)
axis(side=4, at=seq(0,1.5,by=0.5), las=1, tcl=anaciz, lwd=par("lwd"), labels=F)
axis(side=4, at=seq(0,1.5,by=0.25), las=1, tcl=araciz, lwd=par("lwd"), labels=F)

#mtext("\u2605", side=2, line=4, cex=par("cex")+0.1, las=0)
#mtext(paste0("R",expression('\u2605')), side=2, line=4, cex=par("cex")+0.1, las=0)
mtext(expression(paste(italic(R)["\u2217"]," (",italic(R)[o],")")), side=2, line=kenarlar_bosluklu[2]-4, cex=par("cex")+0.1, las=0, at=1)
#mtext(quote("R"*"\u2605"), side=2, line=4, cex=par("cex")+0.1, las=0)
#mtext(bquote(R~"\u2605"), side=2, line=4, cex=par("cex")+0.1, las=0)
#mtext(expression(symbol("\043")), side=2, line=4, cex=par("cex")+0.1, las=0)
}                                        
### Gecis Geometrisi ###
########################

########################                     
### --Geçiþ Eðrisi-- ###
if(geometriciz){
kenarust <- 0
par(mar=c(kenaralt,kenarsol,kenarust,kenarsag))
}
plot(x, y, type=graftur, xlab="", ylab="", axes=F,   
     xlim=c(xi, xs), ylim=c(yi, ys), col=noktarenk)

if(!artikegr_ciz){
axis(side=1, at=seq(xi,xs,by=dx_ana), tcl=anaciz, lwd=par("lwd"), padj=1)
axis(side=1, at=seq(xi,xs,by=dx_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)}
axis(side=2, at=seq(yi+dy_ana,ys+dy_ana,by=dy_ana), tcl=anaciz, lwd=par("lwd"))
axis(side=2, at=seq(yi,ys,by=dy_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=2, at=seq(yi,ys,by=dy_ara), lwd=par("lwd"), lwd.ticks=0, labels=FALSE)
if(!geometriciz){
axis(side=3, at=seq(xi,xs,by=dx_ana), tcl=anaciz, lwd=par("lwd"), labels=FALSE)
axis(side=3, at=seq(xi,xs,by=dx_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)}
axis(side=4, at=seq(yi+dy_ana,ys+dy_ana,by=dy_ana), tcl=anaciz, lwd=par("lwd"), labels=FALSE)
axis(side=4, at=seq(yi,ys,by=dy_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=4, at=seq(yi,ys,by=dy_ara), lwd=par("lwd"), lwd.ticks=0, labels=FALSE)

if(!artikegr_ciz){
mtext(eksenbas_x, side=1, line=kenarlar_bosluklu[1]-2, cex=par("cex")+0.1)}
mtext(eksenbas_y, side=2, line=kenarlar_bosluklu[2]-3, cex=par("cex")+0, las=0)

if(grafikbasligi){
# Grafik Basligi
text(x=0, y=1.0150, label="Combined data of TrES-3 b")
text(x=0, y=1.0125, label="Bessel R")
} 
### --Geçiþ Eðrisi-- ###
########################

######################                     
### --Model Eðri-- ###
# Egride yukari-aþagi tasma olursa na.string degerini dogru degerle degistir

# Bin yapilmis verinin modeli
binnedmodeldosyasi <- paste0(getwd(),"/WinFitter v",winfitter_ver,"/",dosyaadikisaltmasi,"-mean_LN-Run1.mod")
if(file.exists(binnedmodeldosyasi)){
  binnedmodel <- read.table(binnedmodeldosyasi, header=FALSE, skip=3, sep="", na.strings="-99.0000000")
  lines(if(winfitter_ver<3){binnedmodel$V1/360} else{binnedmodel$V1}, binnedmodel$V2+(1-binnedmodel$V2[1]), col=binnedmodelrenk, lwd=modelboyut)
}

# Ortalamalar modeli
ortmodeldosyasi <- paste0(getwd(),"/WinFitter v",winfitter_ver,"/",dosyaadikisaltmasi,"-mean_LN-Run1.mod")
model <- read.table(ortmodeldosyasi, header=FALSE, skip=3, sep="", na.strings="-99.00000000")
lines(if(winfitter_ver<3){model$V1/360} else{model$V1}, model$V2+(1-model$V2[1]), col=modelrenk, lwd=modelboyut)

# Gosterge
if(FALSE){
  gosterge <- c("Bin uygulanan veri modeli","Aðýrlýklý Ortalama ile hesaplanan model")
  gostergerenk <- c(binnedmodelrenk,modelrenk)
  legend("bottomright", gosterge, lty=c(1,1), col=gostergerenk,
     inset=.025, bty = "n", box.lty=0, cex=0.8, text.font=1, text.col="black",  bg='gray50',
     y.intersp=1.8, x.intersp = 2.5)
}
### --Model Eðri-- ###                        
######################

#########################
### --Artiklar Eðri-- ### 
if(artikegr_ciz){
artiklardosyasi <- paste0(getwd(),"/WinFitter v",winfitter_ver,"/",dosyaadikisaltmasi,"-mean_LN-Run1.dif")
artiklar <- read.table(artiklardosyasi, header=FALSE, skip=3, sep="", na.strings="-99.0000000")

if(winfitter_ver<3){artiklar$V1 <- artiklar$V1/360}
artiklar_x <- append(artiklar$V1,artiklar$V1-1,after=length(artiklar$V1))
artiklar_y <- append(artiklar$V2,artiklar$V2,after=length(artiklar$V2))
yi <- -0.001; ys <- 0.001; dy_ana <- 0.0005; dy_ara <- 0.00025

kenarust <- 0; kenaralt <- kenarlar_bosluklu[1]
par(mar=c(kenaralt,kenarsol,kenarust,kenarsag))
plot(artiklar_x, artiklar_y, type=graftur, xlab="", ylab="", axes=F, xlim=c(xi, xs), ylim=c(yi, ys), col=binnedmodelrenk)
axis(side=1, at=seq(xi,xs,by=dx_ana), tcl=anaciz, lwd=par("lwd"), padj=1)
axis(side=1, at=seq(xi,xs,by=dx_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=2, at=seq(yi+dy_ana,ys-dy_ana,by=dy_ana), tcl=anaciz, lwd=par("lwd"), cex.axis=0.8)
axis(side=2, at=seq(yi,ys,by=dy_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=3, at=seq(xi,xs,by=dx_ana), tcl=anaciz, lwd=par("lwd"), labels=FALSE)
axis(side=3, at=seq(xi,xs,by=dx_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=4, at=seq(yi,ys,by=dy_ana), tcl=anaciz, lwd=par("lwd"), labels=FALSE)
axis(side=4, at=seq(yi,ys,by=dy_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)

mtext(eksenbas_x, side=1, line=kenarlar_bosluklu[1]-2, cex=par("cex")+0.1)
mtext("Artýklar", side=2, line=kenarlar_bosluklu[2]-3, cex=par("cex")-0.1, las=0)
}
### --Artiklar Eðri-- ###
#########################
if(yazdir){
dev.off()
}

########################
### --2.Min Eðrisi-- ### 
if(min2){ 
x2 <- append(x,x+1,after=length(x))
y2 <- append(y,y,after=length(y))
if(yazdir){
  if(bin_to){
    cairo_pdf(paste0(dosyaadikisaltmasi,"_Bin",bin_to,"nokta-Min2",".pdf"), width = 8, height = 10)}
  else{
    cairo_pdf(paste0(dosyaadikisaltmasi,"_Bin",bin_size,"dk-Min2",".pdf"), width = 8, height = 10)}
} else {dev.new()}

if(artikegr_ciz){
layout(lymtx)
kenaralt <- 0
kenarsol <- kenarlar_bosluklu[2]
kenarust <- kenarlar_bosluklu[3]
kenarsag <- kenarlar_bosluklu[4]
} else {
#if(geometriciz){
kenaralt <- kenarlar_bosluklu[1]
kenarsol <- kenarlar_bosluklu[2]
kenarust <- kenarlar_bosluklu[3]
kenarsag <- kenarlar_bosluklu[4]
}

par(mar=c(kenaralt,kenarsol,kenarust,kenarsag), 
     cex=icerikboyut, ps=metinboyut, font=gorun, family=bicem, 
     pch=noktatur, bg=NA, las=1, lwd=kalinlik, xaxs=kesisme, yaxs=kesisme)

xi <- xi+0.5; xs <- xs+0.5
# xi <- 0; xs <- 1; dx_ana <- 0.25; dx_ara <- 0.125
yi <-0.9998; ys <- 1.0002; dy_ana <- 0.0001

plot(x2, y2, type=graftur, xlab="", ylab="", axes=F,   
     xlim=c(xi, xs), ylim=c(yi, ys), col=noktarenk)
if(!artikegr_ciz){
axis(side=1, at=seq(xi,xs,by=dx_ana), tcl=anaciz, lwd=par("lwd"), padj=1)
axis(side=1, at=seq(xi,xs,by=dx_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)}
axis(side=2, at=seq(yi+dy_ana,ys,by=dy_ana), tcl=anaciz, lwd=par("lwd"))
axis(side=2, at=seq(yi,ys,by=dy_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=3, at=seq(xi,xs,by=dx_ana), tcl=anaciz, lwd=par("lwd"), labels=FALSE)
axis(side=3, at=seq(xi,xs,by=dx_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=4, at=seq(yi,ys,by=dy_ana), tcl=anaciz, lwd=par("lwd"), labels=FALSE)
axis(side=4, at=seq(yi,ys,by=dy_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)

if(!artikegr_ciz){
mtext(eksenbas_x, side=1, line=kenarlar_bosluklu[1]-2, cex=par("cex")+0.1)}
mtext(eksenbas_y, side=2, line=kenarlar_bosluklu[2]-3, cex=par("cex")+0.1, las=0)

lines(if(winfitter_ver<3){binnedmodel$V1/360} else{binnedmodel$V1}, binnedmodel$V2+(1-binnedmodel$V2[1]), col=binnedmodelrenk, lwd=modelboyut)
lines(if(winfitter_ver<3){model$V1/360} else{model$V1}, model$V2+(1-model$V2[1]), col=modelrenk, lwd=modelboyut)

if(FALSE){
    legend("bottomright", gosterge, lty=c(1,1), col=c(binnedmodelrenk,modelrenk),
     inset=.025, bty = "n", box.lty=0, cex=0.8, text.font=1, text.col="black", bg='gray50',
     y.intersp=1.8, x.intersp = 2.5)
  }
###############################
### --2.Min Artiklar Eðri-- ### 
if(artikegr_ciz){
artiklar_x <- artiklar$V1 #append(artiklar$V1,artiklar$V1-1,after=length(artiklar$V1))
artiklar_y <- artiklar$V2 #append(artiklar$V2,artiklar$V2,after=length(artiklar$V2))

yi <- -0.0003; ys <- 0.0003; dy_ana <- 0.00015; dy_ara <- 0.00005

kenarust <- 0; kenaralt <- kenarlar_bosluklu[1]
par(mar=c(kenaralt,kenarsol,kenarust,kenarsag))
plot(artiklar_x, artiklar_y, type=graftur, xlab="", ylab="", axes=F, xlim=c(xi, xs), ylim=c(yi, ys), col=binnedmodelrenk)
axis(side=1, at=seq(xi,xs,by=dx_ana), tcl=anaciz, lwd=par("lwd"), padj=1)
axis(side=1, at=seq(xi,xs,by=dx_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=2, at=seq(yi+dy_ana,ys-dy_ana,by=dy_ana), tcl=anaciz, lwd=par("lwd"), cex.axis=0.8)
axis(side=2, at=seq(yi,ys,by=dy_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=3, at=seq(xi,xs,by=dx_ana), tcl=anaciz, lwd=par("lwd"), labels=FALSE)
axis(side=3, at=seq(xi,xs,by=dx_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)
axis(side=4, at=seq(yi,ys,by=dy_ana), tcl=anaciz, lwd=par("lwd"), labels=FALSE)
axis(side=4, at=seq(yi,ys,by=dy_ara), tcl=araciz, lwd=par("lwd"), labels=FALSE)

mtext(eksenbas_x, side=1, line=kenarlar_bosluklu[1]-2, cex=par("cex")+0.1)
mtext("Artýklar", side=2, line=kenarlar_bosluklu[2]-3, cex=par("cex")-0.1, las=0)
}
### --2.Min Artiklar Eðri-- ### 
###############################
}
### --2.Min Eðrisi-- ### 
########################
if(yazdir){
dev.off()
}

####################
### --KOD SONU-- ###
winDialog("ok", paste0("Kod Sonlandý!","\n\nBÝLGÝ: ",
  "\nÝþlenen Dosya Top.Sayýsý = ", length(dosyalar)))
### --KOD SONU-- ###
####################