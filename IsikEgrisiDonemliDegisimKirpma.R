#####################################################################################
#                   -- IsikEgrisiDonemliDegisimKirpma.R - 19.10.2016 --             #
# HESAP:: YOK                                                                       #
# CIZIM:: Sadece göstermelik cizim yapar, kaydetmez.                                #
# CIKTI:: Okunan veriden secili sutunlar: Q##TIME_hedefAdi.tbl                      #
#####################################################################################

### --BUYUK HARFLERLE-- YAZILAN BASLIKLAR ICINDEKI GIRDILER DUZENLENMELIDIR. Kucuk Harflerle yazilan basliklar altinda islemler yapilir. ###

####################
### Kutuphaneler ###
library(reader)
source("D:/Akademik/AstroLib/AstroCodes/R/UserFunctions/clipboard.R")   # copy.table()

###################
### --AYARLAR-- ###
setwd("D:/Akademik/PROJE-MFAG113F353/KeplerLCs/Kepler79 (KOI152)/Veri/Kepler79LC/")
dosyalar <- list.files(path = ".", pattern = "*.tbl", all.files = FALSE)
hedefAdi <- "kepler79"
veriTuru <- "LC"

# Ekrana grafiði çizdirilecek sütun numaralarý plot( veri[,#]  vektörü içinde # yerine yazýlmalýdýr.
# OKUNACAK VE ISLENECEK SÜTUNLAR
sutun <- c(1, 2, 3)
evre_eklenecek_sutun <- 4

# DONEM SAYISI
donem_sayi <- 4
kod <- c(); P <- c(); T0 <- c(); kirp_aralik <- c() 
# cift [[]] kullanimiyla cok boyutlu vektor atamasi yapilabilir.

# DONEMLER
kod[1] <- "b"
P[1] <- 13.484500
T0[1] <- 851.307000
kirp_aralik[[1]] <- c(-0.0090,0.0090)
kod[2] <- "c"
P[2] <- 27.402900
T0[2] <- 873.475000
kirp_aralik[[2]] <- c(-0.0066,0.0066)
kod[3] <- "d"
P[3] <- 52.090200
T0[3] <- 888.011000
kirp_aralik[[3]] <- c(-0.0052,0.0052)
kod[4] <- "e"
P[4] <- 81.065900
T0[4] <- 869.126000
kirp_aralik[[4]] <- c(-0.0033,0.0033)
### --AYARLAR-- ###
###################

# Islenen Veri Sayisi dosyasi
veriSayidosyasi <- paste0(hedefAdi,veriTuru,"-TemizVeriSayisi.dat")
if (file.exists(veriSayidosyasi)) file.remove(veriSayidosyasi)

# DONGU: i - dosyalar
for(i in 1:length(dosyalar)){
donem_atla <- 1

  for(k in 1:donem_sayi){
  veri <- read.table(dosyalar[i], header=FALSE, sep = "", stringsAsFactors=FALSE)
  
    for(j in 1:donem_sayi){
    if(j==donem_atla) next # donem_atla numarali iterasyonu gec.
    
    # Evrelendirme (-0.5 ile 0.5 arasinda)
    evre <- (veri[,sutun[1]]-T0[j])/P[j] - as.integer((veri[,sutun[1]]-T0[j])/P[j]) # as.integer, yapisinin Excel ile farkliligi nedeniyle evre hesabi Excel'den farklidir.
    veri[,evre_eklenecek_sutun] <- round(ifelse(evre<(-0.5), evre+1, ifelse(evre>(0.5), evre-1, evre)),15)
    
    # KIRPMA
    # Araliklar, veriden kirpiliyor
    veri <- veri[!veri[,evre_eklenecek_sutun] > kirp_aralik[[j]][1] | !veri[,evre_eklenecek_sutun] < kirp_aralik[[j]][2],] 
    }
  
  ciktiAdi <- paste0(strsplit(basename(dosyalar[i]), "\\.")[[1]][1],kod[donem_atla])
  ciktiDosyasi <- paste0(ciktiAdi,".dat")
  donem_atla <- donem_atla + 1
  
  # veri, dosyaya yazdiriliyor.
  write.table(veri, file=ciktiDosyasi, col.names=FALSE, row.names=FALSE, quote=FALSE, append=FALSE, sep = "\t")
   
  # Kaydedilen her dosyanin veri sayisi yaziliyor.
  write(paste0(ciktiAdi,"\t",length(veri[,sutun[1]])), file=veriSayidosyasi, append=TRUE, sep="\n")
  
  #copy.table(veri)
  ##########################
  ### --GRAFIGI YAZDIR-- ###          
  # pdf(), png(), postscript(): saydamligi desteklemez: pdf'i eps'ye cevir.
  yazdir <- 0 
  if(yazdir){
  png(paste0(ciktiAdi,".png"))#, width = 40, height = 30)
  # En altta dev.off komutunu kapatmali; bu nedenle 'yazdir' degiseni ile kontrol ediliyor.
  }
  ### --GRAFIGI YAZDIR-- ###
  ##########################
   
  if(yazdir){
  dev.off() }
  
  } # DONGU: k SONU
} # Dosyalar dongu sonu: for()
                           
####################
### --KOD SONU-- ###
winDialog("ok", paste0("Kod Sonlandý!","\n\nBÝLGÝ: ",
  "\nÝþlenen Dosya Top.Sayýsý = ", length(dosyalar))) 
  #"\nAtýlan Satýr Top.Sayýsý = ", TopGecersizSayisi))
### --KOD SONU-- ###
####################
