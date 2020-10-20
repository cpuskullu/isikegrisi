#########################################################################################
#                       -- EvreliVeridenNoktaAt.R - 02.11.2016 --                       #
# GIRDI::                                                                               #
# ISLEM:: YOK                                                                           #
# CIZIM:: YOK                                                                           #
# CIKTI:: Okunan veriden secili sutunlar: Q##TIME_hedefAdi.tbl                          #
#########################################################################################

### --BUYUK HARFLERLE-- YAZILAN BASLIKLAR ICINDEKI GIRDILER DUZENLENMELIDIR. Kucuk Harflerle yazilan basliklar altinda islemler yapilir. ###

####################
### Kutuphaneler ###

###################
### --AYARLAR-- ###
setwd("D:/Akademik/TEZ/DR/ExoLCA/Kepler485 (KOI186)/Veri/Kepler485SC/Extracted Data/")
dosyalar <- list.files(path = ".", pattern = "*.tbl", all.files = FALSE)
hedefAdi <- "Kepler491"
veriTuru <- "SC"

# Ekrana grafiði çizdirilecek sütun numaralarý plot( veri[,#]  vektörü içinde # yerine yazýlmalýdýr.
# OKUNACAK VE ISLENECEK SÜTUNLAR
sutun <- c(1, 2, 3)
evre_eklenecek_sutun <- 4

#kod <- "d"
# Evrelendirme Degerleri
P <- 3.243259796
T0 <- (2454966.6690442 - 2545833.0)
atilacak_veri <- c(); sinirlar <- c()
# Ilgili evrede okunan sacilmis noktalar # Ilk degiskende birden fazla nokta olmalidir!
atilacak_veri[[1]] <- c(-0.019413,-0.067138,-0.076087,0.363879,0.451872)
#sinirlar[[1]] <- c(183,187) # [1] "Q02SC170-200_kepler6.tbl"
atilacak_veri[[2]] <- c(-0.131378,-0.050794,0.096104,0.253383)
atilacak_veri[[3]] <- c(-0.396239,-0.156800,0.033151,0.031659,0.054028,0.326286,0.191471,0.430576)
atilacak_veri[[4]] <- c(0.352856,0.315573)
atilacak_veri[[5]] <- c(-0.279627,0.192395)
atilacak_veri[[6]] <- c(-0.356474,-0.475127,-0.434806,-0.225271,0.414597,0.266174,0.058766)
atilacak_veri[[7]] <- c(-0.032283,0.205576,0.280691,0.248909)
atilacak_veri[[8]] <- c(-0.137846,0.116429,0.404263)
atilacak_veri[[9]] <- c(0.228971,0.236452,0.293125,0.019437,0.037334,0.039589)
atilacak_veri[[10]] <- c(-0.121566,-0.319172,-0.340095,0.283395)
atilacak_veri[[11]] <- c(-0.134641,0.398780)
atilacak_veri[[12]] <- c(-0.276478,-0.406146,-0.280259,-0.157845,-0.043760,0.06354,0.078526,0.144780,0.310507,0.340935,0.458752,0.470683)
atilacak_veri[[13]] <- c(-0.156508,0.011971,0.130564)
atilacak_veri[[14]] <- c(-0.203633,-0.216200,0.421343,0.385503)
atilacak_veri[[15]] <- NULL
atilacak_veri[[16]] <- c(-0.279627,0.192395)
atilacak_veri[[17]] <- c(-0.379451,0.423684,0.476629)           
### --AYARLAR-- ###
###################

# DONGU: i - dosyalar
for(i in 1:length(dosyalar)){

  veri <- read.table(dosyalar[i], header=FALSE, sep = "", stringsAsFactors=FALSE)

  evre <- (veri[,sutun[1]]-T0)/P - as.integer((veri[,sutun[1]]-T0)/P) # as.integer, yapisinin Excel ile farkliligi nedeniyle evre hesabi Excel'den farklidir.
  veri[,evre_eklenecek_sutun] <- round(ifelse(evre<(-0.5), evre+1, ifelse(evre>(0.5), evre-1, evre)),15)

  # VERI KIRPMA
#  veri <- veri[veri[,sutun[1]] > sinirlar[[i]][1] & veri[,sutun[1]] < sinirlar[[i]][2],]

  # VERI ATMA
  # Noktalar, belirleniyor
  atilan_noktalar <- grepl(paste(atilacak_veri[[i]],collapse="|"),veri[,evre_eklenecek_sutun])
  # Noktalar, veriden atiliyor
  veri <- veri[-which(atilan_noktalar),]
  # Gereksiz sutunlar cikariliyor.
  veri$V4 <- NULL;  veri$V5 <- NULL
  
  # BILGI
#  sum(atilan_noktalar)
    
  ciktiAdi <- paste0(strsplit(basename(dosyalar[i]), "\\.")[[1]][1],"-temiz")
  ciktiDosyasi <- paste0(ciktiAdi,".dat") 
  
  # veri, dosyaya yazdiriliyor.
  write.table(veri, file=ciktiDosyasi, col.names=FALSE, row.names=FALSE, quote=FALSE, append=FALSE, sep = "\t")
    
}
####################
### --KOD SONU-- ###
winDialog("ok", paste0("Kod Sonlandý!","\n\nBÝLGÝ: ",
  "\nÝþlenen Dosya Top.Sayýsý = ", length(dosyalar))) 
  #"\nAtýlan Satýr Top.Sayýsý = ", TopGecersizSayisi))
### --KOD SONU-- ###
####################