#########################################################################################
#                         -- VeridenNoktaAt.R - 07.11.2016 --                           #
# GIRDI:: *.tbl: [KeplerVeriOKU ile] Ayiklanmis BJD, Aki, Hata verisi [3 sutun]         #
# ISLEM:: <nokta_atma_islemi> ile manual/liste secenekleriyle nokta temizligi yapilir.  #            #         manual: ekrana cizilien grafik uzerinden nokta secilerek                      #
#         liste: kod icinde hazirlanmis liste kullanilarak                              #
# CIZIM:: <nokta_atma_islemi>: manual icin BJD-Aký grafigi cizer, aralik ... belirlenir #            #         temizleme islemi biten grafigi *.png olarak kaydeder.                         #            #                                                                                       #
# CIKTI:: *.ctbl: temizlenen veri                                                       #
#########################################################################################

### --BUYUK HARFLERLE-- YAZILAN BASLIKLAR ICINDEKI GIRDILER DUZENLENMELIDIR. Kucuk Harflerle yazilan basliklar altinda islemler yapilir. ###

####################
### Kutuphaneler ###

###################
### --AYARLAR-- ###
setwd("J:/Akademik/Araþtýrma Projeleri/2019 - BAP (Ortak)/Veri/KOI0125/Kepler468SC")
dosyalar <- list.files(path = ".", pattern = "*.tbl", all.files = FALSE)
hedefAdi <- "Kepler468"
veriTuru <- "SC"

# Ekrana grafiði çizdirilecek sütun numaralarý plot(veri[,#])  vektörü içinde # yerine yazýlmalýdýr.
# OKUNACAK VE ISLENECEK SÜTUNLAR
sutun <- c(1, 2, 3) # Veride kullanilacak sutunlar, listelenir.
evre_eklenecek_sutun <- 4 # Evre hesabi yapildiginda hangi siraya eklenecegini gosterir. Bos bir sutun olmalidir.
# Nokta atma/silme islemi, grafik uzerinden yapilacaksa 'manual', asagidaki liste ile yapilacaksa 'liste' girilmelidir. Listeden silme islemi icin noktalar, once evrelendirilmeli, sonra baska grafik programindan kontrol edilerek yazilmali. 
nokta_atma_islemi <- "manual" # manual/liste

if(nokta_atma_islemi == "liste"){
# kod <- "d"
# Evrelendirme Degerleri
P <- 38.47876629
T0 <- (2454833+151.852812 - 2454833)
atilacak_veri <- c(); sinirlar <- c()
# Ilgili evrede okunan sacilmis noktalar, dosya sayisi kadar degisken olmalidir.
# Ilk degiskende birden fazla nokta olmalidir!
atilacak_veri[[1]] <- NULL # [1] Q02SC170-200_kepler6.tbl
sinirlar[[1]] <- c(124,127) # (alt_sinir,ust_sinir)
atilacak_veri[[2]] <- NULL # [2] Q02SC200-230_kepler6.tbl
sinirlar[[2]] <- c(146,149)
atilacak_veri[[3]] <- NULL # [3] Q02SC231-258_kepler6.tbl
sinirlar[[3]] <- c(188,191)
atilacak_veri[[4]] <- NULL # [4] Q02SC231-258_kepler6.tbl
sinirlar[[4]] <- c(219,222)
atilacak_veri[[5]] <- NULL
sinirlar[[5]] <- c(245,248)
atilacak_veri[[6]] <- NULL
sinirlar[[6]] <- c(267,270)
atilacak_veri[[7]] <- NULL
sinirlar[[7]] <- c(305,308)
atilacak_veri[[8]] <- NULL
sinirlar[[8]] <- c(334,337)
atilacak_veri[[9]] <- NULL
sinirlar[[9]] <- c(360,363)
atilacak_veri[[10]] <- NULL
sinirlar[[10]] <- c(405,408)
atilacak_veri[[11]] <- NULL
sinirlar[[11]] <- c(428.5,431.5)
atilacak_veri[[12]] <- NULL
sinirlar[[12]] <- c(526,529)
atilacak_veri[[13]] <- NULL
sinirlar[[13]] <- c(558.5,561.5)
atilacak_veri[[14]] <- NULL
sinirlar[[14]] <- c(578.5,581.5)
atilacak_veri[[15]] <- NULL
sinirlar[[15]] <- c(613.5,615.5)
atilacak_veri[[16]] <- NULL
sinirlar[[16]] <- c(645,648)
atilacak_veri[[17]] <- NULL
sinirlar[[17]] <- c(673.5,676.5)
atilacak_veri[[18]] <- NULL
sinirlar[[18]] <- c(700,703)
atilacak_veri[[19]] <- NULL
sinirlar[[19]] <- c(741.5,744.5)
atilacak_veri[[20]] <- NULL
sinirlar[[20]] <- c(772,775)
atilacak_veri[[21]] <- NULL
sinirlar[[21]] <- c(790,793)
atilacak_veri[[22]] <- NULL
sinirlar[[22]] <- c(818.5,821.5)
sinirlar[[23]] <- c(858.5,861.5)
sinirlar[[24]] <- c(893.5,896.5)
sinirlar[[25]] <- c(927,930)
sinirlar[[26]] <- c(953.5,956.5)
sinirlar[[27]] <- c(980,983)
sinirlar[[28]] <- c(1011,1014)
sinirlar[[29]] <- c(1043.5,1046.5)
sinirlar[[30]] <- c(1083.5,1086.5)
sinirlar[[31]] <- c(1107,1110)
sinirlar[[32]] <- c(1138.5,1141.5)
sinirlar[[33]] <- c(1169.5,1172.5)
sinirlar[[34]] <- c(1187,1190)
sinirlar[[35]] <- c(1233.5,1236.5)
sinirlar[[36]] <- c(1253.5,1256.5)
sinirlar[[37]] <- c(1284.5,1287.5)
sinirlar[[38]] <- c(1315,1318)
sinirlar[[39]] <- c(1343.5,1346.5)
sinirlar[[40]] <- c(1390,1393)
sinirlar[[41]] <- c(1427,1430)
sinirlar[[42]] <- c(1458,1461)
sinirlar[[43]] <- c(1473.5,1476.5)
sinirlar[[44]] <- c(1507.5,1510.5)
sinirlar[[45]] <- c(1533.5,1536.5)
sinirlar[[46]] <- c(1564,1567)
sinirlar[[47]] <- c(1588,1591)
}
### --AYARLAR-- ###
###################
# length(dosyalar)
islenecek_dosya_sayisi <- length(dosyalar)

# DONGU: i - dosyalar
for(i in 1:islenecek_dosya_sayisi){

  veri <- read.table(dosyalar[i], header=FALSE, sep = "", stringsAsFactors=FALSE)
  ciktiAdi <- paste0(strsplit(basename(dosyalar[i]), "\\.")[[1]][1],"")
  ciktiDosyasi <- paste0(ciktiAdi,".ctbl") 

  if(nokta_atma_islemi == "liste"){
#    evre <- (veri[,sutun[1]]-T0)/P - as.integer((veri[,sutun[1]]-T0)/P) # as.integer, yapisinin Excel ile farkliligi nedeniyle evre hesabi Excel'den farklidir.
#    veri[,evre_eklenecek_sutun] <- round(ifelse(evre<(-0.5), evre+1, ifelse(evre>(0.5), evre-1, evre)),15)
  
    # VERI KIRPMA
    veri <- veri[veri[,sutun[1]] > sinirlar[[i]][1] & veri[,sutun[1]] < sinirlar[[i]][2],]
    
    # VERI ATMA
    # Noktalar, belirleniyor
#    atilan_noktalar <- grepl(paste(atilacak_veri[[i]],collapse="|"),veri[,evre_eklenecek_sutun])
    # Noktalar, veriden atiliyor
#    veri <- veri[-which(atilan_noktalar),]
    # Gereksiz sutunlar cikariliyor.
#    veri$V4 <- NULL;  veri$V5 <- NULL
    
    # BILGI
#  sum(atilan_noktalar)  
  }
 
  if(nokta_atma_islemi == "manual"){
    repeat{      
      plot(veri[,sutun[1]], veri[,sutun[2]], main=ciktiAdi)
      mtext("Temizlenecek noktalarý seçin ve sað menüde 'Stop'a týklayýn", side = 3, line = 0)     
      tik <- identify(veri[,sutun[1]], veri[,sutun[2]], plot=T)
      
      if(sum(tik) != 0){veri <- veri[-tik,]}     
  
      tus <- function(key) {
            if (key == "z") return(invisible(1))           
            else return(invisible(0))
      }      
      cikis <- getGraphicsEvent(prompt = "Diger grafik icin 'z'ye BAS; ayni grafik icin baska tusa BAS", onKeybd = tus)
      if(cikis){print("z'ye basildi, donguden cikildi"); break}
    }
    
    if(F){
    repeat{
      plot(veri[,sutun[1]], veri[,sutun[2]], main=ciktiAdi)                     
      tik <- identify(veri[,sutun[1]], veri[,sutun[2]], plot=F)
      abline(h=veri[,sutun[2]][tik], col="red", offset=0, pos=3)
      text(veri[,sutun[1]][tik], veri[,sutun[2]][tik], labels=veri[,sutun[2]][tik], col="red", offset=0, pos=3)
      
      if(sum(tik) != 0){veri <- veri[-tik,]}     
  
      tus <- function(key) {
            if (key == "z") return(invisible(1))           
            else return(invisible(0))
      }      
      cikis <- getGraphicsEvent(prompt = "Diger grafik icin 'z'ye BAS; ayni grafik icin baska tusa BAS", onKeybd = tus)
      if(cikis){print("z'ye basildi, donguden cikildi"); break}
    }
    }
    
    # Grafik penceresini kapat
    dev.off()
  }          
    
  # Veri, dosyaya yazdiriliyor.
  write.table(veri, file=ciktiDosyasi, col.names=FALSE, row.names=FALSE, quote=FALSE, append=FALSE, sep = "\t")     
  
##########################
### --GRAFIGI YAZDIR-- ###
# pdf(), png(), postscript(): saydamligi desteklemez: pdf'i eps'ye cevir.
yazdir <- 1 
if(yazdir){
png(paste0(ciktiAdi,".png"))#, width = 40, height = 30)
plot(veri[,sutun[1]], veri[,sutun[2]], main=ciktiAdi)
# En altta dev.off komutunu kapatmali; bu nedenle 'yazdir' degiseni ile kontrol ediliyor.
}
if(yazdir){
dev.off()} 
### --GRAFIGI YAZDIR-- ###
##########################

} # for DONGU SONU
####################
### --KOD SONU-- ###
winDialog("ok", paste0("Kod Sonlandý!","\n\nBÝLGÝ: ",
  "\nÝþlenen Dosya Top.Sayýsý = ", islenecek_dosya_sayisi)) 
  #"\nAtýlan Satýr Top.Sayýsý = ", TopGecersizSayisi))
### --KOD SONU-- ###
####################