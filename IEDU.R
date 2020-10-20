#####################################################################################
#         -- IEDU.R (IÞIK EÐRÝSÝ DÜZENLEME UYARLAMA)- 29.09.2015 --                 #
# HESAP::                                                                        #
# CIZIM:: YOK                                                                       #
# CIKTI::                                                                           #
# KAYNAK::                                                                          #
#####################################################################################

### --BUYUK HARFLERLE-- YAZILAN BASLIKLAR ICINDEKI GIRDILER DUZENLENMELIDIR. Kucuk Harflerle yazilan basliklar altinda islemler yapilir. ###

####################
### Kutuphaneler ###
library(tools)

###################
### --AYARLAR-- ###
#1 Working Dir:
wd <- "D:/Akademik/TEZ/DR/ExoLCA/Kepler491 (KOI201)"    
setwd(wd)

#2 If working one more file, define output file to append whole data, set file number and TRUE
coklu <- TRUE
birlesen_dosya <- "#Q00-17SC_kepler491b.dat"    

fl <- list.files(path = ".", pattern = ".tbl")
son <- length(fl)

if(!coklu){ son <- 1 }
for(i in 1:son){

#3 Working (data) file: * file has three columns (with errors) and seperated by spaces
if(coklu){  
  girdi_dosyasi <- fl[i] 
  #girdi_dosyasi <- paste0("Q",sprintf("%02d", i),"SC_tres2b.tbl") 
}
else {
  girdi_dosyasi <- "Q01SC132-165_tres2b.tbl" 
  }

#4 End of the script, a file will be created in the working dir.
if(coklu){
  cikti_dosyasi <- paste0(file_path_sans_ext(fl[i]),"-binned.dat")
  #cikti_dosyasi <- paste0("Q",sprintf("%02d", i),"SC_tres2b-binned.dat")
}
else {
  cikti_dosyasi <- "Q01SC132-165_tres2b-binned.dat" 
  }

#5 Normalization Value
if(0){ 
  Norm <- unlist(read.table(file="#Q00-17SC_tres2b_NormDegerleri.dat", header=FALSE, stringsAsFactors=FALSE)) }
else {   
  Norm <- 407300  
  } 
  
#6 Phasing Parameters
T0 = 16.5266399998217
P = 2.47061892	

#7 Expected Point after Binning applied (set bin_size = 0) or bin size in minute
bin_to = 2723
bin_size = 18  # in minute

### --AYARLAR-- ###
###################
        
####################
### --ISLEMLER-- ###  
system.time(veri <- read.table(girdi_dosyasi, header=FALSE, sep="", stringsAsFactors=FALSE))
veri$V1 <- as.numeric(veri$V1)
veri$V2 <- as.numeric(veri$V2)
#veri$V3 <- as.numeric(veri$V3)

# Data sorting by first column
veri <- veri[order(veri$V1),]
summary(veri)
x <- veri[,1]
y <- veri[,2]
#e <- veri[,3]

x_ekseni <- "Time"

if(bin_size){
bin_to <- length(veri[,1])/(bin_size*(veri[length(veri[,1]),1]-veri[1,1])*24*60/length(veri[,1]))
}

{
print('####### INFO & MENU #######')
print(paste0("Current working dir: ", getwd()))
print(paste0("Input Data file: ", girdi_dosyasi))
print(paste0('Row length: ', length(x)))
print(paste0('Bin Size: ', length(x)/bin_to))
print('>>> Select a Process to Continue:')
print('(1) Normalization')
print('(2) Phasing')
print('(3) Sigma Clipping')
print('(4) Binning')
print('(5) Extracting')
print('(6) Norm, Phase and Bin')
print('(0) Exit')
print('###########################')
print(paste0('End of the script, the file <', cikti_dosyasi, '> will be created in the working dir.'))

secim <- 2
## SEÇÝM KOMUTU
#secim <- readline('Select Process: ')
#secim <- ifelse(grepl("\\D",secim),-1,as.integer(secim))
#if(is.na(secim)){break}

#Normalization
if(secim == 1 | secim == 6) {
	print('Applied: (1) Normalization')	
  y <- y/Norm[i]
  e <- e/Norm[i]
}

#Binning
if(secim == 4 | secim == 6) {
	print('Applied: (4) Bin the data')	
	x <- colMeans(matrix(x, length(x)/bin_to))
	y <- colMeans(matrix(y, length(y)/bin_to))
  #e <- colMeans(matrix(e, length(e)/bin_to))
  
  print(paste0('Binned Row length: ', length(x)))
}

#Phasing
if(secim == 2 | secim == 6) {
	print('Applied: (2) Phase the data')	
	x <- abs((x-T0)/P) - as.integer((x-T0)/P)
	veri <- veri[order(x),]

  x_ekseni <- "Phase"
}  

#SigmaClip
if(secim == 3) {
	print('Applied: (3) Sigma Clipping')
	veri <- veri[veri$V2 < 0 + 3 * sd(veri$V2) & veri$V2 > 0 - 3 * sd(veri$V2)]
	points(veri[,1], veri[,2], col="red")
	write.table(veri, file="veri.dat", col.names=FALSE, row.names=FALSE, quote=FALSE, append = FALSE, sep = "\t")
}

#Extracting
if(secim == 5) {
	print('Applied: (5) Extracting')	
	veri$V1 <- veri$V1[seq(1,length(veri$V1),14)]
	veri$V2 <- veri$V2[seq(1,length(veri$V2),14)]
}

#ArtýklarAtlama
if(secim == 10) {
	print('Applied: (5) Artýklar Atlama')
	y <- veri$V2[seq(1,length(veri$V2),14)]
	write.table(y, file="INIT_FLUX_PL_ERR_ext14.tbl", col.names=FALSE, row.names=FALSE, quote=FALSE, append = FALSE, sep = "")
}

# PLOTTING
  xe <- x + 1
  xc <- c(xe,x)
  yc <- c(y,y)
	plot(xc, yc, 
	xlab=x_ekseni, ylab="", pch = 16, col="black",
	xlim=c(0.9, 1.1), 
  ylim=c(0.985, 1.002), 
	tcl=0.2)
	axis(side=1, at = seq(0, 1, by = 0.1), labels=FALSE, tcl=0.2)
	abline(1, 0, col="green")  
  #Error Bars
  #arrows(xc, yc-e, xc, yc+e, length=0.05, angle=90, code=0)



### Veri araligi al
daf <- data.frame(x,y)
#daf <- daf[order(-daf$x),] 
daf1 <- daf[daf$x < 0.05,]
daf2 <- daf[!daf$x < 0.95,]
dafs <- merge(daf1,daf2,all=TRUE) # all=T olmadan merge sorunlu bu nedenle dogrudan yazilamiyor: daf[daf$x < 0.05 & !daf$x < 0.95,]


# WRITING OUTPUT
	m  <- dafs       #cbind(x,y,e)
	write.table(m, file=cikti_dosyasi, col.names=FALSE, row.names=FALSE, quote=FALSE, append = FALSE, sep = "\t")

if(coklu){  	
  write.table(m, file=birlesen_dosya, col.names=FALSE, row.names=FALSE, quote=FALSE, append = TRUE, sep = "\t") }

}
}
### --ISLEMLER-- ###  
####################

####################
### --KOD SONU-- ###
winDialog("ok", paste0("Kod Sonlandý!","\n\nBÝLGÝ: ")
### --KOD SONU-- ###
####################

################################################################################
#                   IÞIK EÐRÝSÝ DÜZENLEME UYARLAMA (IEDU)                      #
#                               29.09.2015                                     #
#                      ** Geliþtirmeler Hakkýnda **                            #
#                                                                              #
#  s2: Hata verisini okuma eklendi. Bin sayýsý üzerinden ortalamasý hesaplaný- #
#      yor. Bin yapýlacak deðere ek olarak, bin boyutu dk cinsinden belirlene- #             
#      biliyor.                                                                #
################################################################################