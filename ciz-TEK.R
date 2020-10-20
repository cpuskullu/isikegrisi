# Working Dir:
wd <- "D:/Akademik/PROJE-MFAG113F353/KeplerLCs/Kepler91 (KOI2133)/Q00-17"

# Working (data) file: * file has two columns and seperated by space(s)
girdi_dosyasi <- "Q03LC_kepler91b.tbl"

## End of the script, the file 'veri.dat' will be created in the working dir.
cikti_dosyasi <- "Q03LC_kepler91b-binned.dat"

## Normalization Value
Norm = 148299.4

## Phasing Parameters
T0 = 136.396600
P = 6.246580	

## Bin Size
bin = 360

#### #### #### #### ####
setwd(wd)
system.time(veri <- read.table(girdi_dosyasi, header=FALSE, sep="", stringsAsFactors=FALSE))
veri$V1 <- as.numeric(veri$V1)
veri$V2 <- as.numeric(veri$V2)

# Data sorting by first column
veri <- veri[order(veri$V1),]
summary(veri)
x <- veri[,1]
y <- veri[,2]

x_ekseni <- "Time"

{
print(paste0("Current working dir: ", getwd()))
print(paste0("Data file: ", girdi_dosyasi))
print(paste0('Row length: ', length(x)))
print('PROCESSES:')
print('(1) Normalization')
print('(2) Phasing')
print('(3) Sigma Clipping')
print('(4) Binning')
print('(5) Extracting')
print('(6) Norm, Phase and Bin')
print('(0) Exit')
print('###################')
print('End of the script, the file <veri.dat> will be created in the working dir.')

#secim <- 6
## SEÇÝM KOMUTU
secim <- readline('Select Process: ')
secim <- ifelse(grepl("\\D",secim),-1,as.integer(secim))
#if(is.na(secim)){break}

#Normalization
if(secim == 1 | secim == 6) {
	print('Applied: (1) Normalization')	
  y <- y/Norm
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

#x <- append(x,x+1)
#y <- append(y,y)

#Binning
if(secim == 4 | secim == 6) {
	print('Applied: (4) Bin the data')	
	x <- colMeans(matrix(x, length(x)/bin))
	y <- colMeans(matrix(y, length(y)/bin))
 
  print(paste0('Binned Row length: ', length(x)))
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
	plot(x, y, 
	xlab=x_ekseni, ylab="", pch = 16, col="black",
	#ylim=c(0.998, 1.002), 
	tcl=0.2)
	axis(side=1, at = seq(0, 1, by = 0.1), labels=FALSE, tcl=0.2)
	abline(1, 0, col="green")

# WRITING OUTPUT
	m  <- cbind(x,y)
	write.table(m, file=cikti_dosyasi, col.names=FALSE, row.names=FALSE, quote=FALSE, append = FALSE, sep = "\t")

}

