setwd("D:/Akademik/TEZ/DR/ExoLCA/TrES-3")

veri <- c(
	"20120603T122_tres3b_R_fYOK_SYNFITTER-v24",
	"20120607T122_tres3b_R_fYOK_SYNFITTER-v24",
	"20130527T122_tres3b_R_fYOK_SYNFITTER-v24",
	"20130713T122_tres3b_R_fYOK_SYNFITTER-v24",
	"20140615TUG100_tres3b_R_SYNFITTER-v24",
	"20140702T122_tres3b_R_SYNFITTER-v24",
	"20150427TUG100_tres3b_R_SYNFITTER-v25"
	)

tarih <- c(
	"03.06.2012",
	"07.06.2012",
	"25.05.2013",
	"13.07.2013",
	"15.06.2014",
	"02.07.2014",
	"27.04.2015"
	)

telccd <- c(
	"ÇOMÜG T122 + AP42 Bessel R",
	"ÇOMÜG T122 + AP42 Bessel R",
	"ÇOMÜG T122 + AP42 Bessel R",
	"ÇOMÜG T122 + AP42 Bessel R",
	"TUG T100 + SI1100 Bessel R",
	"ÇOMÜG T122 + AP42 Bessel R",
	"TUG T100 + SI1100 Bessel R"
	)
	
coklu <- TRUE

gozlem <- list()
length(gozlem) <- 5000

i <- 1
gozlem[[i]] <- read.table(veri[i], 
                     header=FALSE, sep = "\t", 
                     col.names=c("GE", "GA", "GH", "ME", "MA", "AE", "AA"))

gozlem[[i]]$GA <- sapply(gozlem[[i]]$GA, function(x) x-(i*0.04)+0.04)

# Geçiþ Eðrisi
par(fig=c(0,1,0.14,1), new=TRUE, las=1, ps=10)
plot(gozlem[[i]]$GE, gozlem[[i]]$GA, 
     xlab="", ylab="Normalize Aký", pch = 19, col="#00000060",
     xlim=c(-0.08, 0.08), ylim=c(0.77, 1.02), tcl=0.2, xaxt='n',
     cex=0.8, cex.lab=1.0, cex.axis=1.0, cex.main=0.5, cex.sub=0.1)

axis(side=1, at = seq(-10, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=1, at = seq(-10, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=2, at = seq(0, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=2, at = seq(0, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=3, at = seq(-10, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=3, at = seq(-10, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=4, at = seq(0, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=4, at = seq(0, 10, by = 0.005), labels=FALSE, tcl=0.1)

# Üst Yazý
text(x=-0.082, y=1.020, label=tarih[i], pos=4, cex=0.7)
text(x=-0.082, y=1.012, label=telccd[i], pos=4, cex=0.7)
#text(x=0, y=1.010, label="TUG T100 + SI1100 Bessel R", pos=3, cex=0.9)


# Hata Çubuklarý
arrows(gozlem[[i]]$GE, gozlem[[i]]$GA-gozlem[[i]]$GH, 
       gozlem[[i]]$GE, gozlem[[i]]$GA+gozlem[[i]]$GH, 
	 code=0, angle=90, length=0.1, col="#00000030")

# Model Eðri
lines(gozlem[[i]]$ME, gozlem[[i]]$MA, col="red", lwd="2.4")

# Artýklar Eðrisi
par(fig=c(0,1,0,0.4), new=TRUE, las=1)
plot(gozlem[[i]]$AE, gozlem[[i]]$AA, 
     xlab="Evre", ylab="Artýk.", pch = 19,  col="#00000080", 
     xlim=c(-0.08, 0.08), ylim=c(-0.12, 0.02), tcl=0.2, yaxt='n',
     at = seq(-10, 10, by = 0.02),
     cex=0.8, cex.lab=1.0, cex.axis=1.0)

axis(side=1, at = seq(-10, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=1, at = seq(-10, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=2, at = seq(-10, 10, by = 0.04), labels=TRUE, tcl=0.2)
axis(side=2, at = seq(-10, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=3, at = seq(-10, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=3, at = seq(-10, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=4, at = seq(-10, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=4, at = seq(-10, 10, by = 0.005), labels=FALSE, tcl=0.1)

abline(h=0, col="red", lwd="1.6")

if(coklu){
###################################################################
### DONGU BASLIYOR ###
###################################################################

for(i in 2:6){

gozlem[[i]] <- read.table(veri[i], 
                     header=FALSE, sep = "\t", 
                     col.names=c("GE", "GA", "GH", "ME", "MA", "AE", "AA"))

### EGRILER Y EKSENINDE KAYDIRILIYOR
deltaY <- 0.04
gozlem[[i]]$GA <- sapply(gozlem[[i]]$GA, function(x) x-(i*deltaY)+deltaY)
gozlem[[i]]$MA <- sapply(gozlem[[i]]$MA, function(x) x-(i*deltaY)+deltaY)
gozlem[[i]]$AA <- sapply(gozlem[[i]]$AA, function(x) x-(i*deltaY/2)+deltaY/2)
cizgi <- 0-(i*deltaY/2)+deltaY/2

# Gecis Egrisi Dongu

par(fig=c(0,1,0.14,1), new=TRUE, las=1)
plot(gozlem[[i]]$GE, gozlem[[i]]$GA, 
     xlab="", ylab="", pch = 19, col="#00000060",
     xlim=c(-0.08, 0.08), ylim=c(0.77, 1.02), tcl=0.2, xaxt='n', yaxt='n',
     cex=0.8, cex.lab=1.0, cex.axis=1.0, cex.main=0.5, cex.sub=0.1
	)

axis(side=1, at = seq(-10, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=1, at = seq(-10, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=2, at = seq(0, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=2, at = seq(0, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=3, at = seq(-10, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=3, at = seq(-10, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=4, at = seq(0, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=4, at = seq(0, 10, by = 0.005), labels=FALSE, tcl=0.1)

# Üst Yazý
text(x=-0.082, y=1-(i*deltaY)+deltaY+0.020, label=tarih[i], pos=4, cex=0.7)
text(x=-0.082, y=1-(i*deltaY)+deltaY+0.012, label=telccd[i], pos=4, cex=0.7)
#text(x=0, y=1.010, label="TUG T100 + SI1100 Bessel R", pos=3, cex=0.9)


# Hata Çubuklarý
arrows(gozlem[[i]]$GE, gozlem[[i]]$GA-gozlem[[i]]$GH, 
       gozlem[[i]]$GE, gozlem[[i]]$GA+gozlem[[i]]$GH, 
	 code=0, angle=90, length=0.1, col="#00000030")

# Model Eðri
lines(gozlem[[i]]$ME, gozlem[[i]]$MA, col="red", lwd="2.4")

# Artýklar Eðrisi
par(fig=c(0,1,0,0.4), new=TRUE, las=1)
plot(gozlem[[i]]$AE, gozlem[[i]]$AA, 
     xlab="", ylab="", pch = 19,  col="#00000080", 
     xlim=c(-0.08, 0.08), ylim=c(-0.12, 0.02), tcl=0.2, yaxt='n', xaxt='n',
     at = seq(-10, 10, by = 0.02),
     cex=0.8, cex.lab=1.0, cex.axis=1.0)

axis(side=1, at = seq(-10, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=1, at = seq(-10, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=2, at = seq(-10, 10, by = 0.04), labels=F, tcl=0.2)
axis(side=2, at = seq(-10, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=3, at = seq(-10, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=3, at = seq(-10, 10, by = 0.005), labels=FALSE, tcl=0.1)
axis(side=4, at = seq(-10, 10, by = 0.01), labels=FALSE, tcl=0.2)
axis(side=4, at = seq(-10, 10, by = 0.005), labels=FALSE, tcl=0.1)

abline(h=cizgi, col="red", lwd="1.6")
}
}

