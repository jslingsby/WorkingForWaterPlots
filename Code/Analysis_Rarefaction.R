##############################################################################
######## Code to repeat rarefaction analysis for all surveys of the WfW plots - JSlingsby
##############################################################################
######## Last edited: 8 July 2016
##############################################################################
######## 
###Steps:
##############################################################################
##############################################################################
###1) Get libraries and setwd
##############################################################################
library(vegan)

if (Sys.getenv("USER")=="jasper"){setwd("/Users/jasper/Dropbox/Shared/CapeCommunities")}

dt <- "12July16"

##############################################################################
###2) Get data
##############################################################################

###Treatment data
trt <- read.table("Data/LaurePrep/plot_treatments_21June16.txt")

###Sub-plot data (1x1m)
sp02 <- read.table(paste("Data/LaurePrep/subPlotSpc_2002_NbIndiv_", dt, ".txt", sep=""), stringsAsFactors = F)
sp08 <- read.table(paste("Data/LaurePrep/subPlotSpc_2008_NbIndiv_", dt, ".txt", sep=""), stringsAsFactors = F)
sp11 <- read.table(paste("Data/LaurePrep/subPlotSpc_2011_NbIndiv_", dt, ".txt", sep=""), stringsAsFactors = F)
sp14 <- read.table(paste("Data/LaurePrep/subPlotSpc_2014_NbIndiv_", dt, ".txt", sep=""), stringsAsFactors = F)

#Fix error?
sp02$Wahlenbergia_tenella[which(sp02$Wahlenbergia_tenella==.5)] <- 5

#Fix rownames
r2 <- rownames(sp02)
r8 <- rownames(sp08)
r11 <- rownames(sp11)
r14 <- rownames(sp14)

r2 <- gsub("p", "", r2)
rownames(sp02) <- sapply(r2, function(x) {paste(strsplit(x, split = "_")[[1]][1], which(letters==strsplit(x, split = "_")[[1]][2]), sep="_")})
rownames(sp08) <- sapply(r8, function(x) {paste(strsplit(x, split = "\\.")[[1]][1], strsplit(x, split = "\\.")[[1]][3], sep="_")})
rownames(sp11) <- sapply(r11, function(x) {paste(strsplit(x, split = "\\.")[[1]][1], strsplit(x, split = "\\.")[[1]][3], sep="_")})
rownames(sp14) <- sapply(r14, function(x) {paste(strsplit(x, split = "\\.")[[1]][1], which(LETTERS==strsplit(x, split = "\\.")[[1]][3]), sep="_")})


##############################################################################
###3) Rarefy
##############################################################################
spd2<-rowSums(decostand(sp02,"pa"))
spd8<-rowSums(decostand(sp08,"pa"))
spd11<-rowSums(decostand(sp11,"pa"))
spd14<-rowSums(decostand(sp14,"pa"))

sp02_5 <- sp02[which(spd2>5),]
sp02_5 <- sp02_5[,which(colSums(sp02_5)>0)]
sp08_5 <- sp08[which(spd8>5),]
sp08_5 <- sp08_5[,which(colSums(sp08_5)>0)]
sp11_5 <- sp11[which(spd11>5),]
sp11_5 <- sp11_5[,which(colSums(sp11_5)>0)]
sp14_5 <- sp14[which(spd14>5),]
sp14_5 <- sp14_5[,which(colSums(sp14_5)>0)]

spr2_5 <- rarefy(sp02_5,4)
spr8_5 <- rarefy(sp08_5,4)
spr11_5 <- rarefy(sp11_5,4)
spr14_5 <- rarefy(sp14_5,4)

spd2_5 <- rowSums(decostand(sp02_5,"pa"))
spd8_5 <- rowSums(decostand(sp08_5,"pa"))
spd11_5 <- rowSums(decostand(sp11_5,"pa"))
spd14_5 <- rowSums(decostand(sp14_5,"pa"))

Ind2_5 <- rowSums(sp02_5)
Ind8_5 <- rowSums(sp08_5)
Ind11_5 <- rowSums(sp11_5)
Ind14_5 <- rowSums(sp14_5)

dat <- data.frame(
  Subplot = c(rownames(sp02_5), rownames(sp08_5), rownames(sp11_5), rownames(sp14_5)),
  Year = c(rep(2002, length(spr2_5)), rep(2008, length(spr8_5)), rep(2011, length(spr11_5)), rep(2014, length(spr14_5))),
  Individuals = c(Ind2_5, Ind8_5, Ind11_5, Ind14_5),
  Species_density = c(spd2_5, spd8_5, spd11_5, spd14_5),
  Species_richness = c(spr2_5, spr8_5, spr11_5, spr14_5)
  )



