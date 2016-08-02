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
trt <- read.table("Data/LaurePrep/plot_treatments_6July16.txt")

###Sub-plot data (1x1m)
sp02 <- read.table(paste("Data/LaurePrep/subPlotSpc_2002_NbIndiv_", dt, ".txt", sep=""), stringsAsFactors = F)
sp08 <- read.table(paste("Data/LaurePrep/subPlotSpc_2008_NbIndiv_", dt, ".txt", sep=""), stringsAsFactors = F)
sp11 <- read.table(paste("Data/LaurePrep/subPlotSpc_2011_NbIndiv_", dt, ".txt", sep=""), stringsAsFactors = F)
sp14 <- read.table(paste("Data/LaurePrep/subPlotSpc_2014_NbIndiv_", dt, ".txt", sep=""), stringsAsFactors = F)

##############################################################################
###3) Rarefy
##############################################################################
spd2<-rowSums(decostand(sp02,"pa"))
spd8<-rowSums(decostand(sp08,"pa"))
spd11<-rowSums(decostand(sp11,"pa"))
spd14<-rowSums(decostand(sp14,"pa"))

spr2<-rarefy(sp02,sample=10)
spr8<-rarefy(sp08,10)
spr11<-rarefy(sp11,10)
spr14<-rarefy(sp14,10)

spd2<-rowSums(decostand(sp02,"pa"))
spd8<-rowSums(decostand(sp08,"pa"))
spd11<-rowSums(decostand(sp11,"pa"))
spd14<-rowSums(decostand(sp14,"pa"))

Ind2<-rowSums(sp02)
Ind8<-rowSums(sp08)
Ind11<-rowSums(sp11)
Ind14<-rowSums(sp14)

