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

##############################################################################
###2) Get data
##############################################################################

###Treatment data
trt <- read.table("Data/LaurePrep/plot_treatments_6July16.txt")

###Sub-plot data (1x1m)
sp02 <- read.table("Data/LaurePrep/subPlotSpc_2002_NbIndiv_21June16.txt", stringsAsFactors = F)
sp08 <- read.table("Data/LaurePrep/subPlotSpc_2008_NbIndiv_21June16.txt", stringsAsFactors = F)
sp11 <- read.table("Data/LaurePrep/subPlotSpc_2011_NbIndiv_21June16.txt", stringsAsFactors = F)
sp14 <- read.table("Data/LaurePrep/subPlotSpc_2014_NbIndiv_21June16.txt", stringsAsFactors = F)

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

spd2<-rowSums(decostand(samp2,"pa"))
spd8<-rowSums(decostand(samp8,"pa"))

Ind2<-rowSums(samp2)
Ind8<-rowSums(samp8)