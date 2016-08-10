# Laure Gallien
# 13 July 16



library(ape)
library(ggplot2)
library(picante)
library(cluster)
library(grDevices)
library(diveRsity)

setwd("~/Desktop/Dropbox/Travail_SA/3.ComEcol/CapeCommunities/Data/LaurePrep/")


########################################################
########################################################
# Check the phylogenies
########################################################
########################################################

# Load species list
#.............................
spc <- read.delim("All_species_list_CapeCom_13July16.txt")
spc[,1] <- as.character(spc[,1])

spc$Genus <- unlist(lapply(strsplit(spc[,1], "_"), function(x) x[1]))

GEN <- unique(spc$Genus) 
length(GEN)  # 360

# Load phylogeny published by Hinchliff et al 2014 POne
#.............................
hind <- read.tree("~/Desktop/Dropbox/Travail WSL/1.MetaAnalyse/Datasets/Phylogeny/Angiosperm/Hinchliff et al 2014 POne/Hinchliff_chronogram_test.tre")
hind

ComINphy <- GEN[GEN %in% hind$tip.label]
length(ComINphy) # 347 --> REALLY GOOD


# Load the phylogeny of Zanne 2014 et al Sci
#.............................
load("~/Desktop/Dropbox/Travail WSL/1.MetaAnalyse/Datasets/Phylogeny/Angiosperm/Zanne et al 2014 Sci/Angios_genus") # 
zan <- Angios_genus

ComINphy2 <- GEN[GEN %in% zan$tip.label]
length(ComINphy2) # 324

# to fix: 
GEN[!(GEN %in% zan$tip.label)]


# Load the phylogeny of Forest et al 2007 Nat
#.............................
fors <- read.tree("~/Desktop/Dropbox/Travail_SA/3.ComEcol/CapeCommunities/Papers/Forest et al. tree/Foresttree.phy") # 
fors

ComINphy3 <- GEN[GEN %in% fors$tip.label]
length(ComINphy2) # 260


# Look at it
#.............................
phylo <- drop.tip(hind, hind$tip.label[!hind$tip.label %in% GEN])
phylo <- drop.tip(zan, zan$tip.label[!zan$tip.label %in% GEN])
phylo <- drop.tip(fors, fors$tip.label[!fors$tip.label %in% GEN])
plot(phylo, "f", cex=0.5)


########################################################
########################################################
# Check the qualitative traits
########################################################
########################################################
tr <- read.delim("Trait_2002_1Aug16.txt")
tr$SpcID <- as.character(tr$SpcID)
head(tr)
dim(tr) # 823  11

# check the coverage from our species list
#...........................................
SP <- unique(spc$spcID)
length(SP) # 954

spcINtrai <- SP[SP %in% tr$SpcID]
length(spcINtrai) # 596

missSpc <- SP[!SP %in% tr$SpcID]
length(missSpc) # 358

write.table(missSpc, file="/Users/diversitalp/Desktop/Dropbox/Travail_SA/3.ComEcol/CapeCommunities/SpeciesWithMissingQualitativeTraits_1Aug16.txt", sep="\t", row.names=F, quote=F)

# check the invasive species too
#...........................................
inv <- read.delim("Exotic_species_list_CapeCom_13July16.txt")
II <- as.character(unique(inv$spcID))
length(II) # 82

invINtrai <- II[II %in% tr$SpcID]
length(invINtrai) # 62

missInv <- II[!II %in% tr$SpcID]
length(missInv) # 20


# Check the traits available
#...........................................
trC <- tr[which(tr$SpcID %in% SP),]
dim(trC) # 596  11
trC$InvNat <- ifelse(trC$SpcID %in% II, 1, 0)
head(trC)

myTr <- names(trC)[2:11]

COL <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00')
par(mfrow=c(3, 4))
for(i in myTr) {
	temp <- trC[,c(i, "InvNat")]
	plot(t(table(temp)), col=COL, main=i)
}


# Prepare the traits for a distance matrix
#...........................................
TT <- trC
row.names(TT) <- TT$SpcID
TT <- TT[,myTr]
for(i in myTr) TT[,i] <- as.factor(ifelse(as.character(TT[,i])=="u", "NA", as.character(TT[,i])))
str(TT)

dist.tr <- daisy(TT, "gower")


# Check why regional endemics (re) and others (o) are in the list of invasive species
trC[which(trC$InvNat==1 & trC$distribution %in% c("re", "o")), "SpcID"]


########################################################
########################################################
# Check the quantitative traits
########################################################
########################################################

cont <- as.character(read.delim("~/Desktop/Dropbox/Travail_SA/3.ComEcol/CapeCommunities/Data/spp_matched_to_quantitative_trait_data.txt")[,1])

head(cont)
length(cont) # 364

# Look at prop cover by these species
par(mfrow=c(2,2))
YY <- c("02", "08", "11", "14")
TEMP <- paste("PlotSpc_20", YY, "_12July16.txt", sep="")
for(i in 1:4){
	tdat <- read.delim(TEMP[i])
	RichAll <- rowSums(tdat)
	RichTC <- rowSums(tdat[, which(names(tdat) %in% cont)])
	PropRich <- round(RichTC/RichAll*100)
	temp <- data.frame(RichAll=RichAll, RichTC=RichTC, PropRich=PropRich)
	temp[order(temp[,1]),]
	plot(PropRich ~ RichAll, data=temp, main=paste("10x10 sites 20", YY[i], sep=""), ylab="Prop species in Cont Traits", xlab="Site richness")
} 

par(mfrow=c(2,2))
YY <- c("02", "08", "11", "14")
TEMP <- paste("subPlotSpc_20", YY, "_PropCover_12July16.txt", sep="")
for(i in 1:4){
	tdat <- read.delim(TEMP[i])
	RichAll <- rowSums(tdat)
	RichTC <- rowSums(tdat[, which(names(tdat) %in% cont)])
	PropRich <- round(RichTC/RichAll*100)
	temp <- data.frame(RichAll=RichAll, RichTC=RichTC, PropRich=PropRich)
	temp[order(temp[,1]),]
	plot(PropRich ~ RichAll, data=temp, main=paste("1x1 sites 20", YY[i], sep=""), ylab="Prop Cover in Cont Traits", xlab="Site total cover")
} 






########################################################
########################################################
# Check richness and diversity of communities
########################################################
########################################################

#------------------------------------------------
# Get the species x site data : 10 x 10m
#------------------------------------------------
# 2002 / 2008 / 2011 / 2014
#........
p02 <- read.delim("PlotSpc_2002_12July16.txt")
p08 <- read.delim("PlotSpc_2008_12July16.txt")
p11 <- read.delim("PlotSpc_2011_12July16.txt")
p14 <- read.delim("PlotSpc_2014_12July16.txt")

# Small test

YY <- c("2002", "2008", "2011", "2014")
l.SumPlot <- list()
for(yy in YY) {
	if(yy=="2002") tempP <- p02 ; if(yy=="2008") tempP <- p08
	if(yy=="2011") tempP <- p11 ; if(yy=="2014") tempP <- p14
	
	temp <- data.frame(sitID=row.names(tempP), tempP)
	tsid <- as.character(temp$sitID)
	if(yy=="2002"){	sitid <- data.frame(matrix(unlist(strsplit(substring(temp$sitID, 2, 10), "_")), ncol=2, byrow=T)) 
		} else { sitid <- data.frame(LID=substring(tsid, 1, nchar(tsid)-2), PID=substring(tsid, nchar(tsid), 20)) }
	names(sitid) <- c("LocID", "PlotID")
	temp <- data.frame(sitID=temp$sitID, sitid, tempP)
	temp$LocID <- as.numeric(as.character(temp$LocID))
	l.SumPlot[[yy]] <- t(sapply(unique(temp$LocID), function(x) colSums(temp[temp$LocID==x,c(2, 4:ncol(temp))])))
}
allRich <- as.data.frame(do.call(rbind, sapply(names(Rich), function(x) cbind(year=x, LocID=l.SumPlot[[x]][,"LocID"], rich=Rich[[x]]))))
for(i in 1:3) allRich[,i] <- as.numeric(as.character(allRich[,i]))
head(allRich)

s4 <- names(table(allRich$LocID)[table(allRich$LocID)==4])
com4 <- allRich[which(allRich$LocID %in% s4),]
length(unique(com4$LocID)) # 55
dim(com4)  # 16

p1 <- ggplot(com4, aes(x=year, y=rich, colour=LocID, group=LocID)) + geom_line(show.legend=F)
p2 <- ggplot(allRich, aes(x=year, y=rich, colour=LocID, group=LocID)) + geom_line(show.legend=F)
multiplot(p1, p2, cols=2)



# Loop to calculate summary metrics
YY <- c("2002", "2008", "2011", "2014")
l.com.yy <- list()
for(yy in YY) {
	if(yy=="2002") tempP <- p02 ; if(yy=="2008") tempP <- p08
	if(yy=="2011") tempP <- p11 ; if(yy=="2014") tempP <- p14
	
	COM <- data.frame(sitID=row.names(tempP), year=yy, rich=rowSums(tempP), nbInv=rowSums(tempP[,names(tempP) %in% II]))

	# separate the native and invasive species
	#.................
	ppN <- c("PP_all", "PP_nat", "PP_inv")
	l.comP <- l.phly <- l.comT <- l.trait.dist <- list()
	for(k in ppN){
		if(k == "PP_all") PP <- tempP
		if(k == "PP_nat") PP <- tempP[,!names(tempP) %in% II]
		if(k == "PP_inv") PP <- tempP[,names(tempP) %in% II]
		
		# add MPD
		#.................
		PP1 <- data.frame(spcID=unlist(lapply(strsplit(names(PP), "_"), function(x) x[1])), t(PP))
		dupli <- unique(as.character(PP1$spcID[duplicated(PP1$spcID)]))
		for(i in dupli) { 
			t.row <- row.names(PP1[which(PP1==i),])
			remp <- as.numeric(apply(PP1[t.row, 2:ncol(PP1)], 2, max)) 
			for(j in t.row) PP1[j, 2:ncol(PP1)] <- remp
		}
		PP1 <- unique(PP1)
		row.names(PP1) <- PP1$spcID
		PP1 <- PP1[,2:ncol(PP1)]
		PP2	<- t(PP1)
		l.comP[[k]] <- PP2[,colnames(PP2) %in% hind$tip.label]
		
		# adapt the phylogeny
		l.phly[[k]] <- drop.tip(hind, hind$tip.label[!hind$tip.label %in% colnames(l.comP[[k]])])

		# add MFD
		#.................
		l.comT[[k]] <- PP[,which(colnames(PP) %in% row.names(TT))]
		TT1 <- TT[which(row.names(TT) %in% names(PP)),]
		l.trait.dist[[k]] <- daisy(TT1[names(l.comT[[k]]), ], "gower")
		
		print(k)
	}
	COM$MPD.all <- ses.mpd(l.comP[[1]], cophenetic(l.phly[[1]]), runs=99)$mpd.obs.p
	COM$MPD.nat <- ses.mpd(l.comP[[2]], cophenetic(l.phly[[2]]), runs=99)$mpd.obs.p
	COM$MPD.inv <- ses.mpd(l.comP[[3]], cophenetic(l.phly[[3]]), runs=99)$mpd.obs.p
	
	COM$MFD.all <- ses.mpd(l.comT[[1]], l.trait.dist[[1]], runs=99)$mpd.obs.p
	COM$MFD.nat <- ses.mpd(l.comT[[2]], l.trait.dist[[2]], runs=99)$mpd.obs.p
	COM$MFD.inv <- ses.mpd(l.comT[[3]], l.trait.dist[[3]], runs=99)$mpd.obs.p

	l.com.yy[[yy]] <- list(COM=COM, l.com.phy=l.comP, l.phly=l.phly, l.com.trait=l.comT, l.trait.dist=l.trait.dist)
	print(yy)
}
save(l.com.yy, file="List_Community_MPD_MFD_and_PhyTraitDistMat_14July16")

#.................
# Make few tests
#.................
load("~/Desktop/Dropbox/Travail_SA/3.ComEcol/CapeCommunities/Data/Results/List_Community_MPD_MFD_and_PhyTraitDistMat_14July16")
names(l.com.yy)  # "2002" "2008" "2011" "2014"
names(l.com.yy[[1]]) # "COM" ; "l.com.phy" ;  "l.phly" ; "l.com.trait" ; "l.trait.dist"

head(l.com.yy$"2002"$COM)
head(l.com.yy$"2008"$COM)
head(l.com.yy$"2011"$COM)
head(l.com.yy$"2014"$COM)

l.com.yy$"2002"$COM$sitID <- unlist(lapply(strsplit(substring(l.com.yy$"2002"$COM$sitID, 2, 10), "_"), function(x) paste(x[1], x[2], sep=".")))

allC <- do.call(rbind, lapply(l.com.yy, function(x) x$COM))
dim(allC) # 778  10
head(allC)

# add information Invaded site or not
allC$Invaded <- ifelse(allC$nbInv>0, 1, 0)


# Try out some plots
par(mfrow=c(2, 3))
plot(nbInv ~ (rich-nbInv), data=allC, xlab="Native Richness", pch=19, col=adjustcolor("#2b8cbe", .5), cex=.8)
plot(nbInv ~ MPD.nat, data=allC, pch=19, col=adjustcolor("#2b8cbe", .5), cex=.8)
plot(nbInv ~ MFD.nat, data=allC, pch=19, col=adjustcolor("#2b8cbe", .5), cex=.8)

boxplot((rich-nbInv) ~ Invaded, data=allC, ylab="Native Richness", xlab="Invaded", col=c("white", "#2b8cbe"))
boxplot(MPD.nat ~ Invaded, data=allC, ylab="Native MPD", xlab="Invaded", col=c("white", "#2b8cbe"))
boxplot(MFD.nat ~ Invaded, data=allC, ylab="Native MFD", xlab="Invaded", col=c("white", "#2b8cbe"))

boxplot(MPD.all ~ Invaded, data=allC, ylab="All MPD", xlab="Invaded", col=c("white", "#2b8cbe"))
boxplot(MFD.all ~ Invaded, data=allC, ylab="All MFD", xlab="Invaded", col=c("white", "#2b8cbe"))

plot(MPD.nat~MPD.inv, data=allC)
plot(MFD.nat~MFD.inv, data=allC)

plot(MPD.nat~MFD.nat, data=allC)
plot(MPD.inv~MFD.inv, data=allC)


# Try out some plots about time series
s4 <- names(table(allC$sitID)[table(allC$sitID)==4])
com4 <- allC[which(allC$sitID %in% s4),]
length(unique(com4$sitID)) # 55
dim(com4)  # 220

p1 <- ggplot(com4, aes(x=year, y=rich, colour=sitID, group=sitID)) + geom_line(show.legend=F)
p2 <- ggplot(com4, aes(x=year, y=nbInv, colour=sitID, group=sitID)) + geom_line(show.legend=F)
p3 <- ggplot(com4, aes(x=year, y=MPD.nat, colour=sitID, group=sitID)) + geom_line(show.legend=F)
p4 <- ggplot(com4, aes(x=year, y=MFD.nat, colour=sitID, group=sitID)) + geom_line(show.legend=F)
p5 <- ggplot(com4, aes(x=year, y=MPD.all, colour=sitID, group=sitID)) + geom_line(show.legend=F)
p6 <- ggplot(com4, aes(x=year, y=MFD.all, colour=sitID, group=sitID)) + geom_line(show.legend=F)
multiplot(p1, p2, p5, p6, cols=2)




#------------------------------------------------
# Get the species x site data : 1 x 1m
#------------------------------------------------
# 2002 / 2008 / 2011 / 2014
#........
sp02_Abun <- read.delim("subPlotSpc_2002_NbIndiv_12July16.txt")
sp02_Cov <- read.delim("subPlotSpc_2002_PropCover_12July16.txt")

sp08_Abun <- read.delim("subPlotSpc_2008_NbIndiv_12July16.txt")
sp08_Cov <- read.delim("subPlotSpc_2008_PropCover_12July16.txt")

sp11_Abun <- read.delim("subPlotSpc_2011_NbIndiv_12July16.txt")
sp11_Cov <- read.delim("subPlotSpc_2011_PropCover_12July16.txt")

sp14_Abun <- read.delim("subPlotSpc_2014_NbIndiv_12July16.txt")
sp14_Cov <- read.delim("subPlotSpc_2014_PropCover_12July16.txt")

sp02_Abun[1:10, 1:10] ; sp02_Cov[1:10, 1:10]
sp08_Abun[1:10, 1:10] ; sp08_Cov[1:10, 1:10]
sp11_Abun[1:10, 1:10] ; sp11_Cov[1:10, 1:10]
sp14_Abun[1:10, 1:10] ; sp14_Cov[1:10, 1:10]


# check if the subplot are in the plot data
tid <- row.names(sp02_Abun)
sp02_Abun$PlotID <- as.numeric(unlist(lapply(strsplit(substring(tid, 2, 3), "_"), function(x) x[[1]])))
head(sp02_Abun, 10)
Rsp02 <- as.data.frame(t(sapply(unique(sp02_Abun$PlotID), function(x) colSums(sp02_Abun[sp02_Abun$PlotID==x,]))))
head(Rsp02)

head(l.SumPlot[[1]])

SPP <- melt(Rsp02, id="PlotID")
PP <- melt(as.data.frame(l.SumPlot[[1]]), id="LocID")

head(SPP)
head(PP)

MPSP <- merge(SPP, PP, by.x="PlotID", by.y="LocID")




























































