# Laure Gallien	
# le 2 Fev 2017


library(ggplot2)
library(diveRsity)
library(MCMCglmm)
library(lme4)

if (Sys.getenv("USER")=="jasper") {setwd("/Users/jasper/Dropbox/Shared/CapeCommunities/Data/Raw/")}
if (Sys.getenv("USER")=="Laure") {setwd("~/Dropbox/GIT/2016_CapeCom/Data/LaurePrep/")}


# ##########################################
# Load the data (sub-plot scale)
# ##########################################

# Get the species x site data : 1 x 1m
#------------------------------------------------
sp02_Abun <- read.delim("subPlotSpc_2002_NbIndiv_2Fev17.txt", stringsAsFactors = F)
sp08_Abun <- read.delim("subPlotSpc_2008_NbIndiv_2Fev17.txt", stringsAsFactors = F)
sp11_Abun <- read.delim("subPlotSpc_2011_NbIndiv_2Fev17.txt", stringsAsFactors = F)
sp14_Abun <- read.delim("subPlotSpc_2014_NbIndiv_2Fev17.txt", stringsAsFactors = F)

sp02_Cov <- read.delim("subPlotSpc_2002_PropCover_2Fev17.txt", stringsAsFactors = F)
sp08_Cov <- read.delim("subPlotSpc_2008_PropCover_2Fev17.txt", stringsAsFactors = F)
sp11_Cov <- read.delim("subPlotSpc_2011_PropCover_2Fev17.txt", stringsAsFactors = F)
sp14_Cov <- read.delim("subPlotSpc_2014_PropCover_2Fev17.txt", stringsAsFactors = F)

sp02_Abun[1:10, 1:10] ; sp02_Cov[1:10, 1:10]
sp08_Abun[1:10, 1:10] ; sp08_Cov[1:10, 1:10]
sp11_Abun[1:10, 1:10] ; sp11_Cov[1:10, 1:10]
sp14_Abun[1:10, 1:10] ; sp14_Cov[1:10, 1:10]
class(unlist(sp02_Abun)) ; class(unlist(sp08_Abun)) ; class(unlist(sp11_Abun)) ; class(unlist(sp14_Abun))


# Trait data
#------------------------------------------------
tr <- read.delim("All_Trait_Quanti_26Jan17.txt", stringsAsFactors = F)
head(tr)
str(tr)
invID <- sort(tr[which(tr$Alien ==1), "SpcID"])
invID  # 85

# invader's growth form
tr[which(tr$SpcID %in% invID), c("SpcID", "Growth_Form")]
table(tr[which(tr$SpcID %in% invID), "Growth_Form"]) # h (43) : herb ; g (19) : graminoid; t (21) : tree; 1 bulb + 1 other

invID_herb <- invID[which(invID %in% tr[which(tr$Growth_Form =="h"), "SpcID"])]
invID_gram <- invID[which(invID %in% tr[which(tr$Growth_Form =="g"), "SpcID"])]
invID_tree <- invID[which(invID %in% tr[which(tr$Growth_Form =="t"), "SpcID"])]


# Site treatment information
#------------------------------------------------
SiInf <- read.table("plot_treatments_4Aug16.txt", stringsAsFactors = F, header = T)
head(SiInf)
str(SiInf)



# ##########################################
# Organize the data
# ##########################################
DF <- c("sp02_Abun", "sp08_Abun", "sp11_Abun", "sp14_Abun")
YY <- c(2002, 2008, 2011, 2014)

# Calculate richness
ldf <- lapply(1:4, function(x){
  temp <- eval(parse(text=DF[x]))
  return(data.frame(subpID=row.names(temp), year=YY[x], allRich=apply(temp, 1, function(x) sum(x>0)),
                natRich = apply(temp[,!(names(temp) %in% invID)], 1, function(x) sum(x>0)), 
                invRich = apply(temp[, (names(temp) %in% invID)], 1, function(x) sum(x>0)),
                inv_h_Rich = apply(temp[, (names(temp) %in% invID_herb)], 1, function(x) sum(x>0)),
                inv_g_Rich = apply(temp[, (names(temp) %in% invID_gram)], 1, function(x) sum(x>0)),
                inv_t_Rich = apply(temp[, (names(temp) %in% invID_tree)], 1, function(x) sum(x>0))) ) })

dfall <- do.call(rbind, ldf)
dfall$subpID <- as.character(dfall$subpID)
head(dfall)
str(dfall)

# check matching plot ID
l.sid <- sapply(sort(unique(dfall$subpID)), function(x) dfall[which(dfall$subpID==x), "year"] )



# ##########################################
# Make the plots
# ##########################################
COLS <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00',
          '#cab2d6','#6a3d9a','#ffff99','#b15928')
COLS2 <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5')

# -------------------------------------------------
# Simple richness over time plots
# -------------------------------------------------
plot(allRich ~ jitter(year), data=dfall, cex=0.5, pch=19, ylab = "Richness", xlab="year")
points(natRich ~ jitter(year), data=dfall, cex=0.5, pch=19, col="blue")
points(invRich ~ jitter(year), data=dfall, cex=0.5, pch=19, col="red")
legend("topright", legend=c("total", "native", "alien"), col=c("black", "blue", "red"), pch=19, cex=0.8)

# -------------------------------------------------
# prepare the dataframe
# -------------------------------------------------
dfall_4y <- dfall[which(dfall$subpID %in% names(l.sid)[sapply(l.sid, function(x) length(x)==4)]), ]
dfall_4y$plotID <- sapply(strsplit(dfall_4y$subpID, "_"), function(x) x[[1]])
head(dfall_4y)


# -------------------------------------------------
# color the same plots with the same colors
# -------------------------------------------------
UsubpID <- unique(dfall_4y$subpID)
UpID <- unique(dfall_4y$plotID) ; names(UpID) <- rev(COLS)[1:length(UpID)]

par(mfrow=c(1,3))
plot(0,0, type="n", ylim = c(0,4), xlim = c(2002, 2014), main="alien herbs", ylab = "Richness", xlab = "year")
for(i in UsubpID) {   temp <- dfall_4y[which(dfall_4y$subpID %in% i),] ;
  points(jitter(inv_h_Rich) ~ jitter(year), data=temp, type="l", col=names(UpID[UpID==temp$plotID[1]]), lwd=2) }

plot(0,0, type="n", ylim = c(0,4), xlim = c(2002, 2014), main="alien grasses", ylab = "Richness", xlab = "year")
for(i in UsubpID) {   temp <- dfall_4y[which(dfall_4y$subpID %in% i),] ;
  points(jitter(inv_g_Rich) ~ jitter(year), data=temp, type="l", col=names(UpID[UpID==temp$plotID[1]]), lwd=2) }

plot(0,0, type="n", ylim = c(0,4), xlim = c(2002, 2014), main="alien trees", ylab = "Richness", xlab = "year")
for(i in UsubpID) {  temp <- dfall_4y[which(dfall_4y$subpID %in% i),] ;
  points(jitter(inv_t_Rich) ~ jitter(year), data=temp, type="l", col=names(UpID[UpID==temp$plotID[1]]), lwd=2) }
legend("topright", legend=UpID, col=names(UpID), lty=1)


# -------------------------------------------------
# add up the treatment information
# -------------------------------------------------
head(SiInf)
Treatmt <- SiInf[,c("Site", "Aliens")]
Treatmt$col <- ifelse(Treatmt$Aliens=="Invaded", '#e41a1c',ifelse(Treatmt$Aliens=="Cleared", '#377eb8','#4daf4a'))

par(mfrow=c(2,3))
plot(0,0, type="n", ylim = c(0,35), xlim = c(2002, 2014), main="All species", ylab = "Richness", xlab = "year")
for(i in UsubpID) {   temp <- dfall_4y[which(dfall_4y$subpID %in% i),] ; cc=Treatmt[Treatmt$Site==temp$plotID[1],3]
points(jitter(allRich) ~ jitter(year), data=temp, type="l", col=cc, lwd=1) }

plot(0,0, type="n", ylim = c(0,35), xlim = c(2002, 2014), main="Natives only", ylab = "Richness", xlab = "year")
for(i in UsubpID) {   temp <- dfall_4y[which(dfall_4y$subpID %in% i),] ; cc=Treatmt[Treatmt$Site==temp$plotID[1],3]
points(jitter(natRich) ~ jitter(year), data=temp, type="l", col=cc, lwd=1) }

plot(0,0, type="n", ylim = c(0,10), xlim = c(2002, 2014), main="Invader only", ylab = "Richness", xlab = "year")
for(i in UsubpID) {   temp <- dfall_4y[which(dfall_4y$subpID %in% i),] ; cc=Treatmt[Treatmt$Site==temp$plotID[1],3]
points(jitter(invRich) ~ jitter(year), data=temp, type="l", col=cc, lwd=1) }

plot(0,0, type="n", ylim = c(0,4), xlim = c(2002, 2014), main="alien herbs", ylab = "Richness", xlab = "year")
for(i in UsubpID) {   temp <- dfall_4y[which(dfall_4y$subpID %in% i),] ; cc=Treatmt[Treatmt$Site==temp$plotID[1],3]
points(jitter(inv_h_Rich) ~ jitter(year), data=temp, type="l", col=cc, lwd=1) }

plot(0,0, type="n", ylim = c(0,4), xlim = c(2002, 2014), main="alien grasses", ylab = "Richness", xlab = "year")
for(i in UsubpID) {   temp <- dfall_4y[which(dfall_4y$subpID %in% i),]; cc=Treatmt[Treatmt$Site==temp$plotID[1],3]
points(jitter(inv_g_Rich) ~ jitter(year), data=temp, type="l", col=cc, lwd=1) }

plot(0,0, type="n", ylim = c(0,4), xlim = c(2002, 2014), main="alien trees", ylab = "Richness", xlab = "year")
for(i in UsubpID) {  temp <- dfall_4y[which(dfall_4y$subpID %in% i),] ; cc=Treatmt[Treatmt$Site==temp$plotID[1],3]
points(jitter(inv_t_Rich) ~ jitter(year), data=temp, type="l", col=cc, lwd=1) }
legend("topright", legend=unique(Treatmt$Aliens), col=unique(Treatmt$col), lty=1, lwd=2)













