# Laure Gallien	
# le 2 Fev 2017


library(ggplot2)
library(diveRsity)
library(cluster)
library(picante)
library(MCMCglmm)
library(lme4)
library(mgcv)

if (Sys.getenv("USER")=="jasper") {setwd("/Users/jasper/Dropbox/Shared/CapeCommunities/Data/Raw/")}
if (Sys.getenv("USER")=="Laure") {setwd("~/Dropbox/GIT/2016_CapeCom/Data/LaurePrep/")}

pathRes <- "/Users/Laure/Dropbox/GIT/2016_CapeCom/Results/"

# ##########################################
# Load the data (sub-plot scale)
# ##########################################

# Get the species x site data : 1 x 1m
#------------------------------------------------
sp02_Abun <- read.delim("subPlotSpc_2002_NbIndiv_2Fev17.txt", stringsAsFactors = F)
sp08_Abun <- read.delim("subPlotSpc_2008_NbIndiv_2Fev17.txt", stringsAsFactors = F)
sp14_Abun <- read.delim("subPlotSpc_2014_NbIndiv_2Fev17.txt", stringsAsFactors = F)
sp11_Abun <- read.delim("subPlotSpc_2011_NbIndiv_2Fev17.txt", stringsAsFactors = F)

sp02_Cov <- read.delim("subPlotSpc_2002_PropCover_2Fev17.txt", stringsAsFactors = F)
sp08_Cov <- read.delim("subPlotSpc_2008_PropCover_2Fev17.txt", stringsAsFactors = F)
sp11_Cov <- read.delim("subPlotSpc_2011_PropCover_2Fev17.txt", stringsAsFactors = F)
sp14_Cov <- read.delim("subPlotSpc_2014_PropCover_2Fev17.txt", stringsAsFactors = F)

sp02_Abun[1:10, 1:10] ; sp02_Cov[1:10, 1:10]
sp08_Abun[1:10, 1:10] ; sp08_Cov[1:10, 1:10]
sp11_Abun[1:10, 1:10] ; sp11_Cov[1:10, 1:10]
sp14_Abun[1:10, 1:10] ; sp14_Cov[1:10, 1:10]
class(unlist(sp02_Abun)) ; class(unlist(sp08_Abun)) ; class(unlist(sp11_Abun)) ; class(unlist(sp14_Abun))


# Get species names
#------------------------------------------------
lspc_sp <- sort(unique(c(names(sp02_Abun), names(sp08_Abun), 
                         names(sp11_Abun), names(sp14_Abun))))
length(lspc_sp)  # 959


# Trait data
#------------------------------------------------
tr <- read.delim("All_Trait_Quanti_26Jan17.txt", stringsAsFactors = F)
head(tr)
str(tr)

# native's growth form
natID <- sort(tr[-which(tr$Alien ==1), "SpcID"])
table(tr[tr$SpcID %in% natID, "Growth_Form"]) # herb(h)=348; graminoid(g)=178; ericoid(e)=147; bulb(b)=125 ; tree(t)=55;
natID_herb <- natID[which(natID %in% tr[which(tr$Growth_Form =="h"), "SpcID"])]
natID_gram <- natID[which(natID %in% tr[which(tr$Growth_Form =="g"), "SpcID"])]
natID_eric <- natID[which(natID %in% tr[which(tr$Growth_Form =="e"), "SpcID"])]
natID_bulb <- natID[which(natID %in% tr[which(tr$Growth_Form =="b"), "SpcID"])]
natID_tree <- natID[which(natID %in% tr[which(tr$Growth_Form =="t"), "SpcID"])]

# invader's growth form
invID <- sort(tr[which(tr$Alien ==1), "SpcID"])
invID  # 85

tr[which(tr$SpcID %in% invID), c("SpcID", "Growth_Form")]
table(tr[which(tr$SpcID %in% invID), "Growth_Form"]) # h (43) : herb ; g (19) : graminoid; t (21) : tree
invID_herb <- invID[which(invID %in% tr[which(tr$Growth_Form =="h"), "SpcID"])]
invID_gram <- invID[which(invID %in% tr[which(tr$Growth_Form =="g"), "SpcID"])]
invID_tree <- invID[which(invID %in% tr[which(tr$Growth_Form =="t"), "SpcID"])]


# # Phylogenetic data  # to discuss with Jasper on how to add the missing genera!!!!!!!!!!!!!
# #------------------------------------------------
# load("~/Dropbox/Travail WSL/1.MetaAnalyse/Datasets/Phylogeny/Angiosperm/Zanne et al 2014 Sci/Angios_genus") #
# zan <- Angios_genus
# zan
# 
# # Extract the genera
# head(lspc_sp)
# lgen_sp <- unique(sapply(strsplit(lspc_sp, "_"), function(x) x[1]))
# length(lgen_sp)  # 324
# lgen_spP <- lgen_sp[lgen_sp %in% zan$tip.label]
# length(lgen_spP) # 278 / 324  = 86% present in the phylogeny
# 
# # species to add to phylogeny
# lgen_sp[!(lgen_sp %in% zan$tip.label)] #  "Apiaceae", "Asteraceae", "Blechnum", "Bulb"
#             # "Campanulaceae", "Carpacoce", "Colopsis", "Cyperaceae", "Elytropappus", "Ericoid"
#             # "ExoticWeed", "Fagelia", "Fern", "Gleichenia", "Grass", "Hilliardiella", "Histiopteris"
#             # "Hypochoeris", "Hypodiscus", "Iridacea", "Itasina", "Mesemb", "Microcodon", "Morea", "Moss"
#             # "Muraltia", "MustardSeed", "Nanobubon", "Orchid", "Osteopermum", "Pentaschistis", "Phyllopodium"             # "Pinus", "Poaceae", "Pteridium", "Schizaea", "Seriphium", "Soroveta", "Tenaxia", "Thunbergiella"
#             # "Todea", "Unknown", "Vygie", "Weed", "Widdringtonia", "Zyrphelis"
# 
# # Look at it
# phylo <- drop.tip(zan, zan$tip.label[!zan$tip.label %in% lgen_sp])
# plot(phylo, "f", cex=0.5)
# 

# Site treatment information
#------------------------------------------------
SiInf <- read.table("plot_treatments_4Aug16.txt", stringsAsFactors = F, header = T)
head(SiInf)
str(SiInf)



# ##########################################
# Organize the data
# ##########################################

# estimate functional distances
#.................................
# DgrowthF <- daisy(cbind(as.factor(tr$Growth_Form), as.factor(tr$Growth_Form)), metric = "gower")
# D1 <- as.matrix(DgrowthF) ; row.names(D1) <- colnames(D1) <- tr$SpcID

TRAITS <- names(tr)[2:8]
str(tr[,TRAITS])
for(i in TRAITS) tr[,i] <- as.factor(tr[,i])

# all traits
DallF <- daisy(tr[,TRAITS], metric = "gower")
D1 <- as.matrix(DallF) ; row.names(D1) <- colnames(D1) <- tr$SpcID

# # all traits but growth form
# DFssGF <- daisy(tr[,TRAITS[2:length(TRAITS)]], metric = "gower")
# D1 <- as.matrix(DFssGF) ; row.names(D1) <- colnames(D1) <- tr$SpcID
# 
# Get the phylogenetic distances (genus level)
#.................................
# to do!!!

# -----------------------------------------------------
# Calculate richness & functional diversity
# -----------------------------------------------------
DF <- c("sp02_Abun", "sp08_Abun", "sp11_Abun", "sp14_Abun")
YY <- c(2002, 2008, 2011, 2014)


# # # Considering all spc ID independent (i.e. genus_sp1 = 1 species)
# ldf <- list()
# for(x in 1:4){
#   temp <- eval(parse(text=DF[x]))
#   RemNam <- unlist(sapply(c("Unknown", "Weed", "Moss"), function(y) grep(y, names(temp))))
#   temp2 <- temp[, !names(temp) %in% RemNam]
#   DD <- D1[row.names(D1) %in% names(temp2), colnames(D1) %in% names(temp2)]
#   temp3 <- temp2[, names(temp2) %in% colnames(DD)]
#   NATnam <- names(temp3)[!names(temp3) %in% invID] ; INVnam <- names(temp3)[colnames(temp3) %in% invID]
#   NAT_h <- names(temp3)[names(temp3) %in% natID_herb]
#   NAT_g <- names(temp3)[names(temp3) %in% natID_gram]
#   NAT_e <- names(temp3)[names(temp3) %in% natID_eric]
#   NAT_b <- names(temp3)[names(temp3) %in% natID_bulb]
#   NAT_t <- names(temp3)[names(temp3) %in% natID_tree]
# 
#   INV_h <- names(temp3)[names(temp3) %in% invID_herb]
#   INV_g <- names(temp3)[names(temp3) %in% invID_gram]
#   INV_t <- names(temp3)[names(temp3) %in% invID_tree]
# 
#   RichDF <- data.frame(subpID=row.names(temp3), year=YY[x], allRich=apply(temp3, 1, function(x) sum(x>0)),
#                     natRich = apply(temp3[, NATnam], 1, function(x) sum(x>0)),
#                     nat_h_Rich = apply(temp3[, NAT_h], 1, function(x) sum(x>0)),
#                     nat_g_Rich = apply(temp3[, NAT_g], 1, function(x) sum(x>0)),
#                     nat_e_Rich = apply(temp3[, NAT_e], 1, function(x) sum(x>0)),
#                     nat_b_Rich = apply(temp3[, NAT_b], 1, function(x) sum(x>0)),
#                     nat_t_Rich = apply(temp3[, NAT_t], 1, function(x) sum(x>0)),
#                     invRich = apply(temp3[, INVnam], 1, function(x) sum(x>0)),
#                     inv_h_Rich = apply(temp3[, INV_h], 1, function(x) sum(x>0)),
#                     inv_g_Rich = apply(temp3[, INV_g], 1, function(x) sum(x>0)),
#                     inv_t_Rich = apply(temp3[, INV_t], 1, function(x) sum(x>0)))
#   print("rich")
#   tNh <- cbind(temp3[,NAT_h], apply(temp3[,NATnam[!NATnam %in% NAT_h]],1:2, function(x) return(0)))
#   tNg <- cbind(temp3[,NAT_g], apply(temp3[,NATnam[!NATnam %in% NAT_g]],1:2, function(x) return(0)))
#   tNe <- cbind(temp3[,NAT_e], apply(temp3[,NATnam[!NATnam %in% NAT_e]],1:2, function(x) return(0)))
#   tNb <- cbind(temp3[,NAT_b], apply(temp3[,NATnam[!NATnam %in% NAT_b]],1:2, function(x) return(0)))
#   tNt <- cbind(temp3[,NAT_t], apply(temp3[,NATnam[!NATnam %in% NAT_t]],1:2, function(x) return(0)))
#   tIh <- cbind(temp3[,INV_h], apply(temp3[,INVnam[!INVnam %in% INV_h]],1:2, function(x) return(0)))
#   tIg <- cbind(temp3[,INV_g], apply(temp3[,INVnam[!INVnam %in% INV_g]],1:2, function(x) return(0)))
#   tIt <- cbind(temp3[,INV_t], apply(temp3[,INVnam[!INVnam %in% INV_t]],1:2, function(x) return(0)))
#   FunDivDF <- data.frame(subpID=row.names(temp3), year=YY[x],
#                     allFD=ses.mpd(temp3, DD, runs=99)$mpd.obs.p,
#                     natFD=ses.mpd(temp3[, NATnam], DD[NATnam, NATnam], runs=99)$mpd.obs.p,
#                     nat_h_FD = ses.mpd(tNh, DD[NATnam, NATnam], runs=99)$mpd.obs.p,
#                     nat_g_FD = ses.mpd(tNg, DD[NATnam, NATnam], runs=99)$mpd.obs.p,
#                     nat_e_FD = ses.mpd(tNe, DD[NATnam, NATnam], runs=99)$mpd.obs.p,
#                     nat_b_FD = ses.mpd(tNb, DD[NATnam, NATnam], runs=99)$mpd.obs.p,
#                     nat_t_FD = ses.mpd(tNt, DD[NATnam, NATnam], runs=99)$mpd.obs.p,
#                     invFD=ses.mpd(temp3[, INVnam], DD[INVnam, INVnam], runs=99)$mpd.obs.p,
#                     inv_h_FD=ses.mpd(tIh, DD[INVnam, INVnam], runs=99)$mpd.obs.p,
#                     inv_g_FD=ses.mpd(tIg, DD[INVnam, INVnam], runs=99)$mpd.obs.p,
#                     inv_t_FD=ses.mpd(tIt, DD[INVnam, INVnam], runs=99)$mpd.obs.p)
#   ldf[[x]] <- list(spc_site=temp3, dist=DD, RichDF=RichDF, FunDivDF=FunDivDF)
#   print(paste("div", x))
# }
# names(ldf) <- as.character(YY)
# save(ldf, file=paste(pathRes, "Richness_FunDiversity_allTraits_allSpcIDindpdt_10Fev17", sep=""))
# save(ldf, file=paste(pathRes, "Richness_FunDiversity_allTraitsBUTnotGrowthForm_allSpcIDindpdt_13Fev17", sep=""))
load(paste(pathRes, "Richness_FunDiversity_allTraits_allSpcIDindpdt_9Fev17", sep=""))  # ldf
load(paste(pathRes, "Richness_FunDiversity_allTraitsBUTnotGrowthForm_allSpcIDindpdt_13Fev17", sep=""))  # ldf
names(ldf[[1]])
head(ldf[[1]]$FunDivDF)
head(ldf[[1]]$RichDF)

# merge the dataframes
df1 <- merge(ldf[[1]]$RichDF, ldf[[1]]$FunDivDF, by="subpID")
df2 <- merge(ldf[[2]]$RichDF, ldf[[2]]$FunDivDF, by="subpID")
df3 <- merge(ldf[[3]]$RichDF, ldf[[3]]$FunDivDF, by="subpID")
df4 <- merge(ldf[[4]]$RichDF, ldf[[4]]$FunDivDF, by="subpID")
dfall <- do.call(rbind, list(df1, df2, df3, df4))
dfall <- dfall[, !names(dfall)=="year.y"]
dfall$subpID <- as.character(dfall$subpID)
names(dfall)[2] <- "year"
str(dfall)
head(dfall)
dim(dfall) # 1601

# Remove species not identified at the species level --> TO DO

# Group species per genus id --> TO DO


# check matching plot ID
l.sid <- sapply(sort(unique(dfall$subpID)), function(x) dfall[which(dfall$subpID==x), "year"] )




# ##########################################
# Make the plots
# ##########################################
COLS <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00',
          '#cab2d6','#6a3d9a','#ffff99','#b15928')
COLS2 <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5')


# -------------------------------------------------
# add up the treatment information
# -------------------------------------------------
dfall$plotID <- sapply(strsplit(dfall$subpID, "_"), function(x) x[[1]])

head(SiInf)
Treatmt <- SiInf[,c("Site", "Aliens")]
Treatmt$col <- ifelse(Treatmt$Aliens=="Invaded", '#e41a1c',ifelse(Treatmt$Aliens=="Cleared", '#377eb8','#4daf4a'))

dfall <- merge(dfall, Treatmt, by.x="plotID", by.y ="Site")
head(dfall)


# -------------------------------------------------
# add up the time since the last fire
# -------------------------------------------------
head(dfall)
dfall$TimeSinceFire <- unlist(sapply(1:nrow(dfall), function(x) {
                          temp <- SiInf[SiInf$Site==dfall[x,"plotID"], grep("Burned", names(SiInf))]
                          tempb <- as.numeric(sapply(strsplit(names(temp), "_"), function(x) x[[2]])[temp>0])
                          tempy <- dfall[x,"year"]
                          res <- tempy-tempb[tempy-tempb>=0]
                          return(ifelse(length(res)==0, NA, min(res)))  } ) )


# -------------------------------------------------
# Richness change the time since the last fire per treatment
# -------------------------------------------------
# color per sub-site ID
#............................
par(mfrow=c(2,3))
for(j in c("nat_h_Rich", "nat_g_Rich", "nat_e_Rich", "nat_b_Rich", "nat_t_Rich")){
  for(i in unique(dfall$subpID)){
    temp <- dfall[dfall$subpID==i,] ; temp2 <- temp[order(temp$TimeSinceFire),] 
    if(i == unique(dfall$subpID)[1]) { plot(jitter(temp2[,j]) ~ temp2$TimeSinceFire, main=j, type="l", 
                                            ylim=c(0,15), xlim=c(0,14), col=sample(COLS, 1)) 
    } else { points(jitter(temp2[,j]) ~ temp2$TimeSinceFire, type="l", col=sample(COLS, 1)) }
  }
}  

# color per treatment
#............................
# Natives
par(mfrow=c(2,3))
for(j in c("nat_h_Rich", "nat_g_Rich", "nat_e_Rich", "nat_b_Rich", "nat_t_Rich")){
  for(i in unique(dfall$subpID)){
    temp <- dfall[dfall$subpID==i,] ; temp2 <- temp[order(temp$TimeSinceFire),] 
    if(i == unique(dfall$subpID)[1]) { plot(jitter(temp2[,j]) ~ temp2$TimeSinceFire, main=j, type="l", 
        ylim=c(0,15), xlim=c(0,14), col=temp2$col[1]) 
    } else { points(jitter(temp2[,j]) ~ temp2$TimeSinceFire, type="l", col=temp2$col[1]) }
  }
}  
legend("topright", legend = unique(dfall[,26:27])[,1], col=unique(dfall[,26:27])[,2], lty=1)


# Invasives
par(mfrow=c(2,2))
for(j in c("invRich", "inv_h_Rich", "inv_g_Rich", "inv_t_Rich")){
  for(i in unique(dfall$subpID)){
    temp <- dfall[dfall$subpID==i,] ; temp2 <- temp[order(temp$TimeSinceFire),] 
    if(i == unique(dfall$subpID)[1]) { plot(jitter(temp2[,j]) ~ temp2$TimeSinceFire, main=j, type="l", 
          ylab="Richness", xlab="time since fire", ylim=c(0,6), xlim=c(0,14), col=temp2$col[1]) 
    } else { points(jitter(temp2[,j]) ~ temp2$TimeSinceFire, type="l", col=temp2$col[1]) }
  }
}  
legend("topright", legend = unique(dfall[,26:27])[,1], col=unique(dfall[,26:27])[,2], lty=1)



# Look at averages and sd
#............................
# Natives
par(mfrow=c(2,3))
for(j in c("nat_h_Rich", "nat_g_Rich", "nat_e_Rich", "nat_b_Rich", "nat_t_Rich")){
  for(i in unique(dfall$Aliens)){
      temp <- dfall[dfall$Aliens==i,] 
      fireY <- sort(unique(temp$TimeSinceFire))
      tMean <- sapply(fireY, function(x) mean(temp[temp$TimeSinceFire==x, j], na.rm=T))
      tSD <- sapply(fireY, function(x) sd(temp[temp$TimeSinceFire==x, j], na.rm=T))
      if(i==unique(dfall$Aliens)[1]) { 
        plot(0,0, type="n", main=j, ylab="Richness", xlab="time since fire", ylim=c(0,7), xlim=c(0,14)) } 
      points(jitter(tMean) ~ fireY, type="l", col=temp$col[1], lwd=2) 
      polygon(c(fireY, rev(fireY)), c((tMean+tSD), rev(tMean-tSD)), col=adjustcolor(temp$col[1], .3), border = F)    }    
} 
legend("topright", legend = unique(dfall[,26:27])[,1], col=unique(dfall[,26:27])[,2], lty=1)


# Invasives
par(mfrow=c(2,2))
for(j in c("invRich", "inv_h_Rich", "inv_g_Rich", "inv_t_Rich")){
  for(i in unique(dfall$Aliens)){
    temp <- dfall[dfall$Aliens==i,] 
    fireY <- sort(unique(temp$TimeSinceFire))
    tMean <- sapply(fireY, function(x) mean(temp[temp$TimeSinceFire==x, j], na.rm=T))
    tSD <- sapply(fireY, function(x) sd(temp[temp$TimeSinceFire==x, j], na.rm=T))
    if(i==unique(dfall$Aliens)[1]) { 
      plot(0,0, type="n", main=j, ylab="Richness", xlab="time since fire", ylim=c(0,3), xlim=c(0,14)) } 
    points(jitter(tMean) ~ fireY, type="l", col=temp$col[1], lwd=2) 
    polygon(c(fireY, rev(fireY)), c((tMean+tSD), rev(tMean-tSD)), col=adjustcolor(temp$col[1], .3), border = F)    }    
} 
legend("topright", legend = unique(dfall[,26:27])[,1], col=unique(dfall[,26:27])[,2], lty=1)


#............................
# MCMCglmm on native richness
#............................
prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
dfall2 <- dfall[!is.na(dfall$TimeSinceFire),]
mod_rich <- MCMCglmm(natRich ~ Aliens + TimeSinceFire, random=~subpID, data=dfall2, family="poisson", 
                 prior=prior, verbose=F, pr=T)
summary(mod_rich)
plot(mod_rich)

# estimate R2
RES <- dfall2$natRich-predict(mod_rich)[,1]
1-var(RES)/var(dfall2$natRich) # 0.16


#............................
# MCMCglmm on native growth forms
#............................
prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
mod_h_rich <- MCMCglmm(nat_h_Rich ~ Aliens + TimeSinceFire, random=~subpID, data=dfall2, family="poisson", 
                     prior=prior, verbose=F, pr=T)
mod_g_rich <- MCMCglmm(nat_g_Rich ~ Aliens + TimeSinceFire, random=~subpID, data=dfall2, family="poisson", 
                       prior=prior, verbose=F, pr=T)
mod_e_rich <- MCMCglmm(nat_e_Rich ~ Aliens + TimeSinceFire, random=~subpID, data=dfall2, family="poisson", 
                       prior=prior, verbose=F, pr=T)
mod_b_rich <- MCMCglmm(nat_b_Rich ~ Aliens + TimeSinceFire, random=~subpID, data=dfall2, family="poisson", 
                       prior=prior, verbose=F, pr=T)
mod_t_rich <- MCMCglmm(nat_t_Rich ~ Aliens + TimeSinceFire, random=~subpID, data=dfall2, family="poisson", 
                       prior=prior, verbose=F, pr=T)
summary(mod_h_rich)
summary(mod_g_rich)
summary(mod_e_rich)
summary(mod_b_rich)
summary(mod_t_rich)
plot(mod_h_rich)

1-var(dfall2$nat_h_Rich-predict(mod_h_rich)[,1])/var(dfall2$nat_h_Rich) # 0.11
1-var(dfall2$nat_g_Rich-predict(mod_g_rich)[,1])/var(dfall2$nat_g_Rich) # 0.15
1-var(dfall2$nat_e_Rich-predict(mod_e_rich)[,1])/var(dfall2$nat_e_Rich) # 0.11
1-var(dfall2$nat_b_Rich-predict(mod_b_rich)[,1])/var(dfall2$nat_b_Rich) # 0.03
1-var(dfall2$nat_t_Rich-predict(mod_t_rich)[,1])/var(dfall2$nat_t_Rich) # 0.09


# -------------------------------------------------
# Functional diversity  over time pet treatment
# -------------------------------------------------
# color per treatment
#............................
# Natives
par(mfrow=c(2,3))
for(j in c("natFD", "nat_g_FD", "nat_e_FD", "nat_b_FD", "nat_t_FD")){
  for(i in unique(dfall$subpID)){
    temp <- dfall[dfall$subpID==i,] ; temp2 <- temp[order(temp$TimeSinceFire),] 
    if(i == unique(dfall$subpID)[1]) plot(0, 0, main=j, type="n", ylim=c(0,1), xlim=c(0,14))  
    if(sum(is.na(temp2[,j]))!=nrow(temp2)) {
      points(jitter(temp2[,j]) ~ temp2$TimeSinceFire, type="l", col=temp2$col[1]) }
  }
}  
legend("topright", legend = unique(dfall[,26:27])[,1], col=unique(dfall[,26:27])[,2], lty=1)


# Invasives
par(mfrow=c(2,2))
for(j in c("invFD", "inv_h_FD", "inv_g_FD", "inv_t_FD")){
  for(i in unique(dfall$subpID)){
    temp <- dfall[dfall$subpID==i,] ; temp2 <- temp[order(temp$TimeSinceFire),] 
    if(i == unique(dfall$subpID)[1]) plot(0, 0, main=j, type="n", ylim=c(0,1), xlim=c(0,14))  
    if(sum(is.na(temp2[,j]))!=nrow(temp2)) {
      points(jitter(temp2[,j]) ~ temp2$TimeSinceFire, type="l", col=temp2$col[1]) }
  }
}  
legend("topright", legend = unique(dfall[,26:27])[,1], col=unique(dfall[,26:27])[,2], lty=1)


# Look at averages and sd
#............................
# Natives
par(mfrow=c(2,3))
for(j in c("natFD", "nat_g_FD", "nat_e_FD", "nat_b_FD", "nat_t_FD")){
  for(i in unique(dfall$Aliens)){
    temp <- dfall[dfall$Aliens==i,] 
    fireY <- sort(unique(temp$TimeSinceFire))
    tMean <- sapply(fireY, function(x) mean(temp[temp$TimeSinceFire==x, j], na.rm=T))
    tSD <- sapply(fireY, function(x) sd(temp[temp$TimeSinceFire==x, j], na.rm=T))
    if(i==unique(dfall$Aliens)[1]) { 
      plot(0,0, type="n", main=j, ylab="Diversity", xlab="time since fire", ylim=c(0,1), xlim=c(0,14)) } 
    points(jitter(tMean) ~ fireY, type="l", col=temp$col[1], lwd=2) 
    polygon(c(fireY, rev(fireY)), c((tMean+tSD), rev(tMean-tSD)), col=adjustcolor(temp$col[1], .3), border = F)    }    
} 
legend("topright", legend = unique(dfall[,26:27])[,1], col=unique(dfall[,26:27])[,2], lty=1)


# Test the MCMCglmm : gaussian distribution 
#............................
prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
mod1 <- MCMCglmm(natFD ~ Aliens * TimeSinceFire, random=~subpID, data=dfall2, family="gaussian", 
                  prior=prior, verbose=F, pr=T)
summary(mod1)
plot(mod1)
1-var(na.omit(dfall2$natFD-predict(mod1)[,1]))/var(na.omit(dfall2$natFD)) # 0.01

# Test the MCMCglmm : replace the NAs with 0
dfall3 <- dfall[!is.na(dfall$TimeSinceFire),]
dfall3$natFD <- ifelse(is.na(dfall3$natFD), 0, dfall3$natFD)
prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
mod1 <- MCMCglmm(natFD ~ Aliens * TimeSinceFire + Aliens:I(TimeSinceFire^2), random=~subpID, data=dfall3, family="gaussian", prior=prior, verbose=F, pr=T)
summary(mod1)
# plot(mod1)
1-var(na.omit(dfall3$natFD-predict(mod1)[,1]))/var(na.omit(dfall3$natFD)) # 0.02


# Test the gamm 
#............................
gam1 <- gamm(natFD ~ Aliens * TimeSinceFire, random=~subpID, data=dfall2, family="gaussian", 
                 prior=prior, verbose=F, pr=T)
summary(mod1)
plot(mod1)
1-var(na.omit(dfall2$natFD-predict(mod1)[,1]))/var(na.omit(dfall2$natFD)) # 0.01



# Look at averages and sd : replace the NAs with 0
par(mfrow=c(2,3))
for(j in c("natFD", "nat_h_FD", "nat_g_FD", "nat_e_FD", "nat_b_FD", "nat_t_FD")){
  for(i in unique(dfall3$Aliens)){
    temp <- dfall3[dfall3$Aliens==i,] 
    fireY <- sort(unique(temp$TimeSinceFire))
    tMean <- sapply(fireY, function(x) mean(temp[temp$TimeSinceFire==x, j], na.rm=T))
    tSD <- sapply(fireY, function(x) sd(temp[temp$TimeSinceFire==x, j], na.rm=T))
    tMean <- ifelse(is.na(tMean), 0, tMean) ; tSD <- ifelse(is.na(tSD), 0, tSD) 
    if(i==unique(dfall$Aliens)[1]) { 
      plot(0,0, type="n", main=j, ylab="Richness", xlab="time since fire", ylim=c(0,1), xlim=c(0,14)) } 
    points(jitter(tMean) ~ fireY, type="l", col=temp$col[1], lwd=2) 
    polygon(c(fireY, rev(fireY)), c((tMean+tSD), rev(tMean-tSD)), col=adjustcolor(temp$col[1], .3), border = F)    }    
} 
legend("topright", legend = unique(dfall[,26:27])[,1], col=unique(dfall[,26:27])[,2], lty=1)


# -------------------------------------------------
# FD per native species type  over time per treatment
# -------------------------------------------------
# MCMCglmm
prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
mod_h_div <- MCMCglmm(nat_h_FD ~ Aliens + year, random=~subpID, data=dfall, family="gaussian", 
                       prior=prior, verbose=F, pr=T)
mod_g_div <- MCMCglmm(nat_g_FD ~ Aliens + year, random=~subpID, data=dfall, family="gaussian", 
                       prior=prior, verbose=F, pr=T)
mod_e_div <- MCMCglmm(nat_e_FD ~ Aliens + year, random=~subpID, data=dfall, family="gaussian", 
                       prior=prior, verbose=F, pr=T)
mod_b_div <- MCMCglmm(nat_b_FD ~ Aliens + year, random=~subpID, data=dfall, family="gaussian", 
                       prior=prior, verbose=F, pr=T)
mod_t_div <- MCMCglmm(nat_t_FD ~ Aliens + year, random=~subpID, data=dfall, family="gaussian", 
                       prior=prior, verbose=F, pr=T)
summary(mod_h_div)
summary(mod_g_div)
summary(mod_e_div)
summary(mod_b_div)
summary(mod_t_div)


# -------------------------------------------------
# prepare the dataframe
# -------------------------------------------------
dfall_4y <- dfall[which(dfall$subpID %in% names(l.sid)[sapply(l.sid, function(x) length(x)==4)]), ]
dfall_4y$plotID <- sapply(strsplit(dfall_4y$subpID, "_"), function(x) x[[1]])
head(dfall_4y)

















