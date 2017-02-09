# Laure Gallien	
# le 2 Fev 2017


library(ggplot2)
library(diveRsity)
library(cluster)
library(picante)
library(MCMCglmm)
library(lme4)

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
invID <- sort(tr[which(tr$Alien ==1), "SpcID"])
invID  # 85

# invader's growth form
tr[which(tr$SpcID %in% invID), c("SpcID", "Growth_Form")]
table(tr[which(tr$SpcID %in% invID), "Growth_Form"]) # h (43) : herb ; g (19) : graminoid; t (21) : tree; 1 bulb + 1 other

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
DgrowthF <- daisy(cbind(as.factor(tr$Growth_Form), as.factor(tr$Growth_Form)), metric = "gower")
D1 <- as.matrix(DgrowthF) ; row.names(D1) <- colnames(D1) <- tr$SpcID

# Get the phylogenetic distances (genus level)
# to do!!!

# -----------------------------------------------------
# Calculate richness & functional diversity
# -----------------------------------------------------
DF <- c("sp02_Abun", "sp08_Abun", "sp11_Abun", "sp14_Abun")
YY <- c(2002, 2008, 2011, 2014)


# # Considering all spc ID independent (i.e. genus_sp1 = 1 species)
# ldf <- list()
# for(x in 1:4){
#   temp <- eval(parse(text=DF[x]))
#   RemNam <- unlist(sapply(c("Unknown", "Weed", "Moss"), function(y) grep(y, names(temp))))
#   temp2 <- temp[, !names(temp) %in% RemNam]
#   DD <- D1[row.names(D1) %in% names(temp2), colnames(D1) %in% names(temp2)]
#   temp3 <- temp2[, names(temp2) %in% colnames(DD)]
#   NATnam <- names(temp3)[!names(temp3) %in% invID] ; INVnam <- names(temp3)[colnames(temp3) %in% invID]
#   INV_h <- names(temp3)[names(temp3) %in% invID_herb]
#   INV_g <- names(temp3)[names(temp3) %in% invID_gram]
#   INV_t <- names(temp3)[names(temp3) %in% invID_tree]
#   RichDF <- data.frame(subpID=row.names(temp3), year=YY[x], allRich=apply(temp3, 1, function(x) sum(x>0)),
#                     natRich = apply(temp3[, NATnam], 1, function(x) sum(x>0)),
#                     invRich = apply(temp3[, INVnam], 1, function(x) sum(x>0)),
#                     inv_h_Rich = apply(temp3[, INV_h], 1, function(x) sum(x>0)),
#                     inv_g_Rich = apply(temp3[, INV_g], 1, function(x) sum(x>0)),
#                     inv_t_Rich = apply(temp3[, INV_t], 1, function(x) sum(x>0)))
#   print("rich")
#   FunDivDF <- data.frame(subpID=row.names(temp3), year=YY[x],
#                     allFD=ses.mpd(temp3, DD, runs=99)$mpd.obs.p,
#                     natFD=ses.mpd(temp3[, NATnam], DD[NATnam, NATnam], runs=99)$mpd.obs.p,
#                     invFD=ses.mpd(temp3[, INVnam], DD[INVnam, INVnam], runs=99)$mpd.obs.p,
#                     inv_h_FD=ses.mpd(temp3[, INV_h], DD[INV_h, INV_h], runs=99)$mpd.obs.p,
#                     inv_g_FD=ses.mpd(temp3[, INV_g], DD[INV_g, INV_g], runs=99)$mpd.obs.p,
#                     inv_t_FD=ses.mpd(temp3[, INV_t], DD[INV_t, INV_t], runs=99)$mpd.obs.p)
#   ldf[[x]] <- list(spc_site=temp3, dist=DD, RichDF=RichDF, FunDivDF=FunDivDF)
#   print(paste("div", x))
# }
# names(ldf) <- as.character(YY)
# save(ldf, file=paste(pathRes, "Richness_FunDiversity_allSpcID_indpdt_7Fev17", sep=""))
load(paste(pathRes, "Richness_FunDiversity_allSpcID_indpdt_7Fev17", sep=""))  # ldf
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
# Richness  over time pet treatment
# -------------------------------------------------
# all species together
p1 <- ggplot(dfall, aes(factor(year), allRich)) + geom_boxplot(aes(fill = factor(Aliens))) 
p2 <- ggplot(dfall, aes(factor(year), natRich)) + geom_boxplot(aes(fill = factor(Aliens))) 
p3 <- ggplot(dfall, aes(factor(year), invRich)) + geom_boxplot(aes(fill = factor(Aliens))) 
multiplot(p1, p2, p3, cols=3)

# lmer
# glmm1 <- glmer(natRich ~ Aliens + year + (1|subpID), data=dfall, family="poisson")
               
# MCMCglmm
prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
mod_rich <- MCMCglmm(natRich ~ Aliens + year, random=~subpID, data=dfall, family="poisson", 
                 prior=prior, verbose=F, pr=T)
summary(mod_rich)
plot(mod_rich)


# -------------------------------------------------
# Functional diversity  over time pet treatment
# -------------------------------------------------
p4 <- ggplot(dfall, aes(factor(year), natFD)) + geom_boxplot(aes(fill = factor(Aliens))) 
p4

# Test the MCMCglmm : gaussian distribution 
# FD ~ treatment + time + Alien|Site + 1|Site + 1|site/subplot (+ env?)
head(dfall)

prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
mod1 <- MCMCglmm(natFD ~ Aliens + year, random=~subpID, data=dfall, family="gaussian", 
                  prior=prior, verbose=F, pr=T)
summary(mod1)
plot(mod1)





# -------------------------------------------------
# prepare the dataframe
# -------------------------------------------------
dfall_4y <- dfall[which(dfall$subpID %in% names(l.sid)[sapply(l.sid, function(x) length(x)==4)]), ]
dfall_4y$plotID <- sapply(strsplit(dfall_4y$subpID, "_"), function(x) x[[1]])
head(dfall_4y)

















