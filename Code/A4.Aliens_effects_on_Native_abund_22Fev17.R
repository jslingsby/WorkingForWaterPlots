# Laure Gallien	
# le 2 Fev 2017


library(ggplot2)
library(diveRsity)
library(cluster)
library(picante)
library(MCMCglmm)
library(lme4)
library(mgcv)
library(reshape)

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

# Site treatment information
#------------------------------------------------
SiInf <- read.table("plot_treatments_4Aug16.txt", stringsAsFactors = F, header = T)
head(SiInf)
str(SiInf)



# ##########################################
# Organize the data
# ##########################################

# -------------------------------------------------
# combine all data
# -------------------------------------------------
lDF <- list(sp02_Abun, sp08_Abun, sp11_Abun, sp14_Abun)
lMdf <- lapply(lDF, function(x) { x$sitID <- row.names(x) ; y <- melt(x, "sitID") ; return(y) } )
for(i in 1:4) lMdf[[i]]$year <- c(2002, 2008, 2011, 2014)[i]
alldf <- do.call(rbind, lMdf) 
dim(alldf) # 7981235 
head(alldf)

# -------------------------------------------------
# add up the treatment information
# -------------------------------------------------
alldf$plotID <- sapply(strsplit(alldf$sitID, "_"), function(x) x[[1]])

head(SiInf)
Treatmt <- SiInf[,c("Site", "Aliens")]
Treatmt$col <- ifelse(Treatmt$Aliens=="Invaded", '#e41a1c',ifelse(Treatmt$Aliens=="Cleared", '#377eb8','#4daf4a'))

alldf <- merge(alldf, Treatmt, by.x="plotID", by.y ="Site")
head(alldf)


# -------------------------------------------------
# add up the time since the last fire
# -------------------------------------------------
head(alldf)
alldf$TimeSinceFire <- unlist(sapply(1:nrow(alldf), function(x) {
  temp <- SiInf[SiInf$Site==alldf[x,"plotID"], grep("Burned", names(SiInf))]
  tempb <- as.numeric(sapply(strsplit(names(temp), "_"), function(x) x[[2]])[temp>0])
  tempy <- alldf[x,"year"]
  res <- tempy-tempb[tempy-tempb>=0]
  return(ifelse(length(res)==0, NA, min(res)))  } ) )



# -------------------------------------------------
# Add the growth form information
# -------------------------------------------------
names(alldf)[3] <- "SpcID"

head(tr)
alldf2 <- merge(alldf, tr[,1:2], by="SpcID")
head(alldf2)


# -------------------------------------------------
# select only the native species
# -------------------------------------------------
alldfN <- alldf2[which(alldf2$SpcID %in% natID),]
dim(alldfN) # 655842
head(alldfN)

str(alldfN)

# -------------------------------------------------
# Remove the native species that are completely absent of the plotID all times
# -------------------------------------------------
# tempN <- unique(alldfN[,c(1:2)])
# tempN$NbOcc <- NA
# for(i in unique(tempN[,1])){ # i="Carpacoce_vaginellata"
#   for(j in unique(tempN[,2])){ # j=3
#     ss <- sum(alldfN[which(alldfN$SpcID==i & alldfN$plotID==j),"value"])
#     tempN[which(tempN$SpcID==i & tempN$plotID==j), "NbOcc"] <- ss
#   } ; print(i)
# }
# tempN2 <- tempN[which(tempN$NbOcc>0),]
# save(tempN2, file="/Users/Laure/Dropbox/GIT/2016_CapeCom/Data/Results/Nb_of_spc_occ_per_PlotID_across_all_times_23Fev17")
load("/Users/Laure/Dropbox/GIT/2016_CapeCom/Data/Results/Nb_of_spc_occ_per_PlotID_across_all_times_23Fev17")
alldfN <- alldfN[which(paste(alldfN$SpcID, alldfN$plotID, sep="_") %in% paste(tempN2$SpcID, tempN2$plotID, sep="_")),]


# -------------------------------------------------
# reorganize the treatment levels
# -------------------------------------------------
alldfN$Aliens2 <- factor(alldfN$Aliens, levels=c("No Aliens", "Cleared", "Invaded"))

# -------------------------------------------------
# only look at the sites that burnt only once
# -------------------------------------------------
f2000 <- SiInf[which(SiInf$Burned_2000==1 & rowSums(SiInf[,10:15])==0), "Site"]
alldfN_f <- alldfN[which(alldfN$plotID %in% f2000),]
dim(alldfN_f) # 18235  29


# ####################################################################################
# ####################################################################################
# ####################################################################################
# ####################################################################################
# Make the plots
# ####################################################################################
# ####################################################################################
# ####################################################################################
# ####################################################################################
COLS <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00',
          '#cab2d6','#6a3d9a','#ffff99','#b15928')
COLS2 <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5')

# ####################################################################################
# ####################################################################################
# ALL NATIVE SPECIES TOGETHER -- ABUNDANCES
# ####################################################################################
# ####################################################################################
#............................
# All data   --> OK
#............................
alldfN <- alldfN[!is.na(alldfN$TimeSinceFire),]
alldfN <- alldfN[which(alldfN$Growth_Form %in% c("e", "g", "h", "t")),]
alldfN$value <- ifelse(alldfN$value==0.1, 1, alldfN$value)

prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
mod_ab <- MCMCglmm(value ~ Aliens2 * TimeSinceFire, random=~sitID, data=alldfN, # family="poisson", 
                 prior=prior, verbose=F, pr=T)
summary(mod_ab)
1-var(alldfN$value-predict(mod_ab)[,1])/var(alldfN$value) # 0.16

#............................
# All data + growth form   --> OK
#............................
alldfN_gf <- alldfN
alldfN_gf$Growth_Form <- ifelse(alldfN_gf$Growth_Form=="t", "e", alldfN_gf$Growth_Form)
alldfN_gf$Aliens <- ifelse(alldfN_gf$Aliens=="Cleared", "No Aliens", alldfN_gf$Aliens)
alldfN_gf$Aliens2 <- factor(alldfN_gf$Aliens, levels=c("No Aliens", "Invaded"))

mod_ab <- MCMCglmm(value ~ Aliens2 * TimeSinceFire + Growth_Form, random=~sitID, data=alldfN_gf, # family="poisson", 
                   prior=prior, verbose=F, pr=T)
summary(mod_ab)
1-var(alldfN_gf$value-predict(mod_ab)[,1])/var(alldfN_gf$value) # 0.16


#............................
# site which only burnt on 2001  --> OK
#............................
alldfN_f <- alldfN[which(alldfN$plotID %in% f2000),]
mod_ab <- MCMCglmm(value ~ Aliens2 * TimeSinceFire, random=~sitID, data=alldfN_f, # family="poisson", 
                   prior=prior, verbose=F, pr=T)
summary(mod_ab)
1-var(alldfN_f$value-predict(mod_ab)[,1])/var(alldfN_f$value) # 0.16


#............................
# site which only burnt on 2001 + growth form  --> OK
#............................
alldfN_f <- alldfN_gf[which(alldfN_gf$plotID %in% f2000),]
mod_ab <- MCMCglmm(value ~ Aliens2 * TimeSinceFire + Growth_Form, random=~sitID, data=alldfN_f, # family="poisson", 
                   prior=prior, verbose=F, pr=T)
summary(mod_ab)
1-var(alldfN_f$value-predict(mod_ab)[,1])/var(alldfN_f$value) # 0.16



# ####################################################################################
# ####################################################################################
# SEPARATE THE DIFFERENT GROWTH FORMS
# ####################################################################################
# ####################################################################################

#............................
# All data   --> OK
#............................
alldfN_h <- alldfN[which(alldfN$Growth_Form=="h"),]
alldfN_g <- alldfN[which(alldfN$Growth_Form=="g"),]
alldfN_e <- alldfN[which(alldfN$Growth_Form=="e"),]
alldfN_t <- alldfN[which(alldfN$Growth_Form=="t"),]

prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
mod_h_rich <- MCMCglmm(value ~ Aliens2 * TimeSinceFire, random=~sitID, , data=alldfN_h, # family="poisson", 
                       verbose=F, pr=T, prior=prior)
mod_g_rich <- MCMCglmm(value ~ Aliens2 * TimeSinceFire, random=~sitID, data=alldfN_g, # family="poisson", 
                       verbose=F, pr=T, prior=prior)
mod_e_rich <- MCMCglmm(value ~ Aliens2 * TimeSinceFire, random=~sitID, data=alldfN_e, # family="poisson", 
                       verbose=F, pr=T, prior=prior)
mod_t_rich <- MCMCglmm(value ~ Aliens2 * TimeSinceFire, random=~sitID, data=alldfN_t, # family="poisson", 
                       verbose=F, pr=T, prior=prior)
summary(mod_h_rich)
summary(mod_g_rich)
summary(mod_e_rich)
summary(mod_t_rich)

1-var(alldfN_h$value-predict(mod_h_rich)[,1])/var(alldfN_h$value) # 0.15
1-var(alldfN_g$value-predict(mod_g_rich)[,1])/var(alldfN_g$value) # 0.15
1-var(alldfN_e$value-predict(mod_e_rich)[,1])/var(alldfN_e$value) # 0.11
1-var(alldfN_t$value-predict(mod_t_rich)[,1])/var(alldfN_t$value) # 0.09


#............................
# site which only burnt on 2001  --> OK
#............................
dfNams <- c("alldfN_h", "alldfN_g", "alldfN_e", "alldfN_t") ; gtyp <- c("h", "g", "e", "t")
for(i in 1:4){
  temp <- alldfN[which(alldfN$Growth_Form==gtyp[i]),]
  # temp2 <- temp[which(temp$plotID %in% f2000 & temp$Aliens %in% c("No Aliens", "Invaded")),]
  # temp2$Aliens2 <- factor(temp2$Aliens, levels=c("No Aliens", "Invaded"))
  temp$Aliens2 <- factor(temp$Aliens, levels=c("No Aliens", "Cleared", "Invaded"))
  eval(parse(text=paste(dfNams[i], "<- temp"))) }

mod_h_rich <- MCMCglmm(value ~ Aliens2 * TimeSinceFire, random=~sitID, data=alldfN_h, # family="poisson", 
                       verbose=F, pr=T, prior=prior)
mod_g_rich <- MCMCglmm(value ~ Aliens2 * TimeSinceFire, random=~sitID, data=alldfN_g, # family="poisson", 
                       verbose=F, pr=T, prior=prior)
mod_e_rich <- MCMCglmm(value ~ Aliens2 * TimeSinceFire, random=~sitID, data=alldfN_e, # family="poisson", 
                       verbose=F, pr=T, prior=prior)
mod_t_rich <- MCMCglmm(value ~ Aliens2 * TimeSinceFire, random=~sitID, data=alldfN_t, # family="poisson", 
                       verbose=F, pr=T, prior=prior)
summary(mod_h_rich)
summary(mod_g_rich)
summary(mod_e_rich)
summary(mod_t_rich)

1-var(alldfN_h$value-predict(mod_h_rich)[,1])/var(alldfN_h$value) # 0.15
1-var(alldfN_g$value-predict(mod_g_rich)[,1])/var(alldfN_g$value) # 0.15
1-var(alldfN_e$value-predict(mod_e_rich)[,1])/var(alldfN_e$value) # 0.11
1-var(alldfN_t$value-predict(mod_t_rich)[,1])/var(alldfN_t$value) # 0.09







# ####################################################################################
# ####################################################################################
# INVASIVE SPECIES -- ABUNDANCES
# ####################################################################################
# ####################################################################################

# select only the ALIEN species
alldfI <- alldf2[-which(alldf2$SpcID %in% natID),]

# Remove the native species that are completely absent of the plotID all times
tempN <- unique(alldfI[,c(1:2)])
tempN$NbOcc <- NA
for(i in unique(tempN[,1])){ # i="Carpacoce_vaginellata"
  for(j in unique(tempN[,2])){ # j=3
    ss <- sum(alldfI[which(alldfI$SpcID==i & alldfI$plotID==j),"value"])
    tempN[which(tempN$SpcID==i & tempN$plotID==j), "NbOcc"] <- ss  } ; print(i) }
tempN2 <- tempN[which(tempN$NbOcc>0),]
alldfI <- alldfI[which(paste(alldfI$SpcID, alldfI$plotID, sep="_") %in% paste(tempN2$SpcID, tempN2$plotID, sep="_")),]

# Select only the invaded sites
alldfI <- alldfI[which(alldfI$Aliens=="Invaded"),]
dim(alldfI) # 2164

# only look at the sites that burnt only once
alldfI_f <- alldfI[which(alldfI$plotID %in% f2000),]
dim(alldfI_f) # 660  29


#............................
# All data   --> OK
#............................
alldfI <- alldfI[!is.na(alldfI$TimeSinceFire),]
alldfI$value <- ifelse(alldfI$value==0.1, 1, alldfI$value)

mod_ab <- MCMCglmm(value ~ TimeSinceFire, random=~sitID, data=alldfI, # family="poisson", 
                   prior=prior, verbose=F, pr=T)
summary(mod_ab)
1-var(alldfI$value-predict(mod_ab)[,1])/var(alldfI$value) # 0.16

#............................
# All data + growth form   --> OK
#............................
alldfI_gf <- alldfI
alldfI_gf$Growth_Form <- ifelse(alldfI_gf$Growth_Form=="h", "g", alldfI_gf$Growth_Form)
mod_ab <- MCMCglmm(value ~ TimeSinceFire * Growth_Form, random=~sitID, data=alldfI_gf, # family="poisson", 
                   prior=prior, verbose=F, pr=T)
summary(mod_ab)
1-var(alldfI_gf$value-predict(mod_ab)[,1])/var(alldfI_gf$value) # 0.16


#............................
# site which only burnt on 2001  --> OK
#............................
alldfI_f <- alldfI[which(alldfI$plotID %in% f2000),]
mod_ab <- MCMCglmm(value ~ TimeSinceFire, random=~sitID, data=alldfI_f, # family="poisson", 
                   prior=prior, verbose=F, pr=T)
summary(mod_ab)
1-var(alldfI_f$value-predict(mod_ab)[,1])/var(alldfI_f$value) # 0.16


#............................
# site which only burnt on 2001 + growth form  --> OK
#............................
alldfI_gf <- alldfI[which(alldfI$plotID %in% f2000),]
 alldfI_gf$Growth_Form <- ifelse(alldfI_gf$Growth_Form=="h", "g", alldfI_gf$Growth_Form)
mod_ab <- MCMCglmm(value ~ TimeSinceFire * Growth_Form, random=~sitID, data=alldfI_gf, # family="poisson", 
                   prior=prior, verbose=F, pr=T)
summary(mod_ab)
1-var(alldfI_gf$value-predict(mod_ab)[,1])/var(alldfI_gf$value) # 0.16




# ####################################################################################
# ####################################################################################
# INVASIVE SPECIES -->  SEPARATE THE DIFFERENT GROWTH FORMS
# ####################################################################################
# ####################################################################################

#............................
# All data   --> OK
#............................
alldfI_h <- alldfI[which(alldfI$Growth_Form=="h"),]
alldfI_g <- alldfI[which(alldfI$Growth_Form=="g"),]
alldfI_t <- alldfI[which(alldfI$Growth_Form=="t"),]

mod_h_rich <- MCMCglmm(value ~ TimeSinceFire, random=~sitID, data=alldfI_h, # family="poisson", 
                       verbose=F, pr=T, prior=prior)
mod_g_rich <- MCMCglmm(value ~ TimeSinceFire, random=~sitID, data=alldfI_g, # family="poisson", 
                       verbose=F, pr=T, prior=prior)
mod_t_rich <- MCMCglmm(value ~ TimeSinceFire, random=~sitID, data=alldfI_t, # family="poisson", 
                       verbose=F, pr=T, prior=prior)
summary(mod_h_rich)
summary(mod_g_rich)
summary(mod_t_rich)

1-var(alldfI_h$value-predict(mod_h_rich)[,1])/var(alldfI_h$value) # 0.15
1-var(alldfI_g$value-predict(mod_g_rich)[,1])/var(alldfI_g$value) # 0.15
1-var(alldfI_t$value-predict(mod_t_rich)[,1])/var(alldfI_t$value) # 0.09


#............................
# site which only burnt on 2001  --> OK
#............................
alldfI_h <- alldfI[which(alldfI$Growth_Form=="h" & alldfI$plotID %in% f2000),]
alldfI_g <- alldfI[which(alldfI$Growth_Form=="g" & alldfI$plotID %in% f2000),]
alldfI_t <- alldfI[which(alldfI$Growth_Form=="t" & alldfI$plotID %in% f2000),]

mod_h_rich <- MCMCglmm(value ~ TimeSinceFire, random=~sitID, data=alldfI_h, # family="poisson", 
                       verbose=F, pr=T, prior=prior)
mod_g_rich <- MCMCglmm(value ~ TimeSinceFire, random=~sitID, data=alldfI_g, # family="poisson", 
                       verbose=F, pr=T, prior=prior)
mod_t_rich <- MCMCglmm(value ~ TimeSinceFire, random=~sitID, data=alldfI_t, # family="poisson", 
                       verbose=F, pr=T, prior=prior)
summary(mod_h_rich)
summary(mod_g_rich)
summary(mod_t_rich)

1-var(alldfI_h$value-predict(mod_h_rich)[,1])/var(alldfI_h$value) # 0.15
1-var(alldfI_g$value-predict(mod_g_rich)[,1])/var(alldfI_g$value) # 0.15
1-var(alldfI_t$value-predict(mod_t_rich)[,1])/var(alldfI_t$value) # 0.09









