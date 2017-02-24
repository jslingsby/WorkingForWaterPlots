# Laure Gallien	
# le 2 Fev 2017


library(ggplot2)
library(diveRsity)
library(cluster)
library(picante)
library(MCMCglmm)
library(lme4)
library(vegan)

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

DF <- c("sp02_Cov", "sp08_Cov", "sp11_Cov", "sp14_Cov")
YY <- c(2002, 2008, 2011, 2014)

# -----------------------------------------------------
# Calculate proportion of invasive/native species cover
# -----------------------------------------------------
# # Considering all spc ID independent (i.e. genus_sp1 = 1 species)
# ldf <- list()
# for(x in 1:4){
#   temp <- eval(parse(text=DF[x]))
#   RemNam <- unlist(sapply(c("Unknown", "Weed", "Moss"), function(y) grep(y, names(temp))))
#   temp3 <- temp[, !names(temp) %in% RemNam]
#   NATnam <- names(temp3)[!names(temp3) %in% invID]    ; INVnam <- names(temp3)[colnames(temp3) %in% invID]
#   NAT_h <- names(temp3)[names(temp3) %in% natID_herb] ; NAT_g <- names(temp3)[names(temp3) %in% natID_gram]
#   NAT_e <- names(temp3)[names(temp3) %in% natID_eric] ; NAT_b <- names(temp3)[names(temp3) %in% natID_bulb]
#   NAT_t <- names(temp3)[names(temp3) %in% natID_tree] ; INV_h <- names(temp3)[names(temp3) %in% invID_herb]
#   INV_g <- names(temp3)[names(temp3) %in% invID_gram] ; INV_t <- names(temp3)[names(temp3) %in% invID_tree]
# 
#   InvNatCovDF <- data.frame(subpID=row.names(temp3), year=YY[x], TotCov = rowSums(temp3),
#                             natCov = rowSums(temp3[, NATnam]), nat_h_Cov = rowSums(temp3[, NAT_h]),
#                             nat_g_Cov = rowSums(temp3[, NAT_g]), nat_e_Cov = rowSums(temp3[, NAT_e]),
#                             nat_b_Cov = rowSums(temp3[, NAT_b]), nat_t_Cov = rowSums(temp3[, NAT_t]),
#                             invCov = rowSums(temp3[, INVnam]), inv_h_Cov = rowSums(temp3[, INV_h]),
#                             inv_g_Cov = rowSums(temp3[, INV_g]), inv_t_Cov = rowSums(temp3[, INV_t]))
#   ldf[[x]] <- list(spc_site=temp3, RichDF=InvNatCovDF)
# }
# names(ldf) <- as.character(YY)
# save(ldf, file=paste(pathRes, "Inv-Nat_propCover_22Fev17", sep=""))



# -----------------------------------------------------
# merge the dataframes
# -----------------------------------------------------
load(paste(pathRes, "Inv-Nat_propCover_22Fev17", sep=""))  # ldf

dfall <- do.call(rbind, list(ldf[[1]][[2]], ldf[[2]][[2]], ldf[[3]][[2]], ldf[[4]][[2]]))
dfall$subpID <- as.character(dfall$subpID)
names(dfall)[2] <- "year"
str(dfall)
head(dfall)
dim(dfall) # 1601

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
# reorganize the treatment levels
# -------------------------------------------------
head(dfall)
dfall$Aliens2 <- factor(dfall$Aliens, levels=c("No Aliens", "Cleared", "Invaded"))

# -------------------------------------------------
# only look at the sites that burnt only once
# -------------------------------------------------
f2000 <- SiInf[which(SiInf$Burned_2000==1 & rowSums(SiInf[,10:15])==0), "Site"]
dfall_f <- dfall[which(dfall$plotID %in% f2000),]
dim(dfall_f) # 424  29

# -------------------------------------------------
# TOTAL species cover over time pet treatment
# -------------------------------------------------

dfall <- dfall[!is.na(dfall$TimeSinceFire),]
dfall$propNatCov <- dfall$natCov/dfall$TotCov
dfall$propNatCov <- ifelse(dfall$TotCov==0, 1, dfall$propNatCov)
# dfall <- dfall[which(dfall$plotID %in% f2000),]


# all species together
p1 <- ggplot(dfall, aes(factor(year), TotCov)) + geom_boxplot(aes(fill = Aliens2)) 
p2 <- ggplot(dfall, aes(factor(year), natCov)) + geom_boxplot(aes(fill = Aliens2)) 
p3 <- ggplot(dfall, aes(factor(year), invCov)) + geom_boxplot(aes(fill = Aliens2)) 
p4 <- ggplot(dfall, aes(factor(year), invCov/TotCov)) + geom_boxplot(aes(fill = Aliens2)) 
p5 <- ggplot(dfall, aes(factor(year), natCov/TotCov)) + geom_boxplot(aes(fill = Aliens2)) 
multiplot(p1, p2, p3, p4, p5, cols=3)

# MCMCglmm
prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
mod_rich <- MCMCglmm(natCov ~ Aliens2 * TimeSinceFire, random=~subpID, data=dfall, prior=prior, verbose=F, pr=T)
summary(mod_rich)
1-var(dfall$natCov-predict(mod_rich)[,1])/var(dfall$natCov) # 0.15
 
# Add the inv covers
mod_rich <- MCMCglmm(natCov ~ Aliens2 * TimeSinceFire + Aliens2:invCov, random=~subpID, data=dfall, prior=prior, verbose=F, pr=T)
summary(mod_rich)
1-var(dfall$natCov-predict(mod_rich)[,1])/var(dfall$natCov) # 0.62 / 0.42



# -------------------------------------------------
# NATIVE species cover over time per growth form and per treatment
# -------------------------------------------------
# all species together
p1 <- ggplot(dfall, aes(factor(year), nat_h_Cov)) + geom_boxplot(aes(fill = factor(Aliens))) 
p2 <- ggplot(dfall, aes(factor(year), nat_g_Cov)) + geom_boxplot(aes(fill = factor(Aliens))) 
p3 <- ggplot(dfall, aes(factor(year), nat_e_Cov)) + geom_boxplot(aes(fill = factor(Aliens))) 
p4 <- ggplot(dfall, aes(factor(year), nat_b_Cov)) + geom_boxplot(aes(fill = factor(Aliens))) 
p5 <- ggplot(dfall, aes(factor(year), nat_t_Cov)) + geom_boxplot(aes(fill = factor(Aliens))) 
multiplot(p1, p2, p3, p4, p5, cols=3)
# 
# p1 <- ggplot(dfall, aes(factor(year), nat_h_Cov/TotCov)) + geom_boxplot(aes(fill = factor(Aliens))) 
# p2 <- ggplot(dfall, aes(factor(year), nat_g_Cov/TotCov)) + geom_boxplot(aes(fill = factor(Aliens))) 
# p3 <- ggplot(dfall, aes(factor(year), nat_e_Cov/TotCov)) + geom_boxplot(aes(fill = factor(Aliens))) 
# p4 <- ggplot(dfall, aes(factor(year), nat_b_Cov/TotCov)) + geom_boxplot(aes(fill = factor(Aliens))) 
# p5 <- ggplot(dfall, aes(factor(year), nat_t_Cov/TotCov)) + geom_boxplot(aes(fill = factor(Aliens))) 
# multiplot(p1, p2, p3, p4, p5, cols=3)


# MCMCglmm
prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
mod_h_rich <- MCMCglmm(nat_h_Cov ~ Aliens2 * TimeSinceFire, random=~subpID, data=dfall,  
                     prior=prior, verbose=F, pr=T)
mod_g_rich <- MCMCglmm(nat_g_Cov ~ Aliens2 * TimeSinceFire, random=~subpID, data=dfall,  
                       prior=prior, verbose=F, pr=T)
mod_e_rich <- MCMCglmm(nat_e_Cov ~ Aliens2 * TimeSinceFire, random=~subpID, data=dfall,  
                       prior=prior, verbose=F, pr=T)
mod_t_rich <- MCMCglmm(nat_t_Cov ~ Aliens2 * TimeSinceFire, random=~subpID, data=dfall,  
                       prior=prior, verbose=F, pr=T)
summary(mod_h_rich)
summary(mod_g_rich)
summary(mod_e_rich)
summary(mod_t_rich)

1-var(dfall$nat_h_Cov-predict(mod_h_rich)[,1])/var(dfall$nat_h_Cov) # 0.17
1-var(dfall$nat_g_Cov-predict(mod_g_rich)[,1])/var(dfall$nat_g_Cov) # 0.14
1-var(dfall$nat_e_Cov-predict(mod_e_rich)[,1])/var(dfall$nat_e_Cov) # 0.13
1-var(dfall$nat_t_Cov-predict(mod_t_rich)[,1])/var(dfall$nat_t_Cov) # 0.08

# # Prop cover
# dfall$N_h_propN <- ifelse(dfall$TotCov==0, 0, dfall$nat_h_Cov/dfall$TotCov)
# dfall$N_g_propN <- ifelse(dfall$TotCov==0, 0, dfall$nat_g_Cov/dfall$TotCov)
# dfall$N_e_propN <- ifelse(dfall$TotCov==0, 0, dfall$nat_e_Cov/dfall$TotCov)
# dfall$N_b_propN <- ifelse(dfall$TotCov==0, 0, dfall$nat_b_Cov/dfall$TotCov)
# dfall$N_t_propN <- ifelse(dfall$TotCov==0, 0, dfall$nat_t_Cov/dfall$TotCov)
# 
# prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
# mod_h_rich <- MCMCglmm(N_h_propN ~ Aliens2 * TimeSinceFire, random=~subpID, data=dfall,  
#                        prior=prior, verbose=F, pr=T)
# mod_g_rich <- MCMCglmm(N_g_propN ~ Aliens2 * TimeSinceFire, random=~subpID, data=dfall,  
#                        prior=prior, verbose=F, pr=T)
# mod_e_rich <- MCMCglmm(N_e_propN ~ Aliens2 * TimeSinceFire, random=~subpID, data=dfall,  
#                        prior=prior, verbose=F, pr=T)
# mod_b_rich <- MCMCglmm(N_b_propN ~ Aliens2 * TimeSinceFire, random=~subpID, data=dfall,  
#                        prior=prior, verbose=F, pr=T)
# mod_t_rich <- MCMCglmm(N_t_propN ~ Aliens2 * TimeSinceFire, random=~subpID, data=dfall,  
#                        prior=prior, verbose=F, pr=T)
# summary(mod_h_rich)
# summary(mod_g_rich)
# summary(mod_e_rich)
# summary(mod_b_rich)
# summary(mod_t_rich)
# 
# 1-var(dfall$N_h_propN-predict(mod_h_rich)[,1])/var(dfall$N_h_propN) # 0.17
# 1-var(dfall$N_g_propN-predict(mod_g_rich)[,1])/var(dfall$N_g_propN) # 0.17
# 1-var(dfall$N_e_propN-predict(mod_e_rich)[,1])/var(dfall$N_e_propN) # 0.17
# 1-var(dfall$N_b_propN-predict(mod_b_rich)[,1])/var(dfall$N_b_propN) # 0.17
# 1-var(dfall$N_t_propN-predict(mod_t_rich)[,1])/var(dfall$N_t_propN) # 0.17

# -------------------------------------------------
# TOTAL INVASIVE Cover over time per treatment
# -------------------------------------------------
head(dfall)
dfall$propInvCov <- dfall$invCov/dfall$TotCov
dfall$propInvCov <- ifelse(dfall$TotCov==0, 1, dfall$propNatCov)
dfallI <- dfall[which(dfall$Aliens=="Invaded"),] 
# dfallI <- dfallI[which(dfallI$plotID %in% f2000),]

mod_rich <- MCMCglmm(invCov ~ TimeSinceFire, random=~subpID, data=dfallI, prior=prior, verbose=F, pr=T)
summary(mod_rich)
1-var(dfallI$invCov-predict(mod_rich)[,1])/var(dfallI$invCov) # 0.15

# -------------------------------------------------
# INVASIVE Richness per native species type  over time per treatment
# -------------------------------------------------
# all species together
p1 <- ggplot(dfall, aes(factor(year), inv_h_Cov)) + geom_boxplot(aes(fill = factor(Aliens))) 
p2 <- ggplot(dfall, aes(factor(year), inv_g_Cov)) + geom_boxplot(aes(fill = factor(Aliens))) 
p3 <- ggplot(dfall, aes(factor(year), inv_t_Cov)) + geom_boxplot(aes(fill = factor(Aliens))) 
multiplot(p1, p2, p3, cols=3)

mod_h_rich <- MCMCglmm(inv_h_Cov ~ TimeSinceFire, random=~subpID, data=dfallI, prior=prior, verbose=F, pr=T)
mod_g_rich <- MCMCglmm(inv_g_Cov ~ TimeSinceFire, random=~subpID, data=dfallI, prior=prior, verbose=F, pr=T)
mod_t_rich <- MCMCglmm(inv_t_Cov ~ TimeSinceFire, random=~subpID, data=dfallI, prior=prior, verbose=F, pr=T)

summary(mod_h_rich)
summary(mod_g_rich)
summary(mod_t_rich)

1-var(dfallI$inv_h_Cov-predict(mod_h_rich)[,1])/var(dfallI$inv_h_Cov) # 0.17
1-var(dfallI$inv_g_Cov-predict(mod_g_rich)[,1])/var(dfallI$inv_g_Cov) # 0.14
1-var(dfallI$inv_t_Cov-predict(mod_t_rich)[,1])/var(dfallI$inv_t_Cov) # 0.08


# prop cover 
p1 <- ggplot(dfall, aes(factor(year), inv_h_Cov/TotCov)) + geom_boxplot(aes(fill = factor(Aliens))) 
p2 <- ggplot(dfall, aes(factor(year), inv_g_Cov/TotCov)) + geom_boxplot(aes(fill = factor(Aliens))) 
p3 <- ggplot(dfall, aes(factor(year), inv_t_Cov/TotCov)) + geom_boxplot(aes(fill = factor(Aliens))) 
multiplot(p1, p2, p3, cols=3)


mod_h_rich <- MCMCglmm(inv_h_Cov/TotCov ~ TimeSinceFire, random=~subpID, data=dfallI, prior=prior, verbose=F, pr=T)
mod_g_rich <- MCMCglmm(inv_g_Cov/TotCov ~ TimeSinceFire, random=~subpID, data=dfallI, prior=prior, verbose=F, pr=T)
mod_t_rich <- MCMCglmm(inv_t_Cov/TotCov ~ TimeSinceFire, random=~subpID, data=dfallI, prior=prior, verbose=F, pr=T)

summary(mod_h_rich)
summary(mod_g_rich)
summary(mod_t_rich)






