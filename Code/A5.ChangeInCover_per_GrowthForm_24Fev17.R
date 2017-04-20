# Laure Gallien	
# le 24 Fev 2017


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
# ##########################################
# Load the data (sub-plot scale)
# ##########################################
# ##########################################

# Get the species x site data : 1 x 1m
#------------------------------------------------
sp02_Cov <- read.delim("subPlotSpc_2002_PropCover_2Fev17.txt", stringsAsFactors = F)
sp08_Cov <- read.delim("subPlotSpc_2008_PropCover_2Fev17.txt", stringsAsFactors = F)
sp11_Cov <- read.delim("subPlotSpc_2011_PropCover_2Fev17.txt", stringsAsFactors = F)
sp14_Cov <- read.delim("subPlotSpc_2014_PropCover_2Fev17.txt", stringsAsFactors = F)

sp02_Cov[1:10, 1:10] ; sp08_Cov[1:10, 1:10] ; sp11_Cov[1:10, 1:10] ; sp14_Cov[1:10, 1:10]


# Get species names
#------------------------------------------------
lspc_sp <- sort(unique(c(names(sp02_Cov), names(sp08_Cov), 
                         names(sp11_Cov), names(sp14_Cov))))
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
# ##########################################
# Organize the data
# ##########################################
# ##########################################

DF <- c("sp02_Cov", "sp08_Cov", "sp11_Cov", "sp14_Cov")
YY <- c(2002, 2008, 2011, 2014)
load(paste(pathRes, "Inv-Nat_propCover_22Fev17", sep=""))  # ldf

dfall <- do.call(rbind, list(ldf[[1]][[2]], ldf[[2]][[2]], ldf[[3]][[2]], ldf[[4]][[2]]))
dfall$subpID <- as.character(dfall$subpID)
names(dfall)[2] <- "year"
str(dfall)
head(dfall)
dim(dfall) # 1601

# check matching plot ID
l.sid <- sapply(sort(unique(dfall$subpID)), function(x) dfall[which(dfall$subpID==x), "year"] )


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


# ##########################################
# ##########################################
# Set up the change in abundance between time periods
# ##########################################
# ##########################################

# set up the time periods
timeF <- expand.grid(t2=c(2014, 2011, 2008, 2002), t1=c(2014, 2011, 2008, 2002))
timeF$dT <- timeF$t2-timeF$t1
timeF <- timeF[timeF$dT>0,]
timeF <- timeF[order(timeF$t2, decreasing=T),]

# reformat the dataset
head(dfall_f)
NewD_f <- lapply(unique(dfall_f$subpID), function(i) {
  temp <- dfall_f[dfall_f$subpID==i,]
  if(nrow(temp)>1){
    ty <- temp$year
    tT <- timeF[timeF$t2 %in% ty & timeF$t1 %in% ty,]
    res <- data.frame(t(sapply(1:nrow(tT), function(x) {
      COV <- temp[which(temp$year == tT[x, 1]), 4:14]-temp[which(temp$year == tT[x, 2]), 4:14]
      names(COV) <- paste("D", names(COV), sep="_")
      T1 <- temp[temp$year == tT[x,"t1"],4:14] 
      names(T1) <- paste("t1", names(T1), sep="_")
      RES <- data.frame(temp[1,1:2], tT[x,], COV, T1, temp[1,c(15:16,18)])
      return(RES)  } ) ) )  
    return(res)   } } )
NewD_f <- NewD_f[!sapply(NewD_f, is.null)]
NewD_f <- do.call(rbind, NewD_f)

for(i in c(1:2, 28:30)) NewD_f[,i] <- as.character(unlist(NewD_f[,i]))
for(i in c(3:27)) NewD_f[,i] <- as.numeric(as.character(unlist(NewD_f[,i])))
NewD_f <- NewD_f[which(NewD_f$Aliens != "Cleared"),]
NewD_f$Aliens2 <- factor(NewD_f$Aliens, levels=c("No Aliens", "Invaded"))
head(NewD_f)
dim(NewD_f) # 391
str(NewD_f)


# ##########################################
# ##########################################
# Make the plots
# ##########################################
# ##########################################

NewD_f <- NewD_f[NewD_f$dT!=9,]

# -------------------------------------------------
# Look at the general changes in abundances
# -------------------------------------------------
ggplot(NewD_f, aes(factor(dT), D_TotCov)) + geom_boxplot(aes(fill = factor(Aliens))) 

prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))
mod_rich <- MCMCglmm(D_TotCov ~ Aliens2 * dT + t1_TotCov, random=~subpID, data=NewD_f, prior=prior, verbose=F, pr=T)
summary(mod_rich)
1-var(NewD_f$D_TotCov-predict(mod_rich)[,1])/var(NewD_f$D_TotCov) # 0.10


# -------------------------------------------------
# Change in species cover over time per growth form and per treatment
# -------------------------------------------------
head(NewD_f)

# all species together
p1 <- ggplot(NewD_f, aes(factor(dT), D_nat_h_Cov)) + geom_boxplot(aes(fill = factor(Aliens))) 
p2 <- ggplot(NewD_f, aes(factor(dT), D_nat_g_Cov)) + geom_boxplot(aes(fill = factor(Aliens))) 
p3 <- ggplot(NewD_f, aes(factor(dT), D_nat_e_Cov)) + geom_boxplot(aes(fill = factor(Aliens))) 
p4 <- ggplot(NewD_f, aes(factor(dT), D_nat_b_Cov)) + geom_boxplot(aes(fill = factor(Aliens))) 
p5 <- ggplot(NewD_f, aes(factor(dT), D_nat_t_Cov)) + geom_boxplot(aes(fill = factor(Aliens))) 
p6 <- ggplot(NewD_f, aes(factor(dT), D_inv_h_Cov)) + geom_boxplot(aes(fill = factor(Aliens))) 
p7 <- ggplot(NewD_f, aes(factor(dT), D_inv_g_Cov)) + geom_boxplot(aes(fill = factor(Aliens))) 
p8 <- ggplot(NewD_f, aes(factor(dT), D_inv_t_Cov)) + geom_boxplot(aes(fill = factor(Aliens))) 
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, cols=3)


# Invaded sites only
# ......................
NewD_f_i <- NewD_f[which(NewD_f$Aliens=="Invaded"),]
NewD_f_n <- NewD_f[which(NewD_f$Aliens=="No Aliens"),]

# extra test
NewD_f_i <- NewD_f_i[which(NewD_f_i$t1==2002),]

prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))

mod_h_rich <- MCMCglmm(D_nat_h_Cov ~ dT + t1_nat_g_Cov + t1_nat_e_Cov + t1_nat_b_Cov + t1_nat_t_Cov 
                     + t1_inv_h_Cov + t1_inv_g_Cov + t1_inv_t_Cov, 
                     random=~subpID, data=NewD_f_i, prior=prior, verbose=F, pr=T)
mod_g_rich <- MCMCglmm(D_nat_g_Cov ~ dT + t1_nat_h_Cov + t1_nat_e_Cov + t1_nat_b_Cov + t1_nat_t_Cov 
                       + t1_inv_h_Cov + t1_inv_g_Cov + t1_inv_t_Cov, 
                       random=~subpID, data=NewD_f_i, prior=prior, verbose=F, pr=T)
mod_e_rich <- MCMCglmm(D_nat_e_Cov ~ dT + t1_nat_g_Cov + t1_nat_h_Cov + t1_nat_b_Cov + t1_nat_t_Cov 
                       + t1_inv_h_Cov + t1_inv_g_Cov + t1_inv_t_Cov, 
                       random=~subpID, data=NewD_f_i, prior=prior, verbose=F, pr=T)
mod_b_rich <- MCMCglmm(D_nat_b_Cov ~ dT + t1_nat_g_Cov + t1_nat_e_Cov + t1_nat_h_Cov + t1_nat_t_Cov 
                       + t1_inv_h_Cov + t1_inv_g_Cov + t1_inv_t_Cov, 
                       random=~subpID, data=NewD_f_i, prior=prior, verbose=F, pr=T)
mod_t_rich <- MCMCglmm(D_nat_t_Cov ~ dT + t1_nat_g_Cov + t1_nat_e_Cov + t1_nat_b_Cov + t1_nat_h_Cov 
                       + t1_inv_h_Cov + t1_inv_g_Cov + t1_inv_t_Cov, 
                       random=~subpID, data=NewD_f_i, prior=prior, verbose=F, pr=T)
mod_h_inv <- MCMCglmm(D_inv_h_Cov ~ dT + t1_nat_g_Cov + t1_nat_e_Cov + t1_nat_b_Cov + t1_nat_t_Cov 
                       + t1_inv_g_Cov + t1_inv_t_Cov, random=~subpID, data=NewD_f_i, prior=prior, verbose=F, pr=T)
mod_g_inv <- MCMCglmm(D_inv_g_Cov ~ dT + t1_nat_g_Cov + t1_nat_e_Cov + t1_nat_b_Cov + t1_nat_t_Cov 
                      + t1_inv_h_Cov + t1_inv_t_Cov, random=~subpID, data=NewD_f_i, prior=prior, verbose=F, pr=T)
mod_t_inv <- MCMCglmm(D_inv_t_Cov ~ dT + t1_nat_g_Cov + t1_nat_e_Cov + t1_nat_b_Cov + t1_nat_t_Cov 
                      + t1_inv_h_Cov + t1_inv_g_Cov, random=~subpID, data=NewD_f_i, prior=prior, verbose=F, pr=T)

summary(mod_h_rich)
summary(mod_g_rich)
summary(mod_e_rich)
summary(mod_b_rich)
summary(mod_t_rich)
summary(mod_h_inv)
summary(mod_g_inv)
summary(mod_t_inv)

1-var(NewD_f_i$D_nat_h_Cov-predict(mod_h_rich)[,1])/var(NewD_f_i$D_nat_h_Cov) # 0.17
1-var(NewD_f_i$D_nat_g_Cov-predict(mod_g_rich)[,1])/var(NewD_f_i$D_nat_g_Cov) # 0.17
1-var(NewD_f_i$D_nat_e_Cov-predict(mod_e_rich)[,1])/var(NewD_f_i$D_nat_e_Cov) # 0.17
1-var(NewD_f_i$D_nat_b_Cov-predict(mod_b_rich)[,1])/var(NewD_f_i$D_nat_b_Cov) # 0.17
1-var(NewD_f_i$D_nat_t_Cov-predict(mod_t_rich)[,1])/var(NewD_f_i$D_nat_t_Cov) # 0.17
1-var(NewD_f_i$D_inv_h_Cov-predict(mod_h_inv)[,1])/var(NewD_f_i$D_inv_h_Cov) # 0.17
1-var(NewD_f_i$D_inv_g_Cov-predict(mod_g_inv)[,1])/var(NewD_f_i$D_inv_g_Cov) # 0.17
1-var(NewD_f_i$D_inv_t_Cov-predict(mod_t_inv)[,1])/var(NewD_f_i$D_inv_t_Cov) # 0.17


# all sites together
# ......................
mod_h_rich <- MCMCglmm(D_nat_h_Cov ~ dT + t1_nat_g_Cov + t1_nat_e_Cov + t1_nat_b_Cov + t1_nat_t_Cov 
                       + t1_inv_h_Cov + t1_inv_g_Cov + t1_inv_t_Cov, 
                       random=~subpID, data=NewD_f, prior=prior, verbose=F, pr=T)
mod_g_rich <- MCMCglmm(D_nat_g_Cov ~ dT + t1_nat_h_Cov + t1_nat_e_Cov + t1_nat_b_Cov + t1_nat_t_Cov 
                       + t1_inv_h_Cov + t1_inv_g_Cov + t1_inv_t_Cov, 
                       random=~subpID, data=NewD_f, prior=prior, verbose=F, pr=T)
mod_e_rich <- MCMCglmm(D_nat_e_Cov ~ dT + t1_nat_g_Cov + t1_nat_h_Cov + t1_nat_b_Cov + t1_nat_t_Cov 
                       + t1_inv_h_Cov + t1_inv_g_Cov + t1_inv_t_Cov, 
                       random=~subpID, data=NewD_f, prior=prior, verbose=F, pr=T)
mod_b_rich <- MCMCglmm(D_nat_b_Cov ~ dT + t1_nat_g_Cov + t1_nat_e_Cov + t1_nat_h_Cov + t1_nat_t_Cov 
                       + t1_inv_h_Cov + t1_inv_g_Cov + t1_inv_t_Cov, 
                       random=~subpID, data=NewD_f, prior=prior, verbose=F, pr=T)
mod_t_rich <- MCMCglmm(D_nat_t_Cov ~ dT + t1_nat_g_Cov + t1_nat_e_Cov + t1_nat_b_Cov + t1_nat_h_Cov 
                       + t1_inv_h_Cov + t1_inv_g_Cov + t1_inv_t_Cov, 
                       random=~subpID, data=NewD_f, prior=prior, verbose=F, pr=T)
mod_h_inv <- MCMCglmm(D_inv_h_Cov ~ dT + t1_nat_g_Cov + t1_nat_e_Cov + t1_nat_b_Cov + t1_nat_t_Cov 
                      + t1_inv_g_Cov + t1_inv_t_Cov, random=~subpID, data=NewD_f, prior=prior, verbose=F, pr=T)
mod_g_inv <- MCMCglmm(D_inv_g_Cov ~ dT + t1_nat_g_Cov + t1_nat_e_Cov + t1_nat_b_Cov + t1_nat_t_Cov 
                      + t1_inv_h_Cov + t1_inv_t_Cov, random=~subpID, data=NewD_f, prior=prior, verbose=F, pr=T)
mod_t_inv <- MCMCglmm(D_inv_t_Cov ~ dT + t1_nat_g_Cov + t1_nat_e_Cov + t1_nat_b_Cov + t1_nat_t_Cov 
                      + t1_inv_h_Cov + t1_inv_g_Cov, random=~subpID, data=NewD_f, prior=prior, verbose=F, pr=T)


summary(mod_h_rich)
summary(mod_g_rich)
summary(mod_e_rich)
summary(mod_b_rich)
summary(mod_t_rich)
summary(mod_h_inv)
summary(mod_g_inv)
summary(mod_t_inv)

1-var(NewD_f$D_nat_h_Cov-predict(mod_h_rich)[,1])/var(NewD_f$D_nat_h_Cov) # 0.17
1-var(NewD_f$D_nat_g_Cov-predict(mod_g_rich)[,1])/var(NewD_f$D_nat_g_Cov) # 0.17
1-var(NewD_f$D_nat_e_Cov-predict(mod_e_rich)[,1])/var(NewD_f$D_nat_e_Cov) # 0.17
1-var(NewD_f$D_nat_b_Cov-predict(mod_b_rich)[,1])/var(NewD_f$D_nat_b_Cov) # 0.17
1-var(NewD_f$D_nat_t_Cov-predict(mod_t_rich)[,1])/var(NewD_f$D_nat_t_Cov) # 0.17
1-var(NewD_f$D_inv_h_Cov-predict(mod_h_inv)[,1])/var(NewD_f$D_inv_h_Cov) # 0.17
1-var(NewD_f$D_inv_g_Cov-predict(mod_g_inv)[,1])/var(NewD_f$D_inv_g_Cov) # 0.17
1-var(NewD_f$D_inv_t_Cov-predict(mod_t_inv)[,1])/var(NewD_f$D_inv_t_Cov) # 0.17



# ==> add the spc richness and diversity?






