# Laure Gallien	
# le 9 May 2017


# library(ggplot2)
# library(diveRsity)
# library(cluster)
library(picante)
# library(MCMCglmm)
# library(lme4)
# library(vegan)
library(betapart)
library(fields)
library(corrplot)
library(abind)

if (Sys.getenv("USER")=="jasper") {setwd("/Users/jasper/Dropbox/Shared/CapeCommunities/Data/Raw/")}
if (Sys.getenv("USER")=="Laure") {setwd("~/Dropbox/GIT/2016_CapeCom/Data/LaurePrep/")}

pathRes <- "/Users/Laure/Dropbox/GIT/2016_CapeCom/Results/"

# AIM
# 1. SELECT ONLY THE SANDSTONE PLOTS (TMS)
# 2. GROUP ALL SUB-PLOT (PER PLOT) TOGETHER
# 3. COMPARE 2002 VS 2008 VS 2014
# 4. COMPARE ALIEN VS CLEAR VS NON-INVADED
# 5. CALCULATE TURNOVER : RICHNESS + FUNCTIONAL + PHYLOGENETIC (SORENSEN? OR NESTEDNESS/TURNOVER)
# 6. LOOK AT QUANTITATIVE TRAITS (WITH PCA AXIS) AND QUALITATIVE TRAITS (WITH MCA AXIS)


# ##########################################
# Load the data (sub-plot scale)
# ##########################################

# Get the species x site data : 1 x 1m
# ------------------------------------------------
sp02_Abun <- read.delim("subPlotSpc_2002_NbIndiv_2Fev17.txt", stringsAsFactors = F)
sp08_Abun <- read.delim("subPlotSpc_2008_NbIndiv_2Fev17.txt", stringsAsFactors = F)
sp14_Abun <- read.delim("subPlotSpc_2014_NbIndiv_2Fev17.txt", stringsAsFactors = F)

sp02_Cov <- read.delim("subPlotSpc_2002_PropCover_2Fev17.txt", stringsAsFactors = F)
sp08_Cov <- read.delim("subPlotSpc_2008_PropCover_2Fev17.txt", stringsAsFactors = F)
sp14_Cov <- read.delim("subPlotSpc_2014_PropCover_2Fev17.txt", stringsAsFactors = F)


# Get species names
# ------------------------------------------------
lspc_sp <- sort(unique(c(names(sp02_Abun), names(sp08_Abun), names(sp14_Abun))))
length(lspc_sp)  # 887


# Get env variables
# ------------------------------------------------
env <- read.table("plot_treatments_4Aug16.txt", stringsAsFactors = F, header=T)
head(env)


# ##########################################
# 1. SELECT ONLY THE SANDSTONE PLOTS (TMS)
# ##########################################
unique(env$Geology)  # "TMS" "Granite" "Sand"
env1 <- env[which(env$Geology == "TMS" & env$Burned_2000 == 1 & env$Burned_2005==0 & env$Burned_2008==0), ]

dim(env1) # 20
env1$Aliens <- ifelse(env1$Aliens=="No Aliens", "No_Aliens", env1$Aliens)
table(env1$Aliens) # cleared (2) ; Invaded (5) ; No_Aliens (13)

SitID <- env1$Site


# ##########################################
# 2. GROUP ALL SUB-PLOT (PER PLOT) TOGETHER
# ##########################################
sp02_Cov[1:10, 1:10]

ltemp <- list()
lNam <- c("sp02_Cov", "sp08_Cov", "sp14_Cov")
YY <- c(2002, 2008, 2014) ; names(YY) <- lNam
for(i in lNam){
  tt <- eval(parse(text=i))
  tt$sitID <- as.numeric(sapply(strsplit(row.names(tt), "_"), function(x) x[[1]]))
  CC <- do.call(rbind, lapply(unique(tt$sitID), function(x) colMeans(tt[tt$sitID==x, 1:(ncol(tt)-1)])) ) 
  ltemp[[as.character(YY[i])]] <- data.frame(cbind(year=YY[i], sitID=unique(tt$sitID), round(CC, 2))) }

lapply(ltemp, dim)


# Make one big file of these 3 years:
lsits <- sort(unique(c(ltemp[[1]]$sitID, ltemp[[2]]$sitID, ltemp[[3]]$sitID)))
DD <- data.frame(sitID=NA, year=NA, matrix(0, length(lsits)*3, length(lspc_sp), dimnames = list(1:(length(lsits)*3), lspc_sp)))
DD$sitID <- rep(lsits, 3) ; DD$year <- rep(YY, each=length(lsits))
DD[1:10, 1:10]

for(i in 1:nrow(DD)){
  temp <- ltemp[[as.character(DD[i, "year"])]]
  Tsit <- DD[i, "sitID"]
  if(Tsit %in% temp$sitID) {
    for(j in names(temp)[3:ncol(temp)]){
      DD[i, j] <- temp[which(temp$sitID==Tsit), j] 
    }  ; print(i)
  }
}

DD[1:3, 1:10]
dim(DD) # 156 sites ;  889 (887 species)



# ##########################################
# ##########################################
# ##########################################
# 3. CALCULATE TURNOVER : RICHNESS
# ##########################################
# ##########################################
# ##########################################
COL <- c("#7570b3", "#d95f02", "#1b9e77")
COL2 <- c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8',
          '#abd9e9','#74add1','#4575b4','#313695')  

row.names(DD) <- paste(DD$year, DD$sitID, sep="_")
spcSMat <- DD[, 3:ncol(DD)]
spcSMatPA <- ifelse(spcSMat>0,1,0)

# select the sites of the different categories
# :::::::::::::::::::::::::::::::::::::::::::::::::::::
env2 <- env1[, c(1:4, 7)]
sInv <- env2[which(env2$Aliens=="Invaded"), "Site"]
sNInv <- env2[which(env2$Aliens=="No_Aliens"), "Site"]
sCl <- env2[which(env2$Aliens=="Cleared"), "Site"]

# Observed beta taxo (bt) : Presence Absence
# :::::::::::::::::::::::::::::::::::::::::::::::::::::
obs.bt <- beta.pair(spcSMatPA) ; names(obs.bt)

pdf(file=paste(pathRes, "Beta_taxo_obs_10May17.pdf", sep=""), width = 20, height = 20)
par(mfrow=c(2,2))
for(k in names(obs.bt)){
  o1 <- as.matrix(obs.bt[[k]])
  # Calculate the mean per treatment
  TRY <- paste(rep(YY, each=3), rep(unique(env1$Aliens), 3), sep="_")
  Ott <- matrix(NA, ncol=9, nrow=9, dimnames=list(TRY, TRY))
  for(i in TRY){
    for(j in TRY){
      i1 <- substr(i, 1, 4) ; i2 <- substr(i, 6, 15) ; i3 <- env2[which(env2$Aliens==i2), "Site"]
      j1 <- substr(j, 1, 4) ;  j2 <- substr(j, 6, 15) ; j3 <- env2[which(env2$Aliens==j2), "Site"]
      i4 <- paste(i1, i3, sep="_") ; j4 <- paste(j1, j3, sep="_")
      if(i==j) { otemp <- o1[i4, i4] ; diag(otemp) <- NA ; Ott[i,i] <- mean(otemp, na.rm = T) }
      if(i!=j & i2==j2) { Ott[i,j] <- mean(diag(o1[i4, j4]), na.rm = T) }
      if(i!=j & i2!=j2) { Ott[i,j] <- mean(colMeans(o1[i4, j4], na.rm = T), na.rm = T) }
  }	  }	
  # plot it
  corrplot(Ott, type="lower", cl.ratio=0.2, cl.align="l", tl.col=COL, 
           cl.lim=c(0,1), col=rep(COL2,2), main=k, mar = c(2,4,2,4))
}  
dev.off()  


# SES-beta taxo (bt) : Presence Absence
# :::::::::::::::::::::::::::::::::::::::::::::::::::::

# list of randim matrices
NREP=100
l.bt <- lapply(1:NREP, function(x) randomizeMatrix(spcSMatPA, null.model="richness"))
# list of null bt
l.ses.bt <- lapply(l.bt, function(x) beta.pair(x)) 

pdf(file=paste(pathRes, "Beta_taxo_SES_10May17.pdf", sep=""), width = 20, height = 20)
par(mfrow=c(2,2))
for(m in 1:3){
  l.rand_t1 <- lapply(l.ses.bt, function(x) as.matrix( x[[m]] ))
  l.rand_t1_a <- do.call(abind, c(l.rand_t1, list(along = 3)) ) # make a 3d array
  o2 <- o1 <- as.matrix(obs.bt[[m]])
  for(rr in 1:nrow(o1)){
    for(cc in 1:ncol(o1)){
      o2[rr, cc] <- rank(c(o1[rr, cc], l.rand_t1_a[rr, cc,]))[1]/(NREP+1)  }  }

  # Calculate the mean per treatment
  TRY <- paste(rep(YY, each=3), rep(unique(env1$Aliens), 3), sep="_")
  Ott <- matrix(NA, ncol=9, nrow=9, dimnames=list(TRY, TRY))
  for(i in TRY){
    for(j in TRY){
      i1 <- substr(i, 1, 4) ; i2 <- substr(i, 6, 15) ; i3 <- env2[which(env2$Aliens==i2), "Site"]
      j1 <- substr(j, 1, 4) ;  j2 <- substr(j, 6, 15) ; j3 <- env2[which(env2$Aliens==j2), "Site"]
      i4 <- paste(i1, i3, sep="_") ; j4 <- paste(j1, j3, sep="_")
      if(i==j) { otemp <- o2[i4, i4] ; diag(otemp) <- NA ; Ott[i,i] <- mean(otemp, na.rm = T) }
      if(i!=j & i2==j2) { Ott[i,j] <- mean(diag(o2[i4, j4]), na.rm = T) }
      if(i!=j & i2!=j2) { Ott[i,j] <- mean(colMeans(o2[i4, j4], na.rm = T), na.rm = T) }
    }	  }	
  corrplot(Ott, type="lower", cl.ratio=0.2, cl.align="l", tl.col=COL, mar = c(2,4,2,4),
           cl.lim=c(0, 1), col=rep(COL2, 2), main=names(obs.bt)[m], is.corr = F)
}
dev.off()  



# Calculate the mean per treatment
Otot <- Ont <- Ott <- DtTOT <- DtNT <- DtTT <- matrix(NA, ncol=4, nrow=4, dimnames=list(c("O", "S", "M", "R"),c("O", "S", "M", "R")))
bsim1 <- ses.bt.TT ; bsim2 <- ses.bt.NT ; bsim3 <- ses.bt.TOT
o1 <- as.matrix(obs.bt[[1]]) ; o2 <- as.matrix(obs.bt[[2]]) ; o3 <- as.matrix(obs.bt[[3]])
for(i in c("O", "S", "M", "R")){
  for(j in c("O", "S", "M", "R")){
    Ott[i,j] <- mean(o1[grep(i, row.names(o1)), grep(j, row.names(o1))], na.rm=T)
    Ont[i,j] <- mean(o2[grep(i, row.names(o2)), grep(j, row.names(o2))], na.rm=T)
    Otot[i,j] <- mean(o3[grep(i, row.names(o3)), grep(j, row.names(o3))], na.rm=T)
    DtTT[i,j] <- mean(bsim1[grep(i, row.names(bsim1)), grep(j, row.names(bsim1))], na.rm=T)
    DtNT[i,j] <- mean(bsim2[grep(i, row.names(bsim2)), grep(j, row.names(bsim2))], na.rm=T)
    DtTOT[i,j] <- mean(bsim3[grep(i, row.names(bsim3)), grep(j, row.names(bsim3))], na.rm=T)
  }	
}	

# plot it en moyenne
par(mfrow=c(2,3))
legnd="text(x=c(0.1, 0.4, 0.6, 0.9), y=c(0.1, 0.4, 0.6, 0.9), labels=c('O', 'S', 'M', 'R'))" ; COL=rev(grey(1:20/20)) ; COL2=colorRampPalette(c("#2B547E", "#2B60DE", "#3BB9FF", "#95B9C7", "#FFF8C6", "#FFDB58", "#FFA62F", "#FF0000", "#C11B17"))(100)  
ZMAX=max(abs(c(DtTT, DtNT, DtTOT)))
image.plot(Otot, col=COL, main="All turnover") ;eval(parse(text=legnd))
image.plot(Ott/Otot*100, col=COL, main="trueTurn prop") ;eval(parse(text=legnd))
image.plot(Ont/Otot*100, col=COL, main="nestedness prop") ;eval(parse(text=legnd))
image.plot(DtTOT, col=COL2, main="All turnover SES", zlim=c(-ZMAX, ZMAX));eval(parse(text=legnd))
image.plot(DtTT, col=COL2, main="True turnover SES", zlim=c(-ZMAX, ZMAX));eval(parse(text=legnd))
image.plot(DtNT, col=COL2, main="Nestedness SES", zlim=c(-ZMAX, ZMAX)) ;eval(parse(text=legnd))


#..............................................
# Test beta taxo diversity Carabids
#..............................................
spcSMat <- cc[,2:21]
spcSMatPA <- ifelse(spcSMat>0,1,0)

# Observed beta taxo (bt)
obs.bt <- beta.pair(t(spcSMatPA))
# list of randim matrices
l.bt <- lapply(1:100, function(x) randomizeMatrix(t(spcSMatPA), null.model="richness"))
# list of null bt
l.ses.bt <- lapply(l.bt, function(x) beta.pair(x)) 

# true turn over
l.ses.bt_TT <- lapply(l.ses.bt, function(x) as.matrix(x[[1]]))
ses.bt.a <- do.call(abind,c(l.ses.bt_TT,list(along=3)))
ses.bt.TT_mean <- apply(ses.bt.a, c(1,2), function(x) mean(x))
ses.bt.TT_sd <- apply(ses.bt.a, c(1,2), function(x) sd(x))
ses.bt.TT <- (as.matrix(obs.bt[[1]])-ses.bt.TT_sd)/ses.bt.TT_mean

# nestedness
l.ses.bt_NT <- lapply(l.ses.bt, function(x) as.matrix(x[[2]]))
ses.bt.nt.a <- do.call(abind,c(l.ses.bt_NT, list(along=3)))
ses.bt.NT_mean <- apply(ses.bt.nt.a, c(1,2), function(x) mean(x))
ses.bt.NT_sd <- apply(ses.bt.nt.a, c(1,2), function(x) sd(x))
ses.bt.NT <- (as.matrix(obs.bt[[2]])-ses.bt.NT_sd)/ses.bt.NT_mean

# total turnover
l.ses.bt_TOT <- lapply(l.ses.bt, function(x) as.matrix(x[[3]]))
ses.bt.tot.a <- do.call(abind,c(l.ses.bt_TOT, list(along=3)))
ses.bt.TOT_mean <- apply(ses.bt.tot.a, c(1,2), function(x) mean(x))
ses.bt.TOT_sd <- apply(ses.bt.tot.a, c(1,2), function(x) sd(x))
ses.bt.TOT <- (as.matrix(obs.bt[[2]])-ses.bt.TOT_sd)/ses.bt.TOT_mean


# Calculate the mean per treatment
Otot <- Ont <- Ott <- DtTOT <- DtNT <- DtTT <- matrix(NA, ncol=4, nrow=4, dimnames=list(c("O", "S", "M", "R"),c("O", "S", "M", "R")))
bsim1 <- ses.bt.TT ; bsim2 <- ses.bt.NT ; bsim3 <- ses.bt.TOT
o1 <- as.matrix(obs.bt[[1]]) ; o2 <- as.matrix(obs.bt[[2]]) ; o3 <- as.matrix(obs.bt[[3]])
for(i in c("O", "S", "M", "R")){
  for(j in c("O", "S", "M", "R")){
    Ott[i,j] <- mean(o1[grep(i, row.names(o1)), grep(j, row.names(o1))], na.rm=T)
    Ont[i,j] <- mean(o2[grep(i, row.names(o2)), grep(j, row.names(o2))], na.rm=T)
    Otot[i,j] <- mean(o3[grep(i, row.names(o3)), grep(j, row.names(o3))], na.rm=T)
    DtTT[i,j] <- mean(bsim1[grep(i, row.names(bsim1)), grep(j, row.names(bsim1))], na.rm=T)
    DtNT[i,j] <- mean(bsim2[grep(i, row.names(bsim2)), grep(j, row.names(bsim2))], na.rm=T)
    DtTOT[i,j] <- mean(bsim3[grep(i, row.names(bsim3)), grep(j, row.names(bsim3))], na.rm=T)
  }	
}	

# plot it en moyenne
par(mfrow=c(2,3))
legnd="text(x=c(0.1, 0.4, 0.6, 0.9), y=c(0.1, 0.4, 0.6, 0.9), labels=c('O', 'S', 'M', 'R'))" ; COL=rev(grey(1:20/20)) ; COL2=colorRampPalette(c("#2B547E", "#2B60DE", "#3BB9FF", "#95B9C7", "#FFF8C6", "#FFDB58", "#FFA62F", "#FF0000", "#C11B17"))(100)  ; ZMAX=max(abs(c(DtTT, DtNT, DtTOT)))
image.plot(Otot, col=COL, main="All turnover") ;eval(parse(text=legnd))
image.plot(Ott/Otot*100, col=COL, main="trueTurn prop") ;eval(parse(text=legnd))
image.plot(Ont/Otot*100, col=COL, main="nestedness prop") ;eval(parse(text=legnd))
image.plot(DtTOT, col=COL2, main="All turnover SES", zlim=c(-ZMAX, ZMAX));eval(parse(text=legnd))
image.plot(DtTT, col=COL2, main="True turnover SES", zlim=c(-ZMAX, ZMAX));eval(parse(text=legnd))
image.plot(DtNT, col=COL2, main="Nestedness SES", zlim=c(-ZMAX, ZMAX)) ;eval(parse(text=legnd))









































