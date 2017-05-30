# Laure Gallien	
# le 9 May 2017


library(picante)
library(betapart)
library(fields)
library(corrplot)
library(abind)
library(reshape2)
library(plyr)
library(ade4)

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


# Trait data
#------------------------------------------------
tr <- read.delim("All_Trait_Quanti_26Jan17.txt", stringsAsFactors = F)
head(tr)
apply(tr, 2, table)





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
COL2 <- colorRampPalette(c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8',
          '#abd9e9','#74add1','#4575b4','#313695') ) (20)
nCol <- length(COL2)
col.s <- data.frame(col=COL2, min=seq(0, 1-(1/nCol), length.out=nCol), 
                    max=seq(0+(1/nCol), 1, length.out=nCol))

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

# pdf(file=paste(pathRes, "Beta_taxo_obs_11May17.pdf", sep=""), width = 20, height = 20)
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
# dev.off()  


# SES-beta taxo (bt) : Presence Absence
# :::::::::::::::::::::::::::::::::::::::::::::::::::::

# list of random matrices
NREP=100
l.bt <- lapply(1:NREP, function(x) randomizeMatrix(spcSMatPA, null.model="richness"))
# list of null bt
l.ses.bt <- lapply(l.bt, function(x) beta.pair(x)) 

# pdf(file=paste(pathRes, "Beta_taxo_SES_10May17.pdf", sep=""), width = 20, height = 20)
par(mfcol=c(2,3))
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
   corrplot(Ott, method="color", type="lower", cl.ratio=0.2, cl.align="l", tl.col=COL, mar = c(2,4,2,4),
           cl.lim=c(0, 1), col=rep(COL2, 2), main=names(obs.bt)[m], is.corr = F)
   
    # MAKE THE FIGURE WITH THE ARROWS
    AA <- as.data.frame(Ott)
    TRMT <- unique(env1$Aliens)
    XY <- data.frame(Nam=names(AA), Year=rep(YY, each=3), Tr=TRMT, X=1:3, Y=rep(1:3, each=3), intra=diag(Ott))
    XY$col.intra <- as.character(sapply(XY$intra, function(x) col.s[x > col.s$min & x <= col.s$max,1]))
    plot(0:4, 0:4, type="n", axes=F, ylab = "", xlab = "", xlim=c(0.8, 3.2), ylim=c(0.8, 3.4))
    mtext(YY, side=2, at=1:3, las=1) ; mtext(TRMT, side=3, at=1:3, las=1)
    points(Y~X, XY, cex=5, col=XY$col.intra, pch=19)
    # within year:
    for(i in YY){ ; temp <- XY[XY$Year==i,]
      for(j in row.names(temp)){ ; temp2 <- AA[j, grep(i, names(AA))] ; temp2 <- temp2[,names(temp2) != j]
        for(k in names(temp2)){ ; tx1 <- temp[j, "X"] ; tx2 <- temp[k, "X"]
          if(tx1==1 & tx2==3 | tx1==3 & tx2==1) { temp3 <- temp ; temp3$Y <- temp3$Y+0.2} else { temp3 <- temp }
          tcol <- as.character(col.s[which(temp2[1, k]>col.s$min & temp2[1, k]<= col.s$max), "col"])
          arrows(tx1, temp3$Y[1], tx2, temp3$Y[1], col=tcol, lwd=3, code = 3, angle = 15) } } }
    # within treatment:
    for(i in TRMT){ ; temp <- XY[XY$Tr==i,]
      for(j in row.names(temp)){ ; temp2 <- AA[j, grep(i, names(AA))] ; temp2 <- temp2[,names(temp2) != j]
        for(k in names(temp2)){ ; ty1 <- temp[j, "Y"] ; ty2 <- temp[k, "Y"]
          if(ty1==1 & ty2==3 | ty1==3 & ty2==1) { temp3 <- temp ; temp3$X <- temp3$X+0.2} else { temp3 <- temp }
        tcol <- as.character(col.s[which(temp2[1, k]>col.s$min & temp2[1, k]<= col.s$max), "col"])
        arrows(temp3$X[1], ty1, temp3$X[1], ty2, col=tcol, lwd=3, code = 3, angle = 15) } } }
}
dev.off()  



# ##########################################
# ##########################################
# ##########################################
# 3. CALCULATE TURNOVER : FUNCTIONAL 
# ##########################################
# ##########################################
# ##########################################
row.names(tr) <- tr$SpcID
TT <- c("Growth_Form", "Life_span_", "Dispersal", "Seed_size", "Regeneration", "plant_height", "time_to_first_flower")
tr2 <- na.omit(tr[,c("SpcID", TT)])
tr2$Seed_size <- ifelse(tr2$Seed_size =="es", "s", tr2$Seed_size)
apply(tr2[,TT], 2, table)

# transform the categorical traits into binary ones
#*******************************
ltemp <- list()
for(i in TT){ ltemp[[i]] <- eval(parse(text = paste("dcast(tr2[,c('SpcID', i)], SpcID ~", i, ", fill=0)", sep="") )) }
allt <- do.call(cbind, ltemp)
row.names(allt) <- allt[,1]
allT <- allt[, -grep("SpcID", names(allt))]
allT <- as.data.frame(ifelse(allT=="0", "0", "1"))
for(i in names(allT)) allT[,i] <- as.numeric(allT[,i])-1
names(allT)
head(allT)

spcSMatPA2 <- data.frame(spcSMatPA[, which(colnames(spcSMatPA) %in% row.names(allT))])
allT2 <- allT[row.names(allT) %in% colnames(spcSMatPA), ]
spcSMatPA2 <- spcSMatPA2[, row.names(allT2)]
	
	
# test the traits
#*******************************
pcaE <- dudi.pca(allT2, scannf=F, nf=3)
TraitMat1 <- as.matrix(pcaE$li) 


# Observed beta function (fb)
obs.fb <- functional.beta.pair(spcSMatPA2[rowSums(spcSMatPA2)>3,], TraitMat1[,1:3])
# save(obs.fb, file="~/Dropbox/GIT/2016_CapeCom/Data/Results/Obs_beta_functio_10May17")

# list of randim matrices
l.fb <- lapply(1:NREP, function(x) spcSMatPA2[,sample(colnames(spcSMatPA2))])
l.fb2 <- lapply(l.fb, function(x) x[, row.names(TraitMat1)])
l.ses.bf <- lapply(l.fb2, function(x) functional.beta.pair(x[rowSums(x)>3,], TraitMat1[,1:3])) 
save(l.ses.bf, file="~/Dropbox/GIT/2016_CapeCom/Data/Results/rand_beta_functio_10May17")


####### TO BE CONTINUED!!!!!!!!!!!!!!!!!





l.beta <- fun_BetaFun(OBS=obs.fb, TRAIT=TraitMat1[,1:2], LIST.RAND=l.fb)

if(i==1) { save(l.beta, file="Spiders_listSES_AllTrait_Beta_8Dec15") ; obs.fbALL <- obs.fb }
if(i==2) { save(l.beta, file="Spiders_listSES_RespAbiot_Beta_8Dec15") ; obs.fbRA <- obs.fb }
if(i==3) { save(l.beta, file="Spiders_listSES_RespDist_Beta_8Dec15") ; obs.fbRD <- obs.fb }
if(i==4) { save(l.beta, file="Spiders_listSES_EffectResp_Beta_8Dec15") ; obs.fbEF <- obs.fb }


test.pair<-functional.beta.pair(x=comm.test, traits=traits.test, index.family = "jaccard" )

# Observed beta taxo (bt) : Presence Absence
# :::::::::::::::::::::::::::::::::::::::::::::::::::::
obs.bf <- functional.beta.pair(spcSMatPA, , "sorensen") 
names(obs.bf)

# pdf(file=paste(pathRes, "Beta_taxo_obs_10May17.pdf", sep=""), width = 20, height = 20)
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
# dev.off()  


# SES-beta taxo (bt) : Presence Absence
# :::::::::::::::::::::::::::::::::::::::::::::::::::::

# list of random matrices
NREP=100
l.bt <- lapply(1:NREP, function(x) randomizeMatrix(spcSMatPA, null.model="richness"))
# list of null bt
l.ses.bt <- lapply(l.bt, function(x) beta.pair(x)) 

# pdf(file=paste(pathRes, "Beta_taxo_SES_10May17.pdf", sep=""), width = 20, height = 20)
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
   corrplot(Ott, method="color", type="lower", cl.ratio=0.2, cl.align="l", tl.col=COL, mar = c(2,4,2,4),
           cl.lim=c(0, 1), col=rep(COL2, 2), main=names(obs.bt)[m], is.corr = F)
}
# dev.off()  






































