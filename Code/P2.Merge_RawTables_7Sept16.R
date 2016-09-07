# Laure Gallien	
# le 21 June 2016

library(gdata)



if (Sys.getenv("USER")=="jasper") {setwd("/Users/jasper/Dropbox/Shared/CapeCommunities/Data/Raw/")}
if (Sys.getenv("USER")=="diversitalp") {setwd("~/Desktop/Dropbox/GIT/2016_CapeCom/Data/LaurePrep/")}


###################################
###  Suffix codes
###################################
#t = trait
#m = mortality
#p = 10x10m plot
#sp = 1x1m subplot
#pl = plot data (no species names)
###################################


######################################################################
######################################################################
# Load the data
######################################################################
######################################################################

#------------------------------------------------
# Get plot treatment info
#------------------------------------------------
treat <- read.table("plot_treatments_4Aug16.txt", stringsAsFactors=F, header=T)
head(treat)
str(treat)
dim(treat) # 51 33


#------------------------------------------------
# Get TRAIT data from 2002 survey
#------------------------------------------------
t02 <- read.delim("Trait_2002_1Aug16.txt", stringsAsFactors=F)
head(t02)
dim(t02) # 823 11
str(t02)
summary(t02)


######################################################################
######################################################################
# Get the species x site data : 10 x 10m
######################################################################
######################################################################

######################################################################
# 2002 
######################################################################
p02_1 <- read.delim("prePrep/PlotSpc_2002_1_21June16.txt", stringsAsFactors=F)
p02_2 <- read.delim("prePrep/PlotSpc_2002_2_21June16.txt", stringsAsFactors=F)

p02 <- merge(p02_1, p02_2, by="spcID", all=T)
p02 <- p02[,c(1:2, 255, 3:254, 256:315)]
p02 <- p02[order(p02$spcID),]
head(p02)
str(p02)

# change weird quantities into numeric
unique(unlist(p02[,4:ncol(p02)]))  # "1" "0" NA  "."
for(i in 4:ncol(p02)){
  p02[,i] <- ifelse(is.na(p02[,i]), 0, p02[,i])
  if( is.character(p02[,i]) ) as.numeric(p02[,i] <- ifelse(p02[,i]==".", 0, p02[,i]))
  print(i) }
sum(as.numeric(unlist(p02[,4:ncol(p02)]))) # 6784

# organize it as the other matrices
row.names(p02) <- p02$spcID
p02s <- t(p02[,4:ncol(p02)])
row.names(p02s) <- substring(row.names(p02s), 2, 10)
head(p02s)
str(p02s)
dim(p02s) # 312 828

write.table(p02s, file="PlotSpc_2002_RawSpcID_7Sept16.txt", row.names=T, sep="\t", quote=)
p02 <- read.delim("PlotSpc_2002_RawSpcID_7Sept16.txt", stringsAsFactors=F)
p02[1:10, 1:10]


######################################################################
# 2008 
######################################################################
p08 <- read.delim("prePrep/PlotSpc_2008_21June16.txt", stringsAsFactors=F)
head(p08)
str(p08)
sum(unique(p08[,3:5])[,3])  # 2809

up <- sort(unique(p08$plot))
usp <- sort(unique(p08$species))
p08s <- matrix(0, nrow=length(up), ncol=length(usp), dimnames=list(up, usp))
for(i in up) { 
	for(j in usp) { 
		if(nrow(p08[which(p08$plot==i & p08$species==j),])>0) p08s[as.character(i), j] <- 1	} ;	print(i) }

p08s <- as.data.frame(p08s)
head(p08s)
str(p08s)
dim(p08s)  # 263 812
unique(unlist(p08s)) # 0, 1
sum(as.numeric(unlist(p08s))) # 2809

write.table(p08s, file="PlotSpc_2008_RawSpcID_7Sept16.txt", quote=F, sep="\t")
p08 <- read.delim("PlotSpc_2008_RawSpcID_7Sept16.txt", stringsAsFactors=F)
p08[1:10, 1:10]


######################################################################
# 2011 
######################################################################
p11 <- read.delim("prePrep/PlotSpc_2011_21June16.txt", stringsAsFactors=F)
head(p11)
str(p11)
p11$value <- 1
sum(unique(p11)[,"value"])  # 651

up <- sort(unique(p11$plot))
usp <- sort(unique(p11$species))
p11s <- matrix(0, nrow=length(up), ncol=length(usp), dimnames=list(up, usp))
for(i in up) { 
	for(j in usp) {
	  if(nrow(p11[which(p11$plot==i & p11$species==j),])>0) p11s[as.character(i), j] <- 1	} ;	print(i) }

p11s <- as.data.frame(p11s)
head(p11s)
str(p11s)
dim(p11s)  # 62 381
unique(unlist(p11s)) # 0, 1
sum(as.numeric(unlist(p11s))) # 651

write.table(p11s, file="PlotSpc_2011_RawSpcID_7Sept16.txt", quote=F, sep="\t")
p11 <- read.delim("PlotSpc_2011_RawSpcID_7Sept16.txt", stringsAsFactors=F)
p11[1:10, 1:10]


######################################################################
# 2014 
######################################################################
p14 <- read.delim("prePrep/PlotSpc_2014_21June16.txt", stringsAsFactors=F)
p14$spcID <- paste(p14$Genus, p14$Species, sep="_")
p14 <- p14[,c(1, 5)]
head(p14)
p14$value <- 1
sum(unique(p14)[,"value"])  # 1314

up <- sort(unique(p14$Plot))
usp <- sort(unique(p14$spcID))
p14s <- matrix(0, nrow=length(up), ncol=length(usp), dimnames=list(up, usp))
for(i in up) { 
	for(j in usp) { 
	  if(nrow(p14[which(p14$Plot==i & p14$spcID==j),])>0) p14s[as.character(i), j] <- 1	} ;	print(i) }

p14s <- as.data.frame(p14s)
head(p14s)
str(p14s)
dim(p14s)  # 141  476
unique(unlist(p14s)) # 0, 1
sum(as.numeric(unlist(p14s))) # 1314

write.table(p14s, file="PlotSpc_2014_RawSpcID_7Sept16.txt", quote=F, sep="\t")
p14 <- read.delim("PlotSpc_2014_RawSpcID_7Sept16.txt", stringsAsFactors=F)
p14[1:10, 1:10]


######################################################################
######################################################################
# Get the species x site data : 1 x 1m
######################################################################
######################################################################

######################################################################
# 2002 
######################################################################

#=====================================================================
# Load the data
#=====================================================================
sp02 <- read.xls("prePrep/sub-plots_21June16.xls", sheet=1, stringsAsFactors=F)
names(sp02) <- c("spcID", paste( rep(paste("p", rep(1:((ncol(sp02)-1)/(12*3)), each=12),
								"_", letters[1:12], sep=""), each=3), c("Reg", "Nb", "Cover"), sep="_") )
CC <- 7  # name of the last plot done
for(i in 2:8) {
	bis <- read.xls("prePrep/sub-plots_21June16.xls", sheet=i, stringsAsFactors=F)
	names(bis) <- c("spcID", paste( rep(paste("p", rep((CC+1):(CC+((ncol(bis)-1)/(12*3))), each=12),
								"_", letters[1:12], sep=""), each=3), c("Reg", "Nb", "Cover"), sep="_") )
	sp02 <- merge(sp02, bis, by="spcID", all=T)
	CC <- CC+((ncol(bis)-1)/(12*3)) ; print(i) ; cat(CC)
}
dim(sp02)  # 830 1873
sp02$spcID <- unlist(lapply(strsplit(sp02$spcID, " "), function(x) paste(x[1], x[2], sep="_")))

sp02

#=====================================================================
# Make 3 different tables for the 3 different types of data
#=====================================================================
sp02_Reg <- sp02[2:nrow(sp02), c(1, grep("Reg", names(sp02)))]
sp02_Abun <- sp02[2:nrow(sp02), c(1, grep("Nb", names(sp02)))]
sp02_Cov <- sp02[2:nrow(sp02), c(1, grep("Cov", names(sp02)))]

#=====================================================================
# clean these tables
#=====================================================================

# The abundance data
#++++++++++++++++++++++++++++++++++++
class(sp02_Abun)
sp02_Abun[1:5, 1:5]
dim(sp02_Abun) # 829 spc X  625 sites
str(sp02_Abun)

# warning: there are duplicated names here --> will be sorted later
unique(unlist(sp02_Abun[,2:ncol(sp02_Abun)]))
sp02_Abun[,2:ncol(sp02_Abun)] <- as.data.frame(apply(sp02_Abun[,2:ncol(sp02_Abun)], 1:2, function(x) {
                        if(nchar(x)==0)     x <- 0
                        if(is.na(x))  x <- 0
                        if(x==">25")  x <- 25
                        if(x==">15")  x <- 15
                        if(x=="s")    x <- 1
                        if(x=="*")    x <- 1
                        if(x=="0.5")  x <- 1
                        if(x=="?")    x <- 0
                        return(x) } ))
for(i in 2:ncol(sp02_Abun)) sp02_Abun[,i] <- as.numeric(as.character(sp02_Abun[,i])) 
sum(unlist(sp02_Abun[,2:ncol(sp02_Abun)]))  # 29632 indiv in total

names(sp02_Abun)[2:ncol(sp02_Abun)] <- strsplit(names(sp02_Abun)[2:ncol(sp02_Abun)], "_Nb")
head(sp02_Abun)

# remove the duplicated species
dupli <- sp02_Abun$spcID[duplicated(sp02_Abun$spcID)]
for(i in dupli) {
  temp <- colSums(sp02_Abun[which(sp02_Abun$spcID %in% i),2:ncol(sp02_Abun)])
  sp02_Abun[which(sp02_Abun$spcID %in% i),2:ncol(sp02_Abun)] <- rbind(temp, temp) 
}
dim(sp02_Abun) # 829  625
dim(unique(sp02_Abun))  # 824  625

sp02_Abun <- unique(sp02_Abun)
row.names(sp02_Abun) <- sp02_Abun$spcID
sp02_Abun_2 <- t(sp02_Abun[,2:ncol(sp02_Abun)])
sp02_Abun_2[1:5, 1:15]
dim(sp02_Abun_2) # 624 824
sum(unlist(sp02_Abun_2))  # 29632 indiv in total

# The proportion of cover data
#++++++++++++++++++++++++++++++++++++
class(sp02_Cov)
sp02_Cov[1:5, 1:5]
dim(sp02_Cov) # 829 spc X  625 sites
str(sp02_Cov)

unique(unlist(sp02_Cov[,2:ncol(sp02_Cov)]))
sp02_Cov[,2:ncol(sp02_Cov)] <- as.data.frame(apply(sp02_Cov[,2:ncol(sp02_Cov)], 1:2, function(x) {
  if(nchar(x)==0)     x <- 0
  if(is.na(x))  x <- 0
  if(x=="+")    x <- 1
  if(x==">25")  x <- 25
  if(x==">15")  x <- 15
  if(x=="s")    x <- 1
  if(x=="*")    x <- 1
  if(x=="0.5")  x <- 1
  if(x=="?")    x <- 0
  if(x=="70?")  x <- 70
  if(x=="0.05")  x <- 0.5
  if(x=="r")    x <- 1
  return(x) } ))

for(i in 2:ncol(sp02_Cov)) sp02_Cov[,i] <- as.numeric(as.character(sp02_Cov[,i])) 
sum(unlist(sp02_Cov[,2:ncol(sp02_Cov)]))  # 27302.5 cover prop in total

names(sp02_Cov)[2:ncol(sp02_Cov)] <- strsplit(names(sp02_Cov)[2:ncol(sp02_Cov)], "_Cover")
sp02_Cov[1:5, 1:5]

# remove the duplicated species
dupli <- sp02_Cov$spcID[duplicated(sp02_Cov$spcID)]
for(i in dupli) {
  temp <- colSums(sp02_Cov[which(sp02_Cov$spcID %in% i),2:ncol(sp02_Cov)])
  sp02_Cov[which(sp02_Cov$spcID %in% i),2:ncol(sp02_Cov)] <- rbind(temp, temp) 
}
dim(sp02_Cov) # 829  625
dim(unique(sp02_Cov))  # 824  625

sp02_Cov <- unique(sp02_Cov)
row.names(sp02_Cov) <- sp02_Cov$spcID
sp02_Cov_2 <- t(sp02_Cov[,2:ncol(sp02_Cov)])
sp02_Cov_2[1:5, 1:15]
dim(sp02_Cov_2) # 624 824
sum(unlist(sp02_Cov_2))  # 27302.5 cover prop in total

#=====================================================================
# Save it
#=====================================================================
write.table(sp02_Abun_2, file="subPlotSpc_2002_NbIndiv_RawSpcID_7Sept16.txt", quote=F, sep="\t", row.names=T)
write.table(sp02_Cov_2, file="subPlotSpc_2002_PropCover_RawSpcID_7Sept16.txt", quote=F, sep="\t", row.names=T)
sp02_Abun <- read.delim("subPlotSpc_2002_NbIndiv_RawSpcID_7Sept16.txt", stringsAsFactors=F)
sp02_Cov <- read.delim("subPlotSpc_2002_PropCover_RawSpcID_7Sept16.txt", stringsAsFactors=F)

sp02_Abun[1:10, 1:10] 
sp02_Cov[1:10, 1:10]



######################################################################
# 2008
######################################################################

#=====================================================================
# Load the data
#=====================================================================
sp08 <- read.delim("prePrep/subPlotSpc_2008_21June16.txt", stringsAsFactors=F)
head(sp08)
str(sp08)
dim(sp08) # 4513 11


sum(unique(sp08[,c(2:3, 9)])$prop_cov, na.rm=T) # 39323.8 [just used for further checking]
unique(sp08$no)
unique(sp08$prop_cov)

#=====================================================================
# Make 2 different tables for the 2 different types of data
#=====================================================================
sp08$no <- ifelse(nchar(sp08$no)==0, 0, sp08$no)
sp08$no <- ifelse(sp08$no=="a", 0, sp08$no)
sp08$prop_cov <- ifelse(is.na(sp08$prop_cov), 0, sp08$prop_cov)

up <- sort(unique(sp08$plot))
usp <- sort(unique(sp08$species))
p08N <- p08C <- matrix(0, nrow=length(up), ncol=length(usp), dimnames=list(up, usp))
for(i in up) { 
	for(j in usp) { 
		NN <- sp08[which(sp08$plot==i & sp08$species==j), "no"]
		CC <- sp08[which(sp08$plot==i & sp08$species==j), "prop_cov"]
		if(length(NN)>0) p08N[i, j] <- sum(as.numeric(as.character(NN)))
		if(length(CC)>0) p08C[i, j] <- sum(as.numeric(as.character(CC)))
  } ;	print(i) }

p08N[1:5, 1:5]
p08C[1:5, 1:5]
dim(p08N) # 527  595
dim(na.omit(p08N)) # 526  595
sort(rowSums(apply(p08N, 1, is.na))) # there is a NA for "Rhus_lucida"

#### TO FINISH TOMORROW!!!

dim(p08C) # 527  595

sum(p08N, na.rm=T) # 14489.1 
sum(p08C, na.rm=T) # 44728.7 


#=====================================================================
# Save it
#=====================================================================
write.table(p08N, file="subPlotSpc_2008_NbIndiv_RawSpcID_7Sept16.txt", quote=F, sep="\t")
write.table(p08C, file="subPlotSpc_2008_PropCover_RawSpcID_7Sept16.txt", quote=F, sep="\t")

sp08_Abun <- read.delim("subPlotSpc_2008_NbIndiv_RawSpcID_7Sept16.txt", stringsAsFactors=F)
sp08_Cov <- read.delim("subPlotSpc_2008_PropCover_RawSpcID_7Sept16.txt", stringsAsFactors=F)


######################################################################
# 2011
######################################################################
# sp11 <- read.delim("prePrep/subPlotSpc_2011_21June16.txt")
# head(sp11)

# up <- sort(as.character(unique(sp11$sub.plot)))
# usp <- sort(as.character(unique(sp11$species)))
# p11N <- p11C <- matrix(NA, nrow=length(up), ncol=length(usp), dimnames=list(up, usp))
# for(i in up) { 
	# for(j in usp) { 
		# NN <- sp11[which(sp11$sub.plot==i & sp11$species==j), "no"]
		# CC <- sp11[which(sp11$sub.plot==i & sp11$species==j), "prop_cov"]
		# p11N[i, j] <- ifelse(length(NN)>0, NN, 0)	
		# p11C[i, j] <- ifelse(length(CC)>0, CC, 0)	
		# } ;	print(i) }
# sort(rowSums(p11N))	; sort(colSums(p11N))		
# sort(rowSums(p11C))	; sort(colSums(p11C))		
# write.table(p11N, file="subPlotSpc_2011_NbIndiv_21June16.txt", quote=F, sep="\t")
# write.table(p11C, file="subPlotSpc_2011_PropCover_21June16.txt", quote=F, sep="\t")
sp11_Abun <- read.delim("subPlotSpc_2011_NbIndiv_21June16.txt")
sp11_Cov <- read.delim("subPlotSpc_2011_PropCover_21June16.txt")
sp11_Abun[1:10, 1:10]
sp11_Cov[1:10, 1:10]

######################################################################
# 2014
######################################################################
# sp14 <- read.delim("prePrep/subPlotSpc_2014_21June16.txt")
# sp14$spcID <- paste(sp14$Genus, sp14$species, sep="_")
# sp14$plot <- paste(sp14$Site.plot, sp14$Subplot, sep=".")
# head(sp14)

# up <- sort(as.character(unique(sp14$plot)))
# usp <- sort(as.character(unique(sp14$spcID)))
# p14N <- p14C <- matrix(NA, nrow=length(up), ncol=length(usp), dimnames=list(up, usp))
# for(i in up) { 
	# for(j in usp) { 
		# NN <- sp14[which(sp14$plot==i & sp14$spcID==j), "no"]
		# CC <- sp14[which(sp14$plot==i & sp14$spcID==j), "prop_cov"]
		# p14N[i, j] <- ifelse(length(NN)>0, NN, 0)	
		# p14C[i, j] <- ifelse(length(CC)>0, CC, 0)	
		# } ;	print(i) }
# sort(rowSums(p14N))	; sort(colSums(p14N))		
# sort(rowSums(p14C))	; sort(colSums(p14C))		
# write.table(p14N, file="subPlotSpc_2014_NbIndiv_21June16.txt", quote=F, sep="\t")
# write.table(p14C, file="subPlotSpc_2014_PropCover_21June16.txt", quote=F, sep="\t")
sp14_Abun <- read.delim("subPlotSpc_2014_NbIndiv_21June16.txt")
sp14_Cov <- read.delim("subPlotSpc_2014_PropCover_21June16.txt")
sp14_Abun[1:10, 1:10]
sp14_Cov[1:10, 1:10]































































