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


#------------------------------------------------
# Get the species x site data : 10 x 10m
#------------------------------------------------

# 2002 
#........
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

write.table(p02, file="PlotSpc_2002_RawSpcID_7Sept16.txt", row.names=F, sep="\t", quote=)
p02 <- read.delim("PlotSpc_2002_RawSpcID_7Sept16.txt", stringsAsFactors=F)
p02[1:10, 1:10]


# 2008 
#........
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


# 2011 
#........
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


# 2014 
#........
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


#------------------------------------------------
# Get the species x site data : 1 x 1m
#------------------------------------------------


# 2002 
#........
# sp02 <- read.xls("prePrep/sub-plots_21June16.xls", sheet=1, stringsAsFactors=FALSE) 
# names(sp02) <- c("spcID", paste( rep(paste("p", rep(1:((ncol(sp02)-1)/(12*3)), each=12), 
								# "_", letters[1:12], sep=""), each=3), c("Reg", "Nb", "Cover"), sep="_") )
# CC <- 7  # name of the last plot done
# for(i in 2:8) {
	# bis <- read.xls("prePrep/sub-plots_21June16.xls", sheet=i, stringsAsFactors=FALSE) 
	# names(bis) <- c("spcID", paste( rep(paste("p", rep((CC+1):(CC+((ncol(bis)-1)/(12*3))), each=12), 
								# "_", letters[1:12], sep=""), each=3), c("Reg", "Nb", "Cover"), sep="_") )
	# sp02 <- merge(sp02, bis, by="spcID", all=T)
	# CC <- CC+((ncol(bis)-1)/(12*3)) ; print(i) ; cat(CC)
# }
# dim(sp02)  # 830 1873
# sp02$spcID <- unlist(lapply(strsplit(sp02$spcID, " "), function(x) paste(x[1], x[2], sep="_")))

# # Make 3 different tables for the 3 different types of data
# sp02_Reg <- sp02[2:nrow(sp02), c(1, grep("Reg", names(sp02)))]
# sp02_Abun <- sp02[2:nrow(sp02), c(1, grep("Nb", names(sp02)))]
# sp02_Cov <- sp02[2:nrow(sp02), c(1, grep("Cov", names(sp02)))]

# # clean these tables
# sp02_Reg <- as.data.frame(apply(sp02_Reg, 1:2, function(x) ifelse(x=="", NA, x)))
# names(sp02_Reg)[2:ncol(sp02_Reg)] <- strsplit(names(sp02_Reg)[2:ncol(sp02_Reg)], "_Reg")
# head(sp02_Reg)

# sp02_Abun <- as.data.frame(apply(sp02_Abun, 1:2, function(x) ifelse(x=="", 0, x)))
# names(sp02_Abun)[2:ncol(sp02_Abun)] <- strsplit(names(sp02_Abun)[2:ncol(sp02_Abun)], "_Nb")
# head(sp02_Abun)

# sp02_Cov <- as.data.frame(apply(sp02_Cov, 1:2, function(x) ifelse(x=="", 0, x)))
# names(sp02_Cov)[2:ncol(sp02_Cov)] <- strsplit(names(sp02_Cov)[2:ncol(sp02_Cov)], "_Cover")
# head(sp02_Cov)

# # Save it
# write.table(sp02_Reg, file="subPlotSpc_2002_Regeneration_21June16.txt", quote=F, sep="\t", row.names=F)
# write.table(sp02_Abun, file="subPlotSpc_2002_NbIndiv_21June16.txt", quote=F, sep="\t", row.names=F)
# write.table(sp02_Cov, file="subPlotSpc_2002_PropCover_21June16.txt", quote=F, sep="\t", row.names=F)
sp02_Reg <- read.delim("subPlotSpc_2002_Regeneration_21June16.txt")
sp02_Abun <- read.delim("subPlotSpc_2002_NbIndiv_21June16.txt")
sp02_Cov <- read.delim("subPlotSpc_2002_PropCover_21June16.txt")

sp02_Reg[1:10, 1:10] ; sp02_Abun[1:10, 1:10] ; sp02_Cov[1:10, 1:10]



# 2008
#........
# sp08 <- read.delim("prePrep/subPlotSpc_2008_21June16.txt")
# head(sp08)

# up <- sort(as.character(unique(sp08$plot)))
# usp <- sort(as.character(unique(sp08$species)))
# p08N <- p08C <- matrix(NA, nrow=length(up), ncol=length(usp), dimnames=list(up, usp))
# for(i in up) { 
	# for(j in usp) { 
		# NN <- sp08[which(sp08$plot==i & sp08$species==j), "no"]
		# CC <- sp08[which(sp08$plot==i & sp08$species==j), "prop_cov"]
		# p08N[i, j] <- ifelse(length(NN)>0, NN, 0)	
		# p08C[i, j] <- ifelse(length(CC)>0, CC, 0)	
		# } ;	print(i) }
# sort(rowSums(p08N))	; sort(colSums(p08N))		
# sort(rowSums(p08C))	; sort(colSums(p08C))		
# write.table(p08N, file="subPlotSpc_2008_NbIndiv_21June16.txt", quote=F, sep="\t")
# write.table(p08C, file="subPlotSpc_2008_PropCover_21June16.txt", quote=F, sep="\t")
sp08_Abun <- read.delim("subPlotSpc_2008_NbIndiv_21June16.txt")
sp08_Cov <- read.delim("subPlotSpc_2008_PropCover_21June16.txt")


# 2011
#........
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

# 2014
#........
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































































