# Laure Gallien	
# le 12 July 2016

library(gdata)



if (Sys.getenv("USER")=="jasper") {setwd("/Users/jasper/Dropbox/Shared/CapeCommunities/Data/Raw/")}
if (Sys.getenv("USER")=="diversitalp") {setwd("~/Desktop/Dropbox/GIT/2016_CapeCom/Data/LaurePrep/")}


######################################################################
######################################################################
# Load the data
######################################################################
######################################################################

#------------------------------------------------
# Get plot treatment info
#------------------------------------------------
treat <- read.delim("plot_treatments_21June16.txt")
head(treat)
dim(treat) # 52 5


#------------------------------------------------
# Get TRAIT data from 2002 survey
#------------------------------------------------
t02 <- read.delim("Trait_2002_21June16.txt")
head(t02)
dim(t02) # 823 15
str(t02)
summary(t02)


#------------------------------------------------
# Get the species x site data : 10 x 10m
#------------------------------------------------
# 2002 / 2008 / 2011 / 2014
#........
p02 <- read.delim("old/PlotSpc_2002_21June16.txt")
p08 <- read.delim("old/PlotSpc_2008_21June16.txt")
p11 <- read.delim("old/PlotSpc_2011_21June16.txt")
p14 <- read.delim("old/PlotSpc_2014_21June16.txt")

p02[1:10, 1:10]
p08[1:10, 1:10]
p11[1:10, 1:10]
p14[1:10, 1:10]


#------------------------------------------------
# Get the species x site data : 1 x 1m
#------------------------------------------------
# 2002 / 2008 / 2011 / 2014
#........
sp02_Abun <- read.delim("old/subPlotSpc_2002_NbIndiv_21June16.txt")
sp02_Cov <- read.delim("old/subPlotSpc_2002_PropCover_21June16.txt")

sp08_Abun <- read.delim("old/subPlotSpc_2008_NbIndiv_21June16.txt")
sp08_Cov <- read.delim("old/subPlotSpc_2008_PropCover_21June16.txt")

sp11_Abun <- read.delim("old/subPlotSpc_2011_NbIndiv_21June16.txt")
sp11_Cov <- read.delim("old/subPlotSpc_2011_PropCover_21June16.txt")

sp14_Abun <- read.delim("old/subPlotSpc_2014_NbIndiv_21June16.txt")
sp14_Cov <- read.delim("old/subPlotSpc_2014_PropCover_21June16.txt")

sp02_Abun[1:10, 1:10] ; sp02_Cov[1:10, 1:10]
sp08_Abun[1:10, 1:10] ; sp08_Cov[1:10, 1:10]
sp11_Abun[1:10, 1:10] ; sp11_Cov[1:10, 1:10]
sp14_Abun[1:10, 1:10] ; sp14_Cov[1:10, 1:10]


#------------------------------------------------
# Get the species list with correspondance Old vs New names
#------------------------------------------------
# synJ <- read.csv("~/Desktop/Dropbox/Travail_SA/3.ComEcol/CapeCommunities/Data/Raw/allnames_old.csv")
# synJ2 <- synJ[,c("Best_source", as.character(unique(synJ$Best_source)))]
# for(i in names(synJ2)) synJ2[,i] <- as.character(synJ2[,i])
# synJ2$Final <- sapply(1:nrow(synJ2), function(x) synJ2[x, synJ2[x, "Best_source"]])
# head(synJ2)
# dim(synJ2) # 2124
# write.table(synJ2[,c("Original", "Final")], file="Synonym_List_29July16.txt", sep="\t", quote=F, row.names=F)

syn <- read.delim("Synonym_List_29July16.txt")
dim(syn) # 2124
dim(unique(syn)) # 2124

head(syn, 200)


names(syn) <- c("ini", "fin")
for(i in 1:2) syn[,i] <- as.character(syn[,i])
syn <- unique(syn)
row.names(syn) <- syn$ini
head(syn)


######################################################################
######################################################################
# Check and change spc names on the PLOT data
######################################################################
######################################################################

#********************************************************************
# 2002
#********************************************************************
p02[1:10, 1:10]

old <- as.character(p02$spcID)
length(old) # 828
length(unique(old)) # 828

# Check that all species are in the list
length(old[old %in% syn$ini]) # 790
out <- old[!old %in% syn$ini]
out2 <- substring(out, 1, nchar(out)-1)
out2
out2[out2 %in% syn$ini]

# adapt them to the correspondance list
old_corr <- old
old_corr[!old_corr %in% syn$ini] <- out2
length(old_corr[old_corr %in% syn$ini]) # 828

# replace the species names by the correct ones
new <- syn[old_corr,"fin"]
length(new)  # 828
COR <- cbind(oldID=old, oldCORR=old_corr, newID=new)
p02n <- merge(COR, p02, by.x="oldID", by.y="spcID")
dim(p02n) # 828  317

# make the final file format
p02n <- p02n[,c(3, 6:ncol(p02n))]
p02n[1:10, 1:10]

# resume the duplicates
dupli <- unique(as.character(p02n$newID[duplicated(p02n$newID)]))
for(i in dupli) { 
	t.row <- row.names(p02n[which(p02n==i),])
	remp <- as.numeric(apply(p02n[t.row, 2:ncol(p02n)], 2, max)) 
	for(j in t.row) p02n[j, 2:ncol(p02n)] <- remp ; print(i)
}
p02n <- unique(p02n)
dim(p02n) #782  313
p02n[1:10, 1:10]

row.names(p02n) <- p02n$newID 
p02n$X5_4 <- as.character(p02n$X5_4)
p02n$X5_4 <- ifelse(p02n$X5_4==".", "0", p02n$X5_4)
p02n$X5_4 <- as.numeric(p02n$X5_4)

p02n2 <- as.data.frame(t(p02n[,2:ncol(p02n)]))
p02n2 <- apply(p02n2, 1:2, function(x) ifelse(is.na(x), 0, x))

# Final checks
p02n2[1:10, 1:10]
dim(p02n2)  # 312 sites 782 species

range(p02n2)
sort(rowSums(p02n2))

# Save it
write.table(p02n2, file="PlotSpc_2002_12July16.txt", sep="\t", quote=F)


#********************************************************************
# 2008
#********************************************************************
p08[1:10, 1:10]
dim(p08) # 263  812

old <- names(p08)
length(old) # 812
length(unique(old)) # 812

# Check that all species are in the list
length(old[old %in% syn$ini]) # 788
out <- old[!old %in% syn$ini]

out2 <- substring(out, 1, nchar(out)-1)
out2[!out2 %in% syn$ini]
out2

# adapt them to the correspondance list
old_corr <- old
old_corr[!old_corr %in% syn$ini] <- out2
length(old_corr[old_corr %in% syn$ini]) # 812

# replace the species names by the correct ones
new <- syn[old_corr,"fin"]
length(new)  # 812
COR <- cbind(oldID=old, oldCORR=old_corr, newID=new)

p08n <- p08
names(p08n) <- COR[order(COR[,1]), "newID"]
dim(p08n) # 263   812
p08n[1:10, 1:10]

# resume the duplicates
p08n1 <- as.data.frame(cbind(spcID=names(p08n), t(p08n)), row.names=1:length(p08n))
p08n1[1:10, 1:10]
dim(p08n1) #   812  264

dupli <- unique(as.character(p08n1$spcID[duplicated(p08n1$spcID)]))
for(i in dupli) { 
	t.row <- row.names(p08n1[which(p08n1==i),])
	remp <- as.numeric(apply(p08n1[t.row, 2:ncol(p08n1)], 2, max)) 
	for(j in t.row) p08n1[j, 2:ncol(p08n1)] <- remp ; print(i)
}
p08n1 <- unique(p08n1)
dim(p08n1) #   683  264
p08n1[1:10, 1:10]

row.names(p08n1) <- p08n1$spcID 
p08n2 <- as.data.frame(t(p08n1[,2:ncol(p08n1)]))
p08n2 <- apply(p08n2[,1:ncol(p08n2)], 1:2, function(x) ifelse(is.na(x), 0, x))
p08n2 <- as.data.frame(p08n2)

for(i in 1:ncol(p08n2)) p08n2[,i] <- as.numeric(as.character(p08n2[,i]))

# Final checks
p08n2[1:10, 1:10]
dim(p08n2)  # 263 sites  683 species

range(p08n2)
sort(colSums(p08n2))
unique(unlist(p08n2))

# Save it
write.table(p08n2, file="PlotSpc_2008_12July16.txt", sep="\t", quote=F)


#********************************************************************
# 2011
#********************************************************************
p11[1:10, 1:10]
dim(p11) # 62  381

old <- names(p11)
length(old) # 381
length(unique(old)) # 381

# Check that all species are in the list
length(old[old %in% syn$ini]) # 367
out <- old[!old %in% syn$ini]

out2 <- substring(out, 1, nchar(out)-1)
out2[!out2 %in% syn$ini]
out2

# adapt them to the correspondance list
old_corr <- old
old_corr[!old_corr %in% syn$ini] <- out2
length(old_corr[old_corr %in% syn$ini]) # 381

# replace the species names by the correct ones
new <- syn[old_corr,"fin"]
length(new)  # 381
COR <- cbind(oldID=old, oldCORR=old_corr, newID=new)
head(COR)

p11n <- p11
names(p11n) <- COR[order(COR[,1]), "newID"]
dim(p11n) # 62  381
p11n[1:10, 1:10]

# resume the duplicates
p11n1 <- as.data.frame(cbind(spcID=names(p11n), t(p11n)), row.names=1:length(p11n))
p11n1[1:10, 1:10]
dim(p11n1) #   381  63

dupli <- unique(as.character(p11n1$spcID[duplicated(p11n1$spcID)]))
for(i in dupli) { 
	t.row <- row.names(p11n1[which(p11n1==i),])
	remp <- as.numeric(apply(p11n1[t.row, 2:ncol(p11n1)], 2, max)) 
	for(j in t.row) p11n1[j, 2:ncol(p11n1)] <- remp ; print(i)
}
p11n1 <- unique(p11n1)
dim(p11n1) #   272  63
p11n1[1:10, 1:10]

row.names(p11n1) <- p11n1$spcID 
p11n2 <- as.data.frame(t(p11n1[,2:ncol(p11n1)]))
p11n2 <- apply(p11n2, 1:2, function(x) ifelse(is.na(x), 0, x))
p11n2 <- as.data.frame(p11n2)

for(i in 1:ncol(p11n2)) p11n2[,i] <- as.numeric(as.character(p11n2[,i]))

# Final checks
p11n2[1:10, 1:10]
dim(p11n2)  # 62 sites  272 species

range(p11n2)
sort(colSums(p11n2))
sort(rowSums(p11n2))
unique(unlist(p11n2))

# Save it
write.table(p11n2, file="PlotSpc_2011_12July16.txt", sep="\t", quote=F)



#********************************************************************
# 2014
#********************************************************************
p14[1:10, 1:10]
dim(p14) # 141  476

old <- names(p14)
length(old) # 476
length(unique(old)) # 476

# Check that all species are in the list
length(old[old %in% syn$ini]) # 467
out <- old[!old %in% syn$ini]

out2 <- substring(out, 1, nchar(out)-1)
out2[!out2 %in% syn$ini]
out2

# adapt them to the correspondance list
old_corr <- old
old_corr[!old_corr %in% syn$ini] <- out2
length(old_corr[old_corr %in% syn$ini]) # 476

# replace the species names by the correct ones
new <- syn[old_corr,"fin"]
length(new)  # 476
COR <- cbind(oldID=old, oldCORR=old_corr, newID=new)
tail(COR)

p14n <- p14
names(p14n) <- COR[order(COR[,1]), "newID"]
dim(p14n) # 141  476
p14n[1:10, 1:10]

# resume the duplicates
p14n1 <- as.data.frame(cbind(spcID=names(p14n), t(p14n)), row.names=1:length(p14n))
p14n1[1:10, 1:10]
dim(p14n1) #   476  142

dupli <- unique(as.character(p14n1$spcID[duplicated(p14n1$spcID)]))
for(i in dupli) { 
	t.row <- row.names(p14n1[which(p14n1==i),])
	remp <- as.numeric(apply(p14n1[t.row, 2:ncol(p14n1)], 2, max)) 
	for(j in t.row) p14n1[j, 2:ncol(p14n1)] <- remp ; print(i)
}
p14n1 <- unique(p14n1)
dim(p14n1) #   445 142
p14n1[1:10, 1:10]

row.names(p14n1) <- p14n1$spcID 
p14n2 <- as.data.frame(t(p14n1[,2:ncol(p14n1)]))
p14n2 <- apply(p14n2, 1:2, function(x) ifelse(is.na(x), 0, x))
p14n2 <- as.data.frame(p14n2)

for(i in 1:ncol(p14n2)) p14n2[,i] <- as.numeric(as.character(p14n2[,i]))

# Final checks
p14n2[1:10, 1:10]
dim(p14n2)  # 141 sites  445 species

range(p14n2)
sort(colSums(p14n2))
sort(rowSums(p14n2))
unique(unlist(p14n2))

# Save it
write.table(p14n2, file="PlotSpc_2014_12July16.txt", sep="\t", quote=F)



######################################################################
######################################################################
# Check and change spc names on the SUB-plot data
######################################################################
######################################################################

#********************************************************************
#********************************************************************
# 2002
#********************************************************************
#********************************************************************
sp02_Abun[1:10, 1:10] 
sp02_Cov[1:10, 1:10]

dim(sp02_Abun) # 829  625
dim(sp02_Cov)  # 829  625


old <- as.character(sp02_Abun$spcID)
length(old) # 829
length(unique(old)) # 824

dd <- old[duplicated(old)]
sp02_Abun[which(sp02_Abun$spcID %in% dd),]

# Check that all species are in the list
length(old[old %in% syn$ini]) # 809
out <- old[!old %in% syn$ini]
out2 <- substring(out, 1, nchar(out)-3)
out2
out2[!out2 %in% syn$ini]

# adapt them to the correspondance list
old_corr <- old
old_corr[!old_corr %in% syn$ini] <- out2
length(old_corr[old_corr %in% syn$ini]) # 829

# replace the species names by the correct ones
new <- syn[old_corr,"fin"]
length(new)  # 829
COR <- unique(cbind(oldID=old, oldCORR=old_corr, newID=new))

#====================
# ABUNDANCE DATA
#====================
sp02_An <- merge(COR, sp02_Abun, by.x="oldID", by.y="spcID")
dim(sp02_An) # 829  627

# make the final file format
sp02_An <- sp02_An[,c(3:ncol(sp02_An))]
sp02_An[1:10, 1:10]

# TRANSFORM THE '*' into '0'
sp02_An[,2:ncol(sp02_An)] <- apply(sp02_An[,2:ncol(sp02_An)], 1:2, function(x) ifelse(x=="*", 0, x))
for(i in 2:ncol(sp02_An)) sp02_An[,i] <- as.numeric(as.character(sp02_An[,i]))
sp02_An[1:10, 1:10]

# resume the duplicates
dupli <- unique(as.character(sp02_An$newID[duplicated(sp02_An$newID)]))
for(i in dupli) { 
	t.row <- row.names(sp02_An[which(sp02_An==i),])
	remp <- colSums(sp02_An[t.row, 2:ncol(sp02_An)], na.rm=T)
	for(j in t.row) sp02_An[j, 2:ncol(sp02_An)] <- remp ; print(i)
}
sp02_An <- unique(sp02_An)
dim(sp02_An) #782  625
sp02_An[1:10, 1:10]

row.names(sp02_An) <- sp02_An$newID 

sp02_An2 <- as.data.frame(t(sp02_An[,2:ncol(sp02_An)]))
sp02_An2 <- apply(sp02_An2, 1:2, function(x) ifelse(is.na(x), 0, x))

# Final checks
sp02_An2[1:10, 1:10]
dim(sp02_An2)  # 624 sites 782 species

range(sp02_An2)
sort(rowSums(sp02_An2))

# Save it
write.table(sp02_An2, file="subPlotSpc_2002_NbIndiv_12July16.txt", sep="\t", quote=F)

#====================
# COVER DATA
#====================
sp02_Cn <- merge(COR, sp02_Cov, by.x="oldID", by.y="spcID")
dim(sp02_Cn) # 829  627

# make the final file format
sp02_Cn <- sp02_Cn[,c(3:ncol(sp02_Cn))]
sp02_Cn[1:10, 1:10]

# TRANSFORM THE '*' into '0'
sp02_Cn[,2:ncol(sp02_Cn)] <- apply(sp02_Cn[,2:ncol(sp02_Cn)], 1:2, function(x) ifelse(x %in% c("*", "+"), 0, x))
for(i in 2:ncol(sp02_Cn)) sp02_Cn[,i] <- as.numeric(as.character(sp02_Cn[,i]))
sp02_Cn[1:10, 1:10]

# resume the duplicates
dupli <- unique(as.character(sp02_Cn$newID[duplicated(sp02_Cn$newID)]))
for(i in dupli) { 
	t.row <- row.names(sp02_Cn[which(sp02_Cn==i),])
	remp <- colSums(sp02_Cn[t.row, 2:ncol(sp02_Cn)], na.rm=T)
	for(j in t.row) sp02_Cn[j, 2:ncol(sp02_Cn)] <- remp ; print(i)
}
sp02_Cn <- unique(sp02_Cn)
dim(sp02_Cn) #782  625
sp02_Cn[1:10, 1:10]

row.names(sp02_Cn) <- sp02_Cn$newID 

sp02_Cn2 <- as.data.frame(t(sp02_Cn[,2:ncol(sp02_Cn)]))
sp02_Cn2 <- apply(sp02_Cn2, 1:2, function(x) ifelse(is.na(x), 0, x))

# Final checks
sp02_Cn2[1:10, 1:10]
dim(sp02_Cn2)  # 624 sites 782 species

range(sp02_Cn2)
sort(rowSums(sp02_Cn2))


# Save it
write.table(sp02_Cn2, file="subPlotSpc_2002_PropCover_12July16.txt", sep="\t", quote=F)



#********************************************************************
#********************************************************************
# 2008
#********************************************************************
#********************************************************************
sp08_Abun[1:10, 1:10] 
sp08_Cov[1:10, 1:10]

dim(sp08_Abun) # 527  595
dim(sp08_Cov)  # 527  595


old <- names(sp08_Abun)
length(old) # 595
length(unique(old)) # 595

# Check that all species are in the list
length(old[old %in% syn$ini]) # 574
out <- old[!old %in% syn$ini]
out

out2 <- substring(out, 1, nchar(out)-1)
out2[!out2 %in% syn$ini]
out2

# adapt them to the correspondance list
old_corr <- old
old_corr[!old_corr %in% syn$ini] <- out2
length(old_corr[old_corr %in% syn$ini]) # 595

# replace the species names by the correct ones
new <- syn[old_corr,"fin"]
length(new)  # 595
COR <- cbind(oldID=old, oldCORR=old_corr, newID=new)
head(COR)

#====================
# ABUNDANCE DATA
#====================
sp08_An <- sp08_Abun
names(sp08_An) <- COR[order(COR[,1]), "newID"]
dim(sp08_An) # 527  595 
sp08_An[1:10, 1:10]

# TRANSFORM THE '*' into '0'
sp08_An <- apply(sp08_An, 1:2, function(x) ifelse(x %in% c("*", "+"), 0, x))
for(i in 1:ncol(sp08_An)) sp08_An[,i] <- as.numeric(as.character(sp08_An[,i]))
sp08_An[1:10, 1:10]

# resume the duplicates
sp08_An1 <- as.data.frame(cbind(spcID=colnames(sp08_An), t(sp08_An)), row.names=1:length(sp08_An))
for(i in 2:ncol(sp08_An1)) sp08_An1[,i] <- as.numeric(as.character(sp08_An1[,i]))
sp08_An1[1:10, 1:10]
dim(sp08_An1) #   595  528

dupli <- unique(as.character(sp08_An1$spcID[duplicated(sp08_An1$spcID)]))
for(i in dupli) { 
	t.row <- row.names(sp08_An1[which(sp08_An1==i),])
	remp <- colSums(sp08_An1[t.row, 2:ncol(sp08_An1)], na.rm=T)
	for(j in t.row) sp08_An1[j, 2:ncol(sp08_An1)] <- remp ; print(i)
}
sp08_An1 <- unique(sp08_An1)
dim(sp08_An1) #   516  528
sp08_An1[1:10, 1:10]

row.names(sp08_An1) <- sp08_An1$spcID 
sp08_An2 <- as.data.frame(t(sp08_An1[,2:ncol(sp08_An1)]))
sp08_An2 <- apply(sp08_An2[,1:ncol(sp08_An2)], 1:2, function(x) ifelse(is.na(x), 0, x))
sp08_An2 <- as.data.frame(sp08_An2)

for(i in 1:ncol(sp08_An2)) sp08_An2[,i] <- as.numeric(as.character(sp08_An2[,i]))

# Final checks
sp08_An2[1:10, 1:10]
dim(sp08_An2)  # 527 sites  516 species

range(sp08_An2)
sort(colSums(sp08_An2))
unique(unlist(sp08_An2))

# Save it
write.table(sp08_An2, file="subPlotSpc_2008_NbIndiv_12July16.txt", sep="\t", quote=F)


#====================
# COVER DATA
#====================
sp08_Cn <- sp08_Cov
names(sp08_Cn) <- COR[order(COR[,1]), "newID"]
dim(sp08_Cn) # 527  595 
sp08_Cn[1:10, 1:10]

# TRANSFORM THE '*' into '0'
sp08_Cn <- apply(sp08_Cn, 1:2, function(x) ifelse(x %in% c("*", "+"), 0, x))
for(i in 1:ncol(sp08_Cn)) sp08_Cn[,i] <- as.numeric(as.character(sp08_Cn[,i]))
sp08_Cn[1:10, 1:10]

# resume the duplicates
sp08_Cn1 <- as.data.frame(cbind(spcID=colnames(sp08_Cn), t(sp08_Cn)), row.names=1:length(sp08_Cn))
for(i in 2:ncol(sp08_Cn1)) sp08_Cn1[,i] <- as.numeric(as.character(sp08_Cn1[,i]))
sp08_Cn1[1:10, 1:10]
dim(sp08_Cn1) #   595  528

dupli <- unique(as.character(sp08_Cn1$spcID[duplicated(sp08_Cn1$spcID)]))
for(i in dupli) { 
	t.row <- row.names(sp08_Cn1[which(sp08_Cn1==i),])
	remp <- colSums(sp08_Cn1[t.row, 2:ncol(sp08_Cn1)], na.rm=T)
	for(j in t.row) sp08_Cn1[j, 2:ncol(sp08_Cn1)] <- remp ; print(i)
}
sp08_Cn1 <- unique(sp08_Cn1)
dim(sp08_Cn1) #   516  528
sp08_Cn1[1:10, 1:10]

row.names(sp08_Cn1) <- sp08_Cn1$spcID 
sp08_Cn2 <- as.data.frame(t(sp08_Cn1[,2:ncol(sp08_Cn1)]))
sp08_Cn2 <- apply(sp08_Cn2[,1:ncol(sp08_Cn2)], 1:2, function(x) ifelse(is.na(x), 0, x))
sp08_Cn2 <- as.data.frame(sp08_Cn2)

for(i in 1:ncol(sp08_Cn2)) sp08_Cn2[,i] <- as.numeric(as.character(sp08_Cn2[,i]))

# Final checks
sp08_Cn2[1:10, 1:10]
dim(sp08_Cn2)  # 527 sites  516 species

range(sp08_Cn2)
sort(colSums(sp08_Cn2))
unique(unlist(sp08_Cn2))

# Save it
write.table(sp08_Cn2, file="subPlotSpc_2008_PropCover_12July16.txt", sep="\t", quote=F)



#********************************************************************
#********************************************************************
# 2011
#********************************************************************
#********************************************************************
sp11_Abun[1:10, 1:10] 
sp11_Cov[1:10, 1:10]

dim(sp11_Abun) # 131 354
dim(sp11_Cov)  # 131 354

old <- names(sp11_Abun)
length(old) # 354
length(unique(old)) # 354

# Check that all species are in the list
length(old[old %in% syn$ini]) # 344
out <- old[!old %in% syn$ini]
out

out2 <- substring(out, 1, nchar(out)-1)
out2[!out2 %in% syn$ini]
out2

# adapt them to the correspondance list
old_corr <- old
old_corr[!old_corr %in% syn$ini] <- out2
length(old_corr[old_corr %in% syn$ini]) # 354

# replace the species names by the correct ones
new <- syn[old_corr,"fin"]
length(new)  # 354
COR <- cbind(oldID=old, oldCORR=old_corr, newID=new)
head(COR)

#====================
# ABUNDANCE DATA
#====================
sp11_An <- sp11_Abun
names(sp11_An) <- COR[order(COR[,1]), "newID"]
dim(sp11_An) # 131  354
sp11_An[1:10, 1:10]

# TRANSFORM THE '*' into '0'
sp11_An <- apply(sp11_An, 1:2, function(x) ifelse(x %in% c("*", "+"), 0, x))
for(i in 1:ncol(sp11_An)) sp11_An[,i] <- as.numeric(as.character(sp11_An[,i]))
sp11_An[1:10, 1:10]

# resume the duplicates
sp11_An1 <- as.data.frame(cbind(spcID=colnames(sp11_An), t(sp11_An)), row.names=1:length(sp11_An))
for(i in 2:ncol(sp11_An1)) sp11_An1[,i] <- as.numeric(as.character(sp11_An1[,i]))
sp11_An1[1:10, 1:10]
dim(sp11_An1) #   354 132

dupli <- unique(as.character(sp11_An1$spcID[duplicated(sp11_An1$spcID)]))
for(i in dupli) { 
	t.row <- row.names(sp11_An1[which(sp11_An1==i),])
	remp <- colSums(sp11_An1[t.row, 2:ncol(sp11_An1)], na.rm=T)
	for(j in t.row) sp11_An1[j, 2:ncol(sp11_An1)] <- remp ; print(i)
}
sp11_An1 <- unique(sp11_An1)
dim(sp11_An1) #   236  132
sp11_An1[1:10, 1:10]

row.names(sp11_An1) <- sp11_An1$spcID 
sp11_An2 <- as.data.frame(t(sp11_An1[,2:ncol(sp11_An1)]))
sp11_An2 <- apply(sp11_An2[,1:ncol(sp11_An2)], 1:2, function(x) ifelse(is.na(x), 0, x))
sp11_An2 <- as.data.frame(sp11_An2)

for(i in 1:ncol(sp11_An2)) sp11_An2[,i] <- as.numeric(as.character(sp11_An2[,i]))

# Final checks
sp11_An2[1:10, 1:10]
dim(sp11_An2)  # 131 sites  236 species

range(sp11_An2)
sort(colSums(sp11_An2))
unique(unlist(sp11_An2))

# Save it
write.table(sp11_An2, file="subPlotSpc_2011_NbIndiv_12July16.txt", sep="\t", quote=F)


#====================
# COVER DATA
#====================
sp11_Cn <- sp11_Cov
names(sp11_Cn) <- COR[order(COR[,1]), "newID"]
dim(sp11_Cn) # 131  354
sp11_Cn[1:10, 1:10]

# TRANSFORM THE '*' into '0'
sp11_Cn <- apply(sp11_Cn, 1:2, function(x) ifelse(x %in% c("*", "+"), 0, x))
for(i in 1:ncol(sp11_Cn)) sp11_Cn[,i] <- as.numeric(as.character(sp11_Cn[,i]))
sp11_Cn[1:10, 1:10]

# resume the duplicates
sp11_Cn1 <- as.data.frame(cbind(spcID=colnames(sp11_Cn), t(sp11_Cn)), row.names=1:length(sp11_Cn))
for(i in 2:ncol(sp11_Cn1)) sp11_Cn1[,i] <- as.numeric(as.character(sp11_Cn1[,i]))
sp11_Cn1[1:10, 1:10]
dim(sp11_Cn1) #   354 132

dupli <- unique(as.character(sp11_Cn1$spcID[duplicated(sp11_Cn1$spcID)]))
for(i in dupli) { 
	t.row <- row.names(sp11_Cn1[which(sp11_Cn1==i),])
	remp <- colSums(sp11_Cn1[t.row, 2:ncol(sp11_Cn1)], na.rm=T)
	for(j in t.row) sp11_Cn1[j, 2:ncol(sp11_Cn1)] <- remp ; print(i)
}
sp11_Cn1 <- unique(sp11_Cn1)
dim(sp11_Cn1) #   236   132
sp11_Cn1[1:10, 1:10]

row.names(sp11_Cn1) <- sp11_Cn1$spcID 
sp11_Cn2 <- as.data.frame(t(sp11_Cn1[,2:ncol(sp11_Cn1)]))
sp11_Cn2 <- apply(sp11_Cn2[,1:ncol(sp11_Cn2)], 1:2, function(x) ifelse(is.na(x), 0, x))
sp11_Cn2 <- as.data.frame(sp11_Cn2)

for(i in 1:ncol(sp11_Cn2)) sp11_Cn2[,i] <- as.numeric(as.character(sp11_Cn2[,i]))

# Final checks
sp11_Cn2[1:10, 1:10]
dim(sp11_Cn2)  # 131 sites  236 species

range(sp11_Cn2)
sort(colSums(sp11_Cn2))
unique(unlist(sp11_Cn2))

# Save it
write.table(sp11_Cn2, file="subPlotSpc_2011_PropCover_12July16.txt", sep="\t", quote=F)



#********************************************************************
#********************************************************************
# 2014
#********************************************************************
#********************************************************************
sp14_Abun[1:10, 1:10] 
sp14_Cov[1:10, 1:10]

dim(sp14_Abun) # 319  420
dim(sp14_Cov)  # 319  420

old <- names(sp14_Abun)
length(old) # 420
length(unique(old)) # 420

# Check that all species are in the list
length(old[old %in% syn$ini]) # 388
out <- old[!old %in% syn$ini]
out

out2 <- unlist(lapply(strsplit(out, "_"), function(x) paste(x[1], x[2], sep="_")))
out2[!out2 %in% syn$ini]
out2

# adapt them to the correspondance list
old_corr <- old
old_corr[!old_corr %in% syn$ini] <- out2
length(old_corr[old_corr %in% syn$ini]) # 420

# replace the species names by the correct ones
new <- syn[old_corr,"fin"]
length(new)  # 420
COR <- cbind(oldID=old, oldCORR=old_corr, newID=new)
head(COR)

#====================
# ABUNDANCE DATA
#====================
sp14_An <- sp14_Abun
names(sp14_An) <- COR[order(COR[,1]), "newID"]
dim(sp14_An) # 319  420
sp14_An[1:10, 1:10]

# TRANSFORM THE '*' into '0'
sp14_An <- apply(sp14_An, 1:2, function(x) ifelse(x %in% c("*", "+"), 0, x))
for(i in 1:ncol(sp14_An)) sp14_An[,i] <- as.numeric(as.character(sp14_An[,i]))
sp14_An[1:10, 1:10]

# resume the duplicates
sp14_An1 <- as.data.frame(cbind(spcID=colnames(sp14_An), t(sp14_An)), row.names=1:length(sp14_An))
for(i in 2:ncol(sp14_An1)) sp14_An1[,i] <- as.numeric(as.character(sp14_An1[,i]))
sp14_An1[1:10, 1:10]
dim(sp14_An1) #   420  320

dupli <- unique(as.character(sp14_An1$spcID[duplicated(sp14_An1$spcID)]))
for(i in dupli) { 
	t.row <- row.names(sp14_An1[which(sp14_An1==i),])
	remp <- colSums(sp14_An1[t.row, 2:ncol(sp14_An1)], na.rm=T)
	for(j in t.row) sp14_An1[j, 2:ncol(sp14_An1)] <- remp ; print(i)
}
sp14_An1 <- unique(sp14_An1)
dim(sp14_An1) #   351  320
sp14_An1[1:10, 1:10]

row.names(sp14_An1) <- sp14_An1$spcID 
sp14_An2 <- as.data.frame(t(sp14_An1[,2:ncol(sp14_An1)]))
sp14_An2 <- apply(sp14_An2[,1:ncol(sp14_An2)], 1:2, function(x) ifelse(is.na(x), 0, x))
sp14_An2 <- as.data.frame(sp14_An2)

for(i in 1:ncol(sp14_An2)) sp14_An2[,i] <- as.numeric(as.character(sp14_An2[,i]))

# Final checks
sp14_An2[1:10, 1:10]
dim(sp14_An2)  # 319 sites  351 species

range(sp14_An2)
sort(colSums(sp14_An2))
unique(unlist(sp14_An2))

# Save it
write.table(sp14_An2, file="subPlotSpc_2014_NbIndiv_12July16.txt", sep="\t", quote=F)


#====================
# COVER DATA
#====================
sp14_Cn <- sp14_Cov
names(sp14_Cn) <- COR[order(COR[,1]), "newID"]
dim(sp14_Cn) # 319  420
sp14_Cn[1:10, 1:10]

# TRANSFORM THE '*' into '0'
sp14_Cn <- apply(sp14_Cn, 1:2, function(x) ifelse(x %in% c("*", "+"), 0, x))
for(i in 1:ncol(sp14_Cn)) sp14_Cn[,i] <- as.numeric(as.character(sp14_Cn[,i]))
sp14_Cn[1:10, 1:10]

# resume the duplicates
sp14_Cn1 <- as.data.frame(cbind(spcID=colnames(sp14_Cn), t(sp14_Cn)), row.names=1:length(sp14_Cn))
for(i in 2:ncol(sp14_Cn1)) sp14_Cn1[,i] <- as.numeric(as.character(sp14_Cn1[,i]))
sp14_Cn1[1:10, 1:10]
dim(sp14_Cn1) #   420  320

dupli <- unique(as.character(sp14_Cn1$spcID[duplicated(sp14_Cn1$spcID)]))
for(i in dupli) { 
	t.row <- row.names(sp14_Cn1[which(sp14_Cn1==i),])
	remp <- colSums(sp14_Cn1[t.row, 2:ncol(sp14_Cn1)], na.rm=T)
	for(j in t.row) sp14_Cn1[j, 2:ncol(sp14_Cn1)] <- remp ; print(i)
}
sp14_Cn1 <- unique(sp14_Cn1)
dim(sp14_Cn1) #   351  320
sp14_Cn1[1:10, 1:10]

row.names(sp14_Cn1) <- sp14_Cn1$spcID 
sp14_Cn2 <- as.data.frame(t(sp14_Cn1[,2:ncol(sp14_Cn1)]))
sp14_Cn2 <- apply(sp14_Cn2[,1:ncol(sp14_Cn2)], 1:2, function(x) ifelse(is.na(x), 0, x))
sp14_Cn2 <- as.data.frame(sp14_Cn2)

for(i in 1:ncol(sp14_Cn2)) sp14_Cn2[,i] <- as.numeric(as.character(sp14_Cn2[,i]))

# Final checks
sp14_Cn2[1:10, 1:10]
dim(sp14_Cn2)  # 131 sites  236 species

range(sp14_Cn2)
sort(colSums(sp14_Cn2))
unique(unlist(sp14_Cn2))

# Save it
write.table(sp14_Cn2, file="subPlotSpc_2014_PropCover_12July16.txt", sep="\t", quote=F)





































































































