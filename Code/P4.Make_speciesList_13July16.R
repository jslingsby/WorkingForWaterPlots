# Laure Gallien
# 13 July 2016

library(reshape)


setwd("~/Desktop/Dropbox/Travail_SA/3.ComEcol/CapeCommunities/Data/LaurePrep")

##############################################################
# Load the data
##############################################################


#------------------------------------------------
# Get the Cape Peninsula species list
#------------------------------------------------
cp <- read.delim("CapePeninsulaList_13July16.txt")
head(cp)


#------------------------------------------------
# Get the species x site data : 10 x 10m
#------------------------------------------------
# 2002 / 2008 / 2011 / 2014
#........
p02 <- read.delim("PlotSpc_2002_12July16.txt")
p08 <- read.delim("PlotSpc_2008_12July16.txt")
p11 <- read.delim("PlotSpc_2011_12July16.txt")
p14 <- read.delim("PlotSpc_2014_12July16.txt")

p02[1:10, 1:10]
p08[1:10, 1:10]
p11[1:10, 1:10]
p14[1:10, 1:10]


#------------------------------------------------
# Get the species x site data : 1 x 1m
#------------------------------------------------
# 2002 / 2008 / 2011 / 2014
#........
sp02_Abun <- read.delim("subPlotSpc_2002_NbIndiv_12July16.txt")
sp08_Abun <- read.delim("subPlotSpc_2008_NbIndiv_12July16.txt")
sp11_Abun <- read.delim("subPlotSpc_2011_NbIndiv_12July16.txt")
sp14_Abun <- read.delim("subPlotSpc_2014_NbIndiv_12July16.txt")

sp02_Abun[1:10, 1:10] 
sp08_Abun[1:10, 1:10] 
sp11_Abun[1:10, 1:10] 
sp14_Abun[1:10, 1:10] 




##############################################################
##############################################################
# create species list from the plot names
##############################################################
##############################################################

# get all species names for out communities
#-----------------------
lsp <- c(names(p02), names(p08), names(p11), names(p14), 
		names(sp02_Abun), names(sp08_Abun), names(sp11_Abun), names(sp14_Abun))
lsp <- sort(unique(lsp))

# remove identification at the genus level only
lsp <- na.omit(unlist(lapply(strsplit(lsp, "_"), function(x) ifelse(length(x)>1, paste(x[1], x[2], sep="_"), NA))))
length(lsp)  # 971 species
lsp
lsp2 <- data.frame(spcID=lsp)
write.table(lsp2, file="All_species_list_CapeCom_13July16.txt", sep="\t", quote=F, row.names=F)


# generate a summary table for species name checking
#-----------------------
head(lsp)
dfN <- c("p02", "p08", "p11", "p14", "sp02_Abun", "sp08_Abun", "sp11_Abun", "sp14_Abun")
YY <- c(2002, 2008, 2011, 2014, 2002, 2008, 2011, 2014) ; names(YY) <- dfN
l.melt <- list()
for(i in dfN) {
	DFN <- eval(parse(text=i))
	DFN$sitID <- row.names(DFN)
	temp <- data.frame(melt(DFN, id="sitID"), year=YY[i])
	l.melt[[i]] <- temp[which(temp$value>0), c("variable", "sitID", "year")] ; print(i)
}
SpcPlotYear <- do.call(rbind, l.melt)
SpcPlotYear <- SpcPlotYear[order(SpcPlotYear$variable),]

dim(SpcPlotYear) # 25633
tail(SpcPlotYear, 10)
write.table(SpcPlotYear, file="Spc_Plot_Year_13July16.txt", sep="\t", quote=F, row.names=F)



# Add it to the cape community list
#-----------------------
head(cp)
dim(cp) # 2886
cp$spcPlots <- ifelse(cp$spcID %in% lsp, 1, 0)
sum(cp$spcPlots)  # 758

spcNOTinCapList <- lsp[!lsp %in% cp$spcID]
length(spcNOTinCapList)  # 197


##############################################################
##############################################################
# Get invasive species information
##############################################################
##############################################################

# SAPIA db
sa <- read.delim("~/Desktop/Dropbox/Travail_SA/3.ComEcol/CapeCommunities/Data/Raw/List_Alien_SAPIA_13July16.txt")
head(sa) # 547 3

# Cape Community db
cc <- read.delim("~/Desktop/Dropbox/Travail_SA/3.ComEcol/CapeCommunities/Data/Raw/List_Alien_CapeCom_13July16.txt")
head(cc)  # 78

# Cape peninsula species list
cpi <- cp[is.na(cp$IUCN) & cp$spcPlots==1,] # --> I checked them all on internet
cpi <- c("Fumaria_muralis", "Hypochoeris_radicata", "Melilotus_indica", 
			"Oenothera_nocturna", "Polycarpon_tetraphyllum", "Populus_canescens")
( cpi <- data.frame(spcID=cpi, Status="Alien", Origin="CapeList") )

# iSPOT data
ii <- read.delim("~/Desktop/Dropbox/Travail_SA/3.ComEcol/CapeCommunities/Data/Raw/List_Alien_iSPOT_13July16.txt")
head(ii)  # 388

# Combine all Exotic species names
l.inv <- list(sa=sa, ii=ii, cc=cc, cpi=cpi)
linv <- unique(do.call(rbind, l.inv))
linv$spcID <- as.character(linv$spcID)

linv$spcID <- unlist(lapply(strsplit(linv$spcID, "_"), function(x) paste(x[1], x[2], sep="_")))
linv <- unique(linv)
dim(linv) # 1016
head(linv)

invSpc <- sort(unique(linv$spcID))  # 876

# remove non species level id, and hybrids
invSpc <- invSpc[-grep(c("_sp."), invSpc)]
invSpc <- invSpc[-grep(c("_X"), invSpc)]
invSpc <- invSpc[-grep(c("_x"), invSpc)]
invSpc <- invSpc[-grep(c("/"), invSpc)]
invSpc  # 778

write.table(invSpc, file="Exotic_species_list_SA_13July16.txt", sep="\t", quote=F, row.names=F)


# Select the exotic species that are in our communities
myExo <- invSpc[invSpc %in% lsp]
write.table(myExo, file="Exotic_species_list_CapeCom_13July16.txt", sep="\t", quote=F, row.names=F)




















































