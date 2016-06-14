##############################################################################
######## Code to accumulate plant names for Working for Water plots on the Cape Peninsula 
######## Data collected by Doug Euston-Brown, Susan Botha and Ryan Blanchard
##############################################################################
######## Compiled by Jasper Slingsby 2014
######## Last edited: 13 June 2016
##############################################################################
######## 
###Steps:
###1) Get libraries and data
###2) Generate a list of all species names, synonyms, misspellings etc
##############################################################################

###Get libraries and set working directory
library(gdata)

if (Sys.getenv("USER")=="jasper") {setwd("/Users/jasper/Dropbox/Shared/CapeCommunities/Data/Raw/")}
if (Sys.getenv("USER")=="laure") {setwd("/Users/laure/Dropbox/CapeCommunities/Data/Raw/")}

###Suffix codes
#t = trait
#m = mortality
#p = 10x10m plot
#sp = 1x1m subplot
#pl = plot data (no species names)

###Get treatment data
#Get plot treatment info
treat<-read.xls("plot_treatments.xlsx", sheet=1, stringsAsFactors=FALSE)

###Get data from 2002 survey
#Traits
dat02t<-read.xls("plants.xls", sheet=1, stringsAsFactors=FALSE)

#10 by 10m plots - need to combine
dat02p1<-read.xls("plots.xls", sheet=1, stringsAsFactors=FALSE)
dat02p2<-read.xls("plots.xls", sheet=2, stringsAsFactors=FALSE)
#colnames(dat02p2)[2] <- "Species"
#dat02p <- merge(dat02p1, dat02p2, all = T)

#Sub-plots - Need to combine sheets 1 to 8!!!
dat02sp1<-read.xls("sub-plots.xls", sheet=1, stringsAsFactors=FALSE) 
dat02sp2<-read.xls("sub-plots.xls", sheet=2, stringsAsFactors=FALSE)
dat02sp3<-read.xls("sub-plots.xls", sheet=3, stringsAsFactors=FALSE)
dat02sp4<-read.xls("sub-plots.xls", sheet=4, stringsAsFactors=FALSE)
dat02sp5<-read.xls("sub-plots.xls", sheet=5, stringsAsFactors=FALSE)
dat02sp6<-read.xls("sub-plots.xls", sheet=6, stringsAsFactors=FALSE)
dat02sp7<-read.xls("sub-plots.xls", sheet=7, stringsAsFactors=FALSE)
dat02sp8<-read.xls("sub-plots.xls", sheet=8, stringsAsFactors=FALSE)

#dat02sp <- merge(dat02sp1, dat02sp2, all=T)

#Get data from 2002 survey edited during 2008 survey
#dat02sp_08<-read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2008 Resurvey_Blanchard and Euston-Brown/2008 Resurvey data/1x1m 2000 plot data.xlsx", sheet=1, pattern="plot", stringsAsFactors=FALSE)

###Get data from 2008 survey
#Plot data
dat08pl<-read.xls("meta data files 2008.xlsx", sheet=4, stringsAsFactors=FALSE)

#Sub-plot data
dat08sp<-read.xls("meta data files 2008.xlsx", sheet=6, stringsAsFactors=FALSE)

#Mortality data
dat08m<-read.xls("meta data files 2008.xlsx", sheet=3, stringsAsFactors=FALSE)

#10x10m plot data
dat08p<-read.xls("meta data files 2008.xlsx", sheet=5, stringsAsFactors=FALSE)

###Get data from 2011 survey
#Plot data
dat11pl<-read.xls("Veg data 2011.xlsx", sheet=2, stringsAsFactors=FALSE)

#Sub-plot data
dat11sp<-read.xls("Veg data 2011.xlsx", sheet=5, stringsAsFactors=FALSE)

#10x10m plot data
dat11p<-read.xls("Veg data 2011.xlsx", sheet=4, stringsAsFactors=FALSE)

###Get data from 2014 survey
#Plot data
dat14pl <- read.xls("SAEON_2014_15_revisedAugust2015.xlsx", sheet = 1, stringsAsFactors=FALSE)

#Sub - plot data
dat14sp <- read.xls("SAEON_2014_15_revisedAugust2015.xlsx", sheet = 2, stringsAsFactors=FALSE)
dat14sp$Genus_species <- paste(dat14sp$Genus, dat14sp$Species, sep=" ") 

#10x10m Plot data
dat14p <- read.xls("SAEON_2014_15_revisedAugust2015.xlsx", sheet = 3, stringsAsFactors=FALSE)
dat14p$Genus_species <- paste(dat14p$Genus, dat14p$Species, sep=" ")

#Mortality data
dat14m <- read.xls("SAEON_2014_15_revisedAugust2015.xlsx", sheet = 4, stringsAsFactors=FALSE)
dat14m$Genus_species <- paste(dat14m$Genus, dat14m$Species, sep=" ") 

###Get all names, merge and unique
spnms <- sort(unique(trim(c(dat02t$Genus.and.species, dat02p1[,2], dat02p2[,2], dat02sp1[,1], dat02sp2[,1], dat02sp3[,1], dat02sp4[,1], dat02sp5[,1], dat02sp6[,1], dat02sp7[,1], dat02sp8[,1], #2002
dat08p$species, dat08sp$species, dat08m$species, #2008
dat11p$species, dat11sp$species, #2011
dat14sp$Genus_species, dat14p$Genus_species, dat14m$Genus_species #2014
                   ))))

spnms <- data.frame(Original=spnms, Fixed=spnms, Official=rep(0, length(spnms)), Synonym=rep(0, length(spnms)), stringsAsFactors=FALSE)

###Get Manning and Goldblatt 2013 names and synonyms
syn <- read.xls ("/Users/jasper/Dropbox/Shared/Synonym cleaning/Conspectuses_alternate_names_JFColville_10Jan2012_DUPLICATES_Emma_spelling.xlsx",sheet=1, stringsAsFactors=FALSE)# 

#Identify known synonyms
spnms$Synonym[which(spnms$Original%in%syn$Alternative.Name)] <- 1
spnms$Official[which(spnms$Original%in%syn$Taxon)] <- 1

#Write out file for error checking in the Taxon Name Resolution Service etc
write.csv(spnms, "allnames.csv", row.names = F)

###Get "fixed" names and explore
nnms <- read.csv("/Users/jasper/Dropbox/Shared/CapeCommunities/Data/Raw/allnames.csv", stringsAsFactors = F)
length(which(nnms$TNRS_Overall_score==1))
length(which(nnms$TNRS_Taxonomic_status=="Accepted"))
length(which(nnms$TNRS_Taxonomic_status=="Synonym"))
length(which(nnms$TNRS_Taxonomic_status=="Accepted" & nnms$TNRS_Overall_score==1))
length(which(nnms$TNRS_Taxonomic_status=="Synonym" & nnms$TNRS_Overall_score==1))
length(which(nnms$TNRS_Taxonomic_status=="No opinion" & nnms$TNRS_Overall_score==1))
