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

###Get libraries
library(gdata)


###Suffix codes
#t = trait
#m = mortality
#p = 10x10m plot
#sp = 1x1m subplot
#pl = plot data (no species names)

###Get treatment data
#Get plot treatment info
treat<-read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/plot_treatments.xlsx", sheet=1, stringsAsFactors=FALSE)

###Get data from 2002 survey
#Traits
dat02t<-read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2002 Survey_Euston-Brown and Botha/Report/plants.xls", sheet=1, stringsAsFactors=FALSE)

#10 by 10m plots - need to combine
dat02p1<-read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2002 Survey_Euston-Brown and Botha/Report/plots.xls", sheet=1, stringsAsFactors=FALSE)
dat02p2<-read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2002 Survey_Euston-Brown and Botha/Report/plots.xls", sheet=2, stringsAsFactors=FALSE)

#Sub-plots - Need to combine sheets 1 to 8!!!
dat02sp1<-read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2002 Survey_Euston-Brown and Botha/Report/sub-plots.xls", sheet=1, stringsAsFactors=FALSE) 
dat02sp2<-read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2002 Survey_Euston-Brown and Botha/Report/sub-plots.xls", sheet=2, stringsAsFactors=FALSE)
dat02sp3<-read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2002 Survey_Euston-Brown and Botha/Report/sub-plots.xls", sheet=3, stringsAsFactors=FALSE)
dat02sp4<-read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2002 Survey_Euston-Brown and Botha/Report/sub-plots.xls", sheet=4, stringsAsFactors=FALSE)
dat02sp5<-read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2002 Survey_Euston-Brown and Botha/Report/sub-plots.xls", sheet=5, stringsAsFactors=FALSE)
dat02sp6<-read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2002 Survey_Euston-Brown and Botha/Report/sub-plots.xls", sheet=6, stringsAsFactors=FALSE)
dat02sp7<-read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2002 Survey_Euston-Brown and Botha/Report/sub-plots.xls", sheet=7, stringsAsFactors=FALSE)
dat02sp8<-read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2002 Survey_Euston-Brown and Botha/Report/sub-plots.xls", sheet=8, stringsAsFactors=FALSE)

#Get data from 2002 survey edited during 2008 survey
dat02sp_08<-read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2008 Resurvey_Blanchard and Euston-Brown/2008 Resurvey data/1x1m 2000 plot data.xlsx", sheet=1, pattern="plot", stringsAsFactors=FALSE)

###Get data from 2008 survey
#Plot data
dat08pl<-read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2008 Resurvey_Blanchard and Euston-Brown/2008 Resurvey data/meta data files 2008.xlsx", sheet=4, stringsAsFactors=FALSE)

#Sub-plot data
dat08sp<-read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2008 Resurvey_Blanchard and Euston-Brown/2008 Resurvey data/meta data files 2008.xlsx", sheet=6, stringsAsFactors=FALSE)

#Mortality data
dat08m<-read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2008 Resurvey_Blanchard and Euston-Brown/2008 Resurvey data/meta data files 2008.xlsx", sheet=3, stringsAsFactors=FALSE)

#10x10m plot data
dat08p<-read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2008 Resurvey_Blanchard and Euston-Brown/2008 Resurvey data/meta data files 2008.xlsx", sheet=5, stringsAsFactors=FALSE)

#Get data from 2014 survey
dat14pl <- read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2014 Survey/Data/SAEON_2014_15_revisedAugust2015.xlsx", sheet = 1, stringsAsFactors=FALSE)
dat14sp <- read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2014 Survey/Data/SAEON_2014_15_revisedAugust2015.xlsx", sheet = 2, stringsAsFactors=FALSE)
dat14p <- read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2014 Survey/Data/SAEON_2014_15_revisedAugust2015.xlsx", sheet = 3, stringsAsFactors=FALSE)
dat14m <- read.xls("/Users/jasper/Documents/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2014 Survey/Data/SAEON_2014_15_revisedAugust2015.xlsx", sheet = 4, stringsAsFactors=FALSE)

names<-sort(unique(c(trait$Genus.and.species, dat2$species, dat8$species)))
