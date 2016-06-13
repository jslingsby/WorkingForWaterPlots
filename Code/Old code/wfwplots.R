library(gdata)
library(vegan)
library(picante)
library(MASS)
library(car)

#Read in data and fix funny values (non-numeric and non-integer)
#Get treatment info
treat<-read.xls("/Users/jasper/Documents/Dell/Jasper/Databases/Working for Water Peninsula plots/plot_treatments.xlsx", sheet=1, stringsAsFactors=FALSE)

#Get plot data and merge
dat1<-read.xls("/Users/jasper/Documents/Dell/Jasper/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2002 Survey_Euston-Brown and Botha/Report/plots.xls", sheet=1, stringsAsFactors=FALSE)
dat2<-read.xls("/Users/jasper/Documents/Dell/Jasper/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2002 Survey_Euston-Brown and Botha/Report/plots.xls", sheet=2, stringsAsFactors=FALSE)
colnames(dat1)[2]<-"Species"
colnames(dat2)[2]<-"Species"

#Which species aren't matched between spreadsheets
cbind(dat1$Species[which(!dat1$Species%in%dat2$Species)],dat2$Species[which(!dat2$Species%in%dat1$Species)])
#Temporary fix - need to see which match trait data...
dat1$Species[which(!dat1$Species%in%dat2$Species)]<-dat2$Species[which(!dat2$Species%in%dat1$Species)]

dat<-merge(dat1,dat2)