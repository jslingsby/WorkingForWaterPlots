##############################################################################
######## Code to set up treatment data for WfW plots - JSlingsby
##############################################################################
######## Last edited: 6 July 2016
##############################################################################
######## 
###Steps:
##############################################################################
##############################################################################
###1) Get libraries and setwd
##############################################################################
library(maptools)
library(raster)
library(rgdal)
library(sp)
library(gdata)
#library(lhs)

###set directories
#if (Sys.getenv("USER")=="jasper"){}

###Get in existing treatment and point data
pnts <- read.csv("/Users/jasper/Dropbox/Shared/Postfire_workshop/data/clean/alienplots.csv", header=T, row.names=1)
pl <- pnts
coordinates(pl) <- ~ x + y
proj4string(pl) <- "+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

###read in 30m data
dat <- stack(list.files("/Users/jasper/Dropbox/SAEON/Projects/Cape Peninsula/Rasters/layers", ".asc", full.names =T)) #read in all ".asc" files - ascii rasters
proj4string(dat)="+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #set projection

###Get 2015 NDVI
#ndvi2015 <- raster("/Users/jasper/Dropbox/Shared/ngschirps/Spatial data/20160531_26dbab02_LC8_L1T_ANNUAL_GREENEST_TOA__2013-2015.tif", band=3)
#NAvalue(ndvi2015)=0
#offs(ndvi2015)=-2
#gain(ndvi2015)=.001

###Combine data so far into a spatialPoints object
#dat <- stack(dat, ndvi2015, A_lig_SDMMX, fires)
#y <- as.data.frame(rasterToPoints(dat))
#y <- na.omit(y)
#coordinates(y) <- ~x+y
#proj4string(y) <- proj4string(dat)

###Fire data
#Get and clean fire data

fire <- readOGR(dsn = "/Users/jasper/Documents/GIS/CapePeninsula/TMNP/1962-2016 v1", layer="TMNP_fires_1962_2016")
fire <- spTransform(fire, CRSobj = CRS(proj4string(dat)))
fires <- rasterize(fire, dat, field = "YEAR", fun="max")

###Alien data
#a <- readOGR("/Users/jasper/Documents/GIS/CapePeninsula/InvasionData/TMNP All Sites 2015 ID.shp", layer = "TMNP All Sites 2015 ID")
#a <- spTransform(a, CRS(proj4string(dat)))
#adat <- read.xls("/Users/jasper/Documents/GIS/CapePeninsula/InvasionData/tmnp.xlsx", sheet=1, stringsAsFactors=F)
#adat <- adat[,which(colnames(adat)%in%c("Nbalid","TreatmentHectares","Total.Density","Total_Acac","Total.Pinus","Total.Eucal","Hakea","Populus","Lepto","All.Others"))]

##Other alien data
#hmm <- readOGR("/Users/jasper/Documents/GIS/CapePeninsula/InvasionData/Nbal Analysis_TMNP_20160314", layer = "Nbal Analysis_TMNP_Central_20160314")

###Extract and arrange alien data
#nbal <- pl %over% a
#y <- cbind(pl, nbal[,1:2])
#y <- merge(y, adat, all.x=T)

###Extract raster data to plots
pdat <- extract(dat, pl) #rasters

###Extract and re-arrange fire data
fdat <- over(pl, fire, returnList = TRUE) #fires - need to collapse
brns <- sapply(fdat, function(x){x$YEAR}) #Fires experienced by each plot
burns <- sort(unique(unlist(brns))) #All unique fires
bmat <- matrix(0, ncol=sum(burns>1999)+1, nrow=length(fdat)) #Matrix to fill in with fire data
colnames(bmat) <- c("All_burns", paste("Burned", burns[burns>1999], sep="_")) #Column names for the matrix

for (i in 1:length(fdat))
{
bmat[i,1] <- paste(brns[[i]], collapse=", ")
bmat[i,which(burns[burns>1999]%in%brns[[i]])+1] <- 1
}

###Merge data
y <- cbind(pnts, bmat, pdat)

###

write.table(y, "/Users/jasper/Dropbox/Shared/CapeCommunities/Data/LaurePrep/plot_treatments_4Aug16.txt", row.names = F)

