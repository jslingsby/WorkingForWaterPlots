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
firewd <- "/Users/jasper/Documents/GIS/CapePeninsula/TMNP/1962-2016 v1"
x <- list.files(firewd, pattern = ".shp") # Get polygons by year
x <- sapply(x, function(x) {strsplit(x[1], split="\\.")[[1]][1]})
fr <- sapply(x, function(x){readOGR(firewd, layer = x, stringsAsFactors=FALSE)})
p4s <- proj4string(fr[50][[1]])
for(i in 1:length(fr)) {proj4string(fr[i][[1]]) <- p4s} #Assign the same projection

fipts<-readOGR("/Users/jasper/Documents/GIS/CapePeninsula/TMNP/Fire_Ignition_pts_July2016_v1", layer = "Fire_Ignition_pts_July2016_v1") #Get fire ignition points
proj4string(fipts) <- p4s #Assign projection

#2012
fy <- over(fr[50][[1]], fipts, returnList = TRUE) #Overlay polygons with ignition points
fyr <- lapply(fy, function(x) {x[x$Fire_Sea==2012,]})
fr[50][[1]]@data$STARTDATE <- unlist(lapply(fyr, function(x) {as.character(max(as.Date(x$Fire_Date1)))}))
fr[50][[1]]@data$YEAR <- 2012

#2013
fy <- over(fr[51][[1]], fipts, returnList = TRUE)
fyr <- lapply(fy, function(x) {x[x$Fire_Sea==2013,]})
fr[51][[1]]@data$STARTDATE <- unlist(lapply(fyr, function(x) {as.character(max(as.Date(x$Fire_Date1)))}))
fr[51][[1]]@data$STARTDATE[8] <- as.character(as.Date(fy$`7`[2,4]))
fr[51][[1]]@data$YEAR <- 2013

#2014
fy <- over(fr[52][[1]], fipts, returnList = TRUE)
fyr <- lapply(fy, function(x) {x[x$Fire_Sea==2014,]})
fyr$`5`$Fire_Date1 <- "2014/03/01"
fr[52][[1]]@data$STARTDATE <- unlist(lapply(fyr, function(x) {as.character(max(as.Date(x$Fire_Date1)))}))
fr[52][[1]]@data$YEAR <- 2014

#Fix and standardize data table names and across fire seasons
for(j in 1:length(fr))
{
x <- fr[j][[1]]@data #Get attribute table
x <- x[sort(colnames(x))] #Sort column names alphabetically
y <- matrix(NA, nrow = nrow(x), ncol=9) #Create new AT to fill
colnames(y) <- sort(c("FIREID","FIRETYPE","FIRECAUSE","YEAR","STARTDATE","XAREA","XPERIMETER","XHECTARES","ID"))
y <- as.data.frame(y)
hmm <- which(colnames(y) %in% colnames(x)) #Loop through matching columns and fill AT
for(i in 1:length(hmm))
{
y[,hmm[i]] <- x[,which(colnames(x) %in% colnames(y))[i]]
y$ID <- paste(x$YEAR[1],1:nrow(y),sep="_")
}
fr[j][[1]]@data <- y #Replace old AT with standardized AT
fr[j][[1]] <- spChFIDs(fr[j][[1]], as.character(fr[j][[1]]$ID))
}

###Bind all fires into one object
fire <- do.call(rbind, fr)

######

#fire <- readOGR(dsn="/Users/jasper/Documents/GIS/CapePeninsula/Fire/CapePeninsulaFiresMerged2015", layer="CapePeninsulaFiresMerged2015")
#fire <- spTransform(fire, CRSobj = CRS(proj4string(dat)))
#fires <- rasterize(fire, dat, field = "YEAR", fun="max")

###Alien data
#a <- readOGR("/Users/jasper/Documents/GIS/CapePeninsula/InvasionData/TMNP All Sites 2015 ID.shp", layer = "TMNP All Sites 2015 ID")
#a <- spTransform(a, CRS(proj4string(dat)))
#adat <- read.xls("/Users/jasper/Documents/GIS/CapePeninsula/InvasionData/tmnp.xlsx", sheet=1, stringsAsFactors=F)
#adat <- adat[,which(colnames(adat)%in%c("Nbalid","TreatmentHectares","Total.Density","Total_Acac","Total.Pinus","Total.Eucal","Hakea","Populus","Lepto","All.Others"))]

###Extract and arrange alien data
#nbal <- pl %over% a
# <- cbind(pl, nbal[,1:2])
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

write.table(y, "/Users/jasper/Dropbox/Shared/CapeCommunities/Data/LaurePrep/plot_treatments_6July16.txt", row.names = F)

