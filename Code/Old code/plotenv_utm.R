library(gdata)

treat=read.xls("/Users/jasper/Dropbox/SAEON/Projects/Cape Peninsula/WFW plots/plot_treatments.xlsx", sheet=1, stringsAsFactors=F)

loc=read.xls("/Users/jasper/Documents/Data/Resampling Cape Peninsula/WFW plots - Jasper's extra/Agrohydrology.xls", sheet=1, stringsAsFactors=F)

colnames(loc)[1]="Site"

x=merge(treat,loc)

x=x[,1:7]

y=SpatialPointsDataFrame(cbind(x$LONG,x$LAT), x, proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
y=spTransform(y, CRS("+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

write.csv(as.data.frame(y)[,1:7], "/Users/jasper/Dropbox/Shared/Postfire_workshop/data/clean/alienplots.csv")
