setwd('../data')

library(rgdal)
library(sp)
library(maptools)

#read in Shape file of English Regions
regions  <- readOGR(dsn=".", layer="Regions")

##Uncomment for checking
#plot(regions)
#head(regions@data)
#table(regions$NAME)

##get scotland and wales
load('GBR_adm1.RData')

##remove England and Ireland
gadmsw  <- gadm[gadm$NAME_1 %in% c("Scotland", "Wales"), ]

##check
#plot(gadmsw) 

#reproject data so they match
proj4string(gadm)
proj4string(regions)

regwgs84  <- spTransform(regions, CRSobj = CRS(proj4string(gadmsw)))
gadmsw  <- spTransform(gadmsw, CRSobj = CRS(proj4string(gadmsw)))
proj4string(regwgs84)
proj4string(gadmsw)


##Check they match
#plot(gadmsw)
#plot(regwgs84, add = T)


##Combine using spRbind

names(regwgs84)
names(gadmsw)


#Rename some vars and reduce to only them
gadmsw$NAME  <- as.factor(gadmsw$NAME_1)
gadmsw$AREA_CODE  <- as.factor(gadmsw$ID_1)
gadmsw$POLYGON_ID  <- as.numeric(gadmsw$PID)


gadmsw@data[,c("NAME", "AREA_CODE", "POLYGON_ID")]
gadmsw@data  <- gadmsw@data[,c("NAME", "AREA_CODE", "POLYGON_ID")]
regwgs84@data  <- regwgs84@data[,c("NAME", "AREA_CODE", "POLYGON_ID")]


#It won;'t bind due to duplicate of an arbritary id for the polygons this function makes them shape fiel
# specific
#To make them uniform we can write a function using the spChFIDs function from sp:
makeUniform<-function(SPDF){
  pref<-substitute(SPDF)  #just putting the file name in front.
  newSPDF<-spChFIDs(SPDF,as.character(paste(pref,rownames(as(SPDF,"data.frame")),sep="_")))
  return(newSPDF)
}

regwgs84  <- makeUniform(regwgs84)
gadmsw  <- makeUniform(gadmsw)

#Now bind
boe  <- spRbind(regwgs84, gadmsw)

levels(boe$NAME)

#Make the NAME match with our BOE data, this is what will wil merge on
levels(boe$NAME)  <- c("E Midlands", "E Anglia", "Greater London", 
                       "North",	"North West", "South East",
                       "South West",	"W Midlands",	"Yorks&Humber", "Scotland",  "Wales")

#plot(boe)



save(boe, file="boe.rdata")
