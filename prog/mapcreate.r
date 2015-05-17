setwd("../data")
library(rgdal)
library(sp)
library(maptools)
library(data.table)
library(plyr)
library(dplyr)

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


boe$labname <- revalue(boe$NAME, c("Greater London Authority" = "Greater London",
                       "East of England" = "East Anglia"))

#Make the NAME match with our BOE data, this is what will wil merge on
boe$NAME <- revalue(boe$NAME, c("East Midlands" = "E Midlands",
                       "East of England" = "E Anglia",
                       "Greater London Authority" = "Greater London", 
                       "North East" = "North",
                       "West Midlands" = "W Midlands",	
                       "Yorkshire and The Humber" = "Yorks&Humber"))
detach("package:plyr", unload=TRUE)


save(boe, file="map.rdata")


# for checking it's the correct region
library(leaflet)
infotip  <- paste0("Region: <b>", boe$NAME, " ", boe$labname)
#leafletR"</b><br>", boe$placenames)

leaflet(data= boe) %>% #tell where the data is
  addPolygons(color = "#BDBDC3", 
               popup = infotip)




#Next stage 
#Load the map data get percentages for the difference and then save as a spatial 
#and normal dataframe


mapmaker <- function(name,data,cluster,year, clust=2){
  data$newregion <- data[,name]
  #survey population percentages
  dat <- data.table(data)
  summarytot <- dat[,list('freq'=.N),by=list(newregion)]
  summarytot$percent <- summarytot$freq/sum(summarytot$freq)*100
  
  if (clust==2 | cluster==0){
      summaryclust2 <- dat[,list('freq'=.N),by=list(newregion,clust2)]
      summaryclust2$percent <- 0
      #Show 2-cluster percentages as default for 'All types' selection
      for (i in unique(summaryclust2$clust2)){
        summaryclust2$percent[summaryclust2$clust2==i] <- summaryclust2$freq[summaryclust2$clust2==i]/sum(summaryclust2$freq[summaryclust2$clust2==i])*100
      }
      summaryclust2$poppercent <- sapply(summaryclust2$newregion,function(x){y <- summarytot[summarytot$newregion==x,]$percent})
      summaryclust2$diffpercent <- summaryclust2$percent-summaryclust2$poppercent
      #cluster percentages
      summary <- dat[,list('freq'=.N),by=list(newregion,clust2)]
  } else if (clust==4){
      #Show 2-cluster percentages as default for 'All types' selection
      summaryclust4 <- dat[,list('freq'=.N),by=list(newregion,clust4)]
      summaryclust4$percent <- 0
      for (i in unique(summaryclust4$clust4)){
        summaryclust4$percent[summaryclust4$clust4==i] <- summaryclust4$freq[summaryclust4$clust4==i]/sum(summaryclust4$freq[summaryclust4$clust4==i])*100
      }
      summaryclust4$poppercent <- sapply(summaryclust4$newregion,function(x){y <- summarytot[summarytot$newregion==x,]$percent})
      summaryclust4$diffpercent <- summaryclust4$percent-summaryclust4$poppercent
      #cluster percentages
      summary <- dat[,list('freq'=.N),by=list(newregion,clust4)]  
        
        #add missing E Anglia in 2009 Falling behind
        if(year=='2009' & cluster==4){ 
            summary <- rbind(data.frame(summary),data.frame('newregion'='E Anglia','clust4'=4,'freq'=0))
            summary$newregion <- as.factor(summary$newregion)
        }   
  }
  summary$percent <- 0
  for (i in unique(summary$clust)){
    summary$percent[summary$clust==i] <- summary$freq[summary$clust==i]/sum(summary$freq[summary$clust==i])*100
  }
  summary$poppercent <- sapply(summary$newregion,function(x){y <- summarytot[summarytot$newregion==x,]$percent})
  summary$diffpercent <- summary$percent-summary$poppercent
  if(cluster==0)
    x <- summaryclust2[summaryclust2$clust2==1,]
  else
    x <- summary[summary$clust==cluster,]
  # merge with centroids data to check
  x$NAME <- x$newregion
  #return(x$diffpercent)
  return(x)
}


# create the var names
v09 <- paste0(rep("09", 2), "_", rep(2,2),"c",0:2)
v10 <- paste0(rep("10", 2), "_", rep(2,2),"c",0:2)
v11 <- paste0(rep("11", 2), "_", rep(2,2),"c",0:2)
v094 <- paste0(rep("09", 4), "_", rep(4,2),"c",0:4)
v104 <- paste0("10_4c0")
v114 <- paste0(rep("11", 4), "_", rep(4,2),"c",0:4)

var <- c(v09, v10, v11, v094,v104, v114)



load("map.rdata")
boe <- subda
boe$NAME <- as.character(boe$NAME)
s2009 <- read.csv("../household-typology-27April/s09.csv")
s2010 <- read.csv("../household-typology-27April/s10.csv")
s2011 <- read.csv("../household-typology-27April/s11.csv")


s2009$region <- as.character(s2009$region)
s2010$region <- as.character(s2010$region)
s2011$region <- as.character(s2011$region)
# Assign the percent diff to the data, this will need to be changed so we merge 
# on the percent diffs
for (i in 1:length(var)){
  #get the year
  tempy <- paste0("20", substring(paste0(var[i]), 1, 2))
  # get the dataframe name
  tempdat <-  paste0("s",tempy)
  # create the dataset
  cl <- as.numeric(substring(paste0(var[i]), 6,6))
  c24 <- as.numeric(substring(paste0(var[i]), 4, 4))
  
  temp <-  data.frame(mapmaker("region", 
                               data= eval(parse(text=tempdat)), cl, year= tempy,
                               clust = c24))
  # assign the variable I want
  temp1 <- temp %>% select(NAME)
  # Include an a because variables can't start with numbers
  temp1[[paste0("a",var[i])]] <- temp$diffpercent 
  boe@data <- left_join(boe@data, temp1)
}
te <- mapmaker("region", s2009, 0, "2009", clust=2)



#Simplify so it is quicker to render.
# save the data slot
subdat_data<-boe@data


# Simplify 
subdat<-rgeos::gSimplify(boe,tol=0.01, topologyPreserve=TRUE)
plot(boe)
row.names(subdat_data) <- row.names(subdat)
#to write to geojson we need a SpatialPolygonsDataFrame
subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)



### Clip the shapefile

save(subdat, file="./subpctmap.rdata")
save(boe, file="./pctmap.rdata")
library(rgdal)
library(spatstat)
library(rgeos)
plot(boe, add=T)
poly1 <- clickpoly(add=T)

coords <- poly1[[4]]

cc <- data.frame(x = coords[[1]]$x, y=coords[[1]]$y)
cc <- as.matrix(cc, ncol = 2)


polysp <- Polygon(coords = cc)
polysps <- Polygons(list(polysp), ID = "a")

poly2 <- SpatialPolygons(list(polysps), proj4string = CRS(proj4string(boe)))

p

subda <-   gIntersection(boe, poly2, byid = T)

plot(subda)

subda <- bbox(boe)

save(subda, file = "./subda.rdata")
