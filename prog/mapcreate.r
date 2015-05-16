
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

boe$NAME

#Make the NAME match with our BOE data, this is what will wil merge on
levels(boe$NAME)  <- c("E Midlands", "E Anglia", "Greater London", 
                       "North",	"North West", "South East",
                       "South West",	"W Midlands",	
                       "Yorks&Humber", "Scotland",  "Wales")

#Add in a placenames file I want all the 
boe$placenames  <- c("East Midlands", "East Anglia",  "London", 
                     "North East", "North West", "South East",
                 "South West", "West Midlands" ,
                 "Yorkshire & Humber", "Scotland", "Wales")



infotip  <- paste0("Region: <b>", boe$NAME, "</b><br>", boe$placenames)

leaflet(data= boe) %>% #tell where the data is
  addPolygons(color = "#BDBDC3", 
               popup = infotip)




c("North East", )

[1] "North"          "North West"     "Greater London" "W Midlands"    
[5] "Yorks&Humber"   "South West"     "E Midlands"     "South East"    
[9] "E Anglia"       "Scotland"       "Wales"



mapmaker <- function(name,data,cluster,year){
  data$newregion <- data[,name]
  #survey population percentages
  dat <- data.table(data)
  summarytot <- dat[,list('freq'=.N),by=list(newregion)]
  summarytot$percent <- summarytot$freq/sum(summarytot$freq)*100
  #Show 2-cluster percentages as default for 'All types' selection
  summaryclust2 <- dat[,list('freq'=.N),by=list(newregion,clust2)]
  summaryclust2$percent <- 0
  for (i in unique(summaryclust2$clust2)){
    summaryclust2$percent[summaryclust2$clust2==i] <- summaryclust2$freq[summaryclust2$clust2==i]/sum(summaryclust2$freq[summaryclust2$clust2==i])*100
  }
  summaryclust2$poppercent <- sapply(summaryclust2$newregion,function(x){y <- summarytot[summarytot$newregion==x,]$percent})
  summaryclust2$diffpercent <- summaryclust2$percent-summaryclust2$poppercent
  #cluster percentages
  summary <- dat[,list('freq'=.N),by=list(newregion,clust2)]
  #add missing E Anglia in 2009 Falling behind
  if(year=='2009' & cluster==4){ 
    summary <- rbind(data.frame(summary),data.frame('newregion'='E Anglia','clust'=4,'freq'=0))
    summary$newregion <- as.factor(summary$newregion)
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


vars <- c(rep"09_2c1", "09_2c2", "09_4c1" )

v09 <- paste0(rep("09", 2), "_", rep(2,2),"c",1:2)
v10 <- paste0(rep("10", 2), "_", rep(2,2),"c",1:2)
v11 <- paste0(rep("11", 2), "_", rep(2,2),"c",1:2)
v094 <- paste0(rep("09", 4), "_", rep(4,2),"c",1:4)
v114 <- paste0(rep("11", 4), "_", rep(4,2),"c",1:4)

var <- c(v09, v10, v11, v094, v114)

# Assign the percent diff to the data, this will need to be changed so we merge 
# on the percent diffs
var
for (i in 1:length(var)){
  #get the year
  tempy <- paste0("20", substring(paste0(var[i]), 1, 2))
  # get the dataframe name
  tempdat <-  paste0("s",tempy)
  # create the dataset
  cl <- substring(paste0(var[i]), 6,6)
  temp <-  data.frame(mapmaker("region", data= eval(parse(text=tempdat)), cl, year ))
  # assign the variable I want
  temp1 <- temp %>% select(NAME)
  temp1[[paste0(var[i])]] <- temp$diffpercent 
  boe@data <- left_join(boe@data, temp1)
}





#Simplify so it is quicker to render.
# save the data slot
subdat_data<-boe@data

# Simplify 
subdat<-gSimplify(boe,tol=0.001, topologyPreserve=TRUE)

#to write to geojson we need a SpatialPolygonsDataFrame
subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)

boe  <- subdat

save(boe, file="../household-typology-27April/map.rdata")


