setwd("../data")
setwd("./projects/hh-typology/data")
#rm(list=setdiff(ls(), "boe"))
load("boe.rdata")

#Simplify as it doesn't need to be so detailed. 

library(ggplot2)
library(dplyr)
library(sp)
library(rgeos)
library(ggvis)


# save the data slot
subdat_data<-boe@data

# Simplify 
subdat<-gSimplify(boe,tol=0.03, topologyPreserve=TRUE)

#to write to geojson we need a SpatialPolygonsDataFrame
subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)

boe  <- subdat
plot(boe)

save(boe, file="../household-typology-27April/map.r")

#collapse the s2009 through to s2011 by cluster for each gor

s2009_gor4  <- s2009 %>%
  group_by(region, clust4) %>%
  summarise(n = n())  

s2009_gor1  <- s2009 %>%
  group_by(region) %>%
  summarise(n = n(), clust4=0)  



s2009_gor  <- full_join(s2009_gor1, s2009_gor4) 

boe@data  <- left_join(boe@data, s2009_gor, by = c("NAME", "region"))
?left_join


#create a dataframe
boedf  <-  fortify(boe)
boe$id <- row.names(boe) # allocate an id variable to the sp data
boedf <- left_join(boedf, boe@data) # join the data

#label centers
region_centers <- boe %>%
  gCentroid(byid=TRUE) %>%
  data.frame %>%
  cbind(name=boe$NAME %>% gsub(" County, ME", "", .) )



#quick way
spplot(boe, "value1")


##GGplot2
ggplot(data = boedf, # the input data
       aes(x = long, y = lat, fill = value, group = group)) + # define variables
  geom_polygon() + # plot the regions
  geom_path(colour="black", lwd=0.05) + # region borders
  coord_equal() + # fixed x and y scales as equal
  # facet_wrap(~ date) + # one plot per time slice if we want to add i year
  scale_fill_gradient2(low = "green", mid = "grey", high = "red", # colors
                       midpoint = 150, name = "Whatever Value") + # legend options
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks



#GGVIS
boedf %>%
  ggvis(~long, ~lat) %>%
  group_by(group, id) %>%
  layer_paths(strokeOpacity:=0.5, stroke:="#7f7f7f") %>%
  hide_legend("fill") %>%
  hide_axis("x") %>% hide_axis("y") %>%
  set_options(width=700, height=600, keep_aspect=TRUE)





?gsub
boedf %>%
  group_by(group, id) %>%
  ggvis(~long, ~lat) %>%
  layer_paths(strokeWidth:=0.25, stroke:="#7f7f7f") %>%
  layer_points(data=region_centers, x=~x, y=~y, size:=8) %>%
  layer_text(data=region_centers,
             x=~x+0.05, y=~y, text:=~name,
             baseline:="middle", fontSize:=8) %>%
  hide_legend("fill") %>%
  hide_axis("x") %>% hide_axis("y") %>%
  set_options(width=400, height=600, keep_aspect=TRUE)


###with popup



##choloropleth map 

#close buyt not quite there need to change the text that comes up

boeval  <- boedf %>%
  group_by(id) %>%
    summarise(value1 = min(value1), value2 = min(value2) ,
            value3 = min(value3), value4 = min(value4), value5 = min(value5))



boe_value <- function(x) {
  if(is.null(x)) return(NULL)
  y <- boedf %>% filter(id==x$id) %>% select(id,12:16)
  sprintf("<table width='100%%'>%s</table>",
          paste0("<tr><td style='text-align:left'>", names(y),
                 ":</td><td style='text-align:right'>", format(y), collapse="</td></tr>"))
}

boe_value(boedf)

# this is for the tooltip. it does a lookup into the crime data frame and
# then uses those values for the popup


boedf %>%
  group_by(group, id) %>%
  ggvis(~long, ~lat) %>%
  layer_paths(fill=input_select(label="Category:",
                                choices=boeval %>%
                                  select(starts_with("value")) %>%
                                  colnames %>% sort,
                                id="id",
                                map=as.name),
              strokeWidth:=0.5, stroke:="white") %>%
  scale_numeric("fill", range=c("#bfd3e6", "#8c6bb1" ,"#4d004b")) %>%
  add_tooltip(boe_value, "hover") %>%
  add_legend("fill", title="value for whatever") %>%
  hide_axis("x") %>% hide_axis("y") %>%
  set_options(width=400, height=600, keep_aspect=TRUE)


