setwd('../household-typology-27April/')
load('map.rdata')
library(ggplot2)
library(ggvis)
library(dplyr)

boe$id <- row.names(boe)

#Create a value to represent the percentage
boe$value <- c(1,15,20,15,20,15,1,7,15,20,20)
boedf  <- fortify(boe)
boedf  <- left_join(boedf, boe@data)




####GGVIS PLOT

#Code for reactivity when hoovering
region_tooltip <- function(x){
  row <- boedf[boedf$group==x$group,]  %>%
    select(NAME, value)  %>% unique
  paste0("<b>", row[,"NAME"], "</b><br>",
         row[,"value"])
  } 


boedf %>% # this is the data it is based on
  group_by(group, id) %>%  # for this one we group by these to get the regions
  ggvis(~long, ~lat) %>% # these are the same as the aes
  layer_paths(fill = ~value, 
              strokeWidth:=0.5, stroke:="white") %>% #This give the polygons
  scale_numeric("fill", range=c("#bfd3e6", "#8c6bb1" ,"#4d004b")) %>% # choose the colour scales
  #add_tooltip(region_tooltip, "hover") %>% #This gives the interactivity switch of for faster rendering
  hide_axis("x" ) %>% hide_axis("y") %>% 
  add_legend(scales = "fill", title = "Pecentage change in value",
             orient = "left") %>% 
  layer_text(x = -2, y = 55, text := "hello", fontSize := 50) # add in text



#leaflet map
library(leaflet)

# Find colour quantiles
pal <- colorQuantile("YlGn", NULL, n = 3)

# Create html code to show when a region is clicked
infotip  <- paste0("Region: <b>", boe$NAME, "</b><br>",
       boe$value)

cl <- pal(boe$value)
vl <- boe$value


leaflet(data= boe) %>% #tell where the data is
  addPolygons(fillColor = ~pal(value), fillOpacity = 0.8, color = "#BDBDC3", 
              weight = 1, popup = infotip) %>% # draw polygons with color fill 
  addLegend(position = "bottomleft", colors = ~cl, labels = ~vl) %>% # not working yet
  setView(-2, 55, zoom = 5) %>% #tell where to start the view
  addTiles() # have a background map. Might note be needed.

