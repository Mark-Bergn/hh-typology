setwd("./data")
setwd("./projects/hh-typology/data")
#rm(list=setdiff(ls(), "boe"))
load("boe.rdata")

levels(boe$NAME)  <-  c("E Anglia", "E Midlands", "Greater London", "North",
                        "North West", "South East", "South West", "W Midlands", "Yorks&Humber",
                        "Scotland", "Wales" )

plot(boe)
plot(boe[boe$NAME == "E Midlands", ], col="blue", add=T) #E Anglia
plot(boe[boe$NAME == "E Anglia", ], col="blue", add=T) # E Midlands
plot(boe[boe$NAME == "Greater London", ], col="blue", add=T) # Fine
plot(boe[boe$NAME == "North", ], col="blue", add=T) #fine
plot(boe[boe$NAME == "North West", ], col="blue", add=T) # Fine
plot(boe[boe$NAME == "Scotland", ], col="blue", add=T) ## Wrong probably South East
plot(boe[boe$NAME == "South East", ], col="blue", add=T) # is South West
plot(boe[boe$NAME == "South West", ], col="blue", add=T) # W Midlands
plot(boe[boe$NAME == "W Midlands", ], col="blue", add=T) # Yorks and Humber 
plot(boe[boe$NAME == "Wales", ], col="blue", add=T) # Scotland
plot(boe[boe$NAME == "Yorks&Humber", ], col="blue", add=T) # Wales


#Simplify as it doesn't need to be so detailed. 

library(ggplot2)
library(dplyr)
library(sp)
library(rgeos)
library(ggvis)


#label centers
region_centers <- boe %>%
  gCentroid(byid=TRUE) %>%
  data.frame %>%
  cbind(NAME=boe$NAME %>% gsub(" County, ME", "", .) )


region_centers$cent_x  <- region_centers$x
region_centers$cent_y  <- region_centers$y
region_centers$x  <- NULL
region_centers$y  <- NULL

levels(regions_centers$NAME)  <-  c("East Anglia", "East Midlands", "London", "North East",
                        "North West", "South East", "South West", "West Midlands", "Yorkshire & Humber",
                        "Scotland", "Wales" )

# save the data slot
subdat_data<-boe@data

# Simplify 
subdat<-gSimplify(boe,tol=0.001, topologyPreserve=TRUE)

#to write to geojson we need a SpatialPolygonsDataFrame
subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)

boe  <- subdat

save(boe, file="../household-typology-27April/map.rdata")
save(region_centers, file="../household-typology-27April/mapcenters.rdata")

#collapse the s2009 through to s2011 by cluster for each gor



# #create a dataframe
# boedf  <-  fortify(boe)
# boe$id <- row.names(boe) # allocate an id variable to the sp data
# boedf <- left_join(boedf, boe@data) # join the data
# 
# 
# boedf  <- left_join(x=boedf, y=x1, by=c("NAME"="newregion"))
# 
# ?left_join
# #quick way
# spplot(boe, "value1")
# 
# 
# ##GGplot2
# ggplot(data = boedf, # the input data
#        aes(x = long, y = lat, fill = diffpercent, group = group )) + # define variables
#   geom_polygon() + # plot the regions
#   geom_path(colour="black", lwd=0.05) + # region borders
#   coord_equal() + # fixed x and y scales as equal
#   # facet_wrap(~ date) + # one plot per time slice if we want to add i year
#   scale_fill_gradient2(low = "green", mid = "grey", high = "red", # colors
#                        midpoint = 0, name = "Whatever Value") + # legend options
#   theme(axis.text = element_blank(), # change the theme options
#         axis.title = element_blank(), # remove axis titles
#         axis.ticks = element_blank()) + # remove axis ticks
#   geom_text(region_centers$x, region_centers$y, label=region_centers$name )
# 
# 
# #GGVIS
# boedf %>%
#   ggvis(~long, ~lat) %>%
#   group_by(group, id) %>%
#   layer_paths(strokeOpacity:=0.5, stroke:="#7f7f7f") %>%
#   hide_legend("fill") %>%
#   hide_axis("x") %>% hide_axis("y") %>%
#   set_options(width=700, height=600, keep_aspect=TRUE)
# 
# 
# 
# 
# 
# ?gsub
# boedf %>%
#   group_by(group, id) %>%
#   ggvis(~long, ~lat) %>%
#   layer_paths(strokeWidth:=0.25, stroke:="#7f7f7f") %>%
#   layer_points(data=region_centers, x=~x, y=~y, size:=8) %>%
#   layer_text(data=region_centers,
#              x=~x+0.05, y=~y, text:=~name,
#              baseline:="middle", fontSize:=8) %>%
#   hide_legend("fill") %>%
#   hide_axis("x") %>% hide_axis("y") %>%
#   set_options(width=400, height=600, keep_aspect=TRUE)
# 
# 
# ###with popup
# 
# 
# 
# ##choloropleth map 
# 
# #close buyt not quite there need to change the text that comes up
# 
# boeval  <- boedf %>%
#   group_by(id) %>%
#     summarise(value1 = min(value1), value2 = min(value2) ,
#             value3 = min(value3), value4 = min(value4), value5 = min(value5))
# 
# 
# 
# boe_value <- function(x) {
#   if(is.null(x)) return(NULL)
#   y <- boedf %>% filter(id==x$id) %>% select(id,12:16)
#   sprintf("<table width='100%%'>%s</table>",
#           paste0("<tr><td style='text-align:left'>", names(y),
#                  ":</td><td style='text-align:right'>", format(y), collapse="</td></tr>"))
# }
# 
# boe_value(boedf)
# 
# # this is for the tooltip. it does a lookup into the crime data frame and
# # then uses those values for the popup
# 
# 
# boedf %>%
#   group_by(group, id) %>%
#   ggvis(~long, ~lat) %>%
#   layer_paths(fill=input_select(label="Category:",
#                                 choices=boeval %>%
#                                   select(starts_with("value")) %>%
#                                   colnames %>% sort,
#                                 id="id",
#                                 map=as.name),
#               strokeWidth:=0.5, stroke:="white") %>%
#   scale_numeric("fill", range=c("#bfd3e6", "#8c6bb1" ,"#4d004b")) %>%
#   add_tooltip(boe_value, "hover") %>%
#   add_legend("fill", title="value for whatever") %>%
#   hide_axis("x") %>% hide_axis("y") %>%
#   set_options(width=400, height=600, keep_aspect=TRUE)


