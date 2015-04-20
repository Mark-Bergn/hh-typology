#rm(list=setdiff(ls(), "boe"))
load("boe.rdata")
boe$value  <- as.numeric(boe$NAME)*10

library(ggplot2)
library(dplyr)
library(sp)
#quick way
spplot(boe, "value")

#with ggplot
boedf  <-  fortify(boe)
boe$id <- row.names(boe) # allocate an id variable to the sp data
boedf <- left_join(boedf, boe@data) # join the data


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
