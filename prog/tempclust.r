library(dplyr)
  load("f2f.r")



clust  <- f2f  %>% 
  filter(year == 2007) %>%
  select(id, dfihhyr_a, dfihhyrchg_a, ustotdy_a, 
         xpus_a, mg1tot_a, mg1totdy_a,
        xpmg_a)

hist(clust$mg1totdy_a)

summary(clust)

clustall  <-  clust[complete.cases(clust),]
d  <- dist(clustall[ ,-1])



?cmdscale
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric  MDS",	type="n")
text(x, y, labels = clustall$id, cex=.7)


?princomp
pcomp  <- princomp(clustall[ ,-1])
summary(pcomp)
plot(pcomp)

library(MASS)
fit <- isoMDS(d, k=2)

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric  MDS",  type="n")
text(x, y, labels = clustall$id, cex=.7)


library(Amelia)
?amelia
missmapp(amelia(clust))
?missmapp
