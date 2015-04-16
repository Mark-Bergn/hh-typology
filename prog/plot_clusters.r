#################
#CLUSTERING ALGOS
#################

library(cluster)
library(ggplot2)
library(grid)

#MDS on Gower dissimilarities
for (i in 2009:2011){
  #dissimilarity matrix
  temp <- daisy(eval(parse(text=paste0('q',i))))
  assign(paste0('d',i),temp)
  #MDS
  fit <- cmdscale(temp,eig=TRUE, k=2)
  assign(paste0('mds',i),fit)
  #partition around mediods 2 clusters
  clust <- pam(temp, 2)
  assign(paste0('clust',i),clust)
}

# plot solution 
x <- mds2009$points[,1]
y <- mds2009$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric  MDS", col = clust2009$clustering,cex=0.5)

#Look at how clusters are differentiated in each variable
q2009 <- cbind('clust'=clust2009$clustering, q2009)
q2010 <- cbind('clust'=clust2010$clustering, q2010)
q2011 <- cbind('clust'=clust2011$clustering, q2011)

plotter <- function(name,data){
  data$newname <- data[,name]
  newdata <- data.table(data)
  summary <- newdata[,list('freq'=.N),by=list(newname,clust)]
  summary$percentage <- 0
  for (i in unique(summary$clust)){
    summary$percentage[summary$clust==i] <- summary$freq[summary$clust==i]/sum(summary$freq[summary$clust==i])*100
  }
  h1 <- ggplot(data.frame(summary)) + 
    geom_bar(aes(x=factor(newname),y=percentage,fill=factor(clust)),stat='identity',position='dodge') + 
    geom_text(aes(x=factor(newname),y=percentage,label=round(percentage,1),group=factor(clust)),position=position_dodge(width=1),vjust=0) + 
    xlab(name) + 
    theme(axis.text.x=element_text(size=10,angle=20,vjust=0.5))
  print(h1)
}

#plot variables
plotter('xphsdf',q2009)





