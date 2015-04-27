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
  clust <- pam(temp, 4)
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
plotter('fisc_impact1',q2011)

#Change order of clusters to make it more intuitive


clusternew <- sapply(clust2009$clustering, function(x){if(x==1) y<-'2' else if(x==2) y<-'1' else y <- x})
clusternew <- factor(as.character(clusternew))
q2009$clust <- clusternew

clusternew <- sapply(clust2011$clustering, function(x){if(x==3) y<-'2' else if(x==2) y<-'3' else y <- x})
clusternew <- factor(as.character(clusternew))
q2011$clust <- clusternew


#ggplot for clusters
library(devtools)
source_url("https://raw.github.com/low-decarie/FAAV/master/r/stat-ellipse.R")

clusterplot <- function(year){
  mds <- eval(parse(text=paste0('mds',year)))
  cluster <- eval(parse(text=paste0('q',year,'$clust')))
  dat <- data.frame(cbind('C1'=mds$points[,1],'C2'=mds$points[,2],'Cluster'=cluster))
  hh <- ggplot(dat) + geom_point(aes(x=C1,y=C2,colour=factor(Cluster)))
  hh1 <- hh + stat_ellipse(aes(x=C1,y=C2,fill=factor(Cluster)),
                           geom="polygon", level=0.95, alpha=0.2)
  hh1 <- hh1 + theme(panel.background=element_rect(fill='white',colour=NA),
                     panel.grid.major  = element_blank(),
                     panel.grid.minor  = element_blank(),
                     axis.text.x	= element_blank(),
                     axis.text.y  = element_blank(),
                     axis.ticks  = element_blank()) + xlab('Coordinate 1') + ylab('Coordinate 2') + ggtitle(paste('Metric MDS',year))
  print(hh1)
}

#plots made which were saved to the wiki
clusterplot('2009')
clusterplot('2011')

plotter('xphsdf',q2009)




#new variable: sum of xphdd_
xphddsum <- apply(q2011[,grepl('xphdd',names(q2011))==T & names(q2011)!='xphdd9'],1,function(x){sum(x=='yes')})
q2011$xphddsum <- xphddsum

#Multiple Correspondence Analysis (MCA)
library(FactoMineR)
mca3 <- MCA(q2011[,nonfisc])

nondebt <- names(q2010)[grepl('fisc|debt|clust',names(q2010))==F]
d2010 <- daisy(q2010[,nondebt])
c2010 <- cmdscale(d2010,eig=T,k=2)
clust2010 <- pam(d2010,3)

nonfisc <- names(q2011)[grepl('fisc_impact|clust',names(q2011))==F]
d2011 <- daisy(q2011[,nonfisc])
c2011 <- cmdscale(d2011,eig=T,k=2)
clust2011 <- pam(d2011,4)

x <- c2010$points[,1]
y <- c2010$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric  MDS", col = clust2010$clustering,cex=0.5)



