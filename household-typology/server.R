library(data.table)
library(gridExtra)
library(ggplot2)
library(RColorBrewer)

#Read data
s2009 <- read.csv('s2009.csv')
s2010 <- read.csv('s2010.csv')
s2011 <- read.csv('s2011.csv')

#remove outlier in MDS plot
s2010 <- s2010[rownames(s2010)!=302,]

#Read dictionary to get correct labels and colors for clusters
dict <- read.csv('clusterdict.csv')
dict$id <- c(1:nrow(dict))

#Labels
all_labels<- list(c('Secure','Insecure'),
               c('Secure','Insecure'),
               c('Insecure','Secure'),
               c('Cluster 1','Cluster 2','Cluster 3','Cluster 4'),
               c('Cluster 1','Survey population'),
               c('Cluster 2','Survey population'),
               c('Cluster 3','Survey population'),
               c('Cluster 4','Survey population'),
               c('Secure','Insecure'),
               c('Secure','Insecure'),
               c('Insecure','Secure'),
               c('Secure 1','Secure 2','Insecure 1','Insecure 2'),
               c('Secure','Insecure'),
               c('Secure','Insecure'),
               c('Insecure','Secure'),
               c('Highly Secure','Secure but worried','Struggling to keep up','Falling behind'),
               c('Highly Secure','Survey population'),
               c('Secure but worried','Survey population'),
               c('Struggling to keep up','Survey population'),
               c('Falling behind','Survey population')
)
               
allcolours<- list(c('#88419D','#FEB24C'),
                  c('#88419D','#969696'),
                  c('#FEB24C','#969696'),
                  c('#810F7C','#8C6BB1','#FED976','#FD8D3C'),
                  c('#810F7C','#969696'),
                  c('#8C6BB1','#969696'),
                  c('#FED976','#969696'),
                  c('#FD8D3C','#969696'),
                  c('#88419D','#FEB24C'),
                  c('#88419D','#969696'),
                  c('#FEB24C','#969696'),
                  c('#810F7C','#8C6BB1','#FED976','#FD8D3C'),
                  c('#88419D','#FEB24C'),
                  c('#88419D','#969696'),
                  c('#FEB24C','#969696'),
                  c('#810F7C','#8C6BB1','#FED976','#FD8D3C'),
                  c('#810F7C','#969696'),
                  c('#8C6BB1','#969696'),
                  c('#FED976','#969696'),
                  c('#FD8D3C','#969696')
                  )

#reverse label and colour order when using coord_flip() 
revlabels<- list(c('Insecure','Secure'),
                  c('Insecure','Secure'),
                  c('Secure','Insecure'),
                  c('Cluster 4','Cluster 3','Cluster 2','Cluster 1'),
                  c('Survey population','Cluster 1'),
                  c('Survey population','Cluster 2'),
                  c('Survey population','Cluster 3'),
                  c('Survey population','Cluster 4'),
                  c('Insecure','Secure'),
                  c('Secure','Insecure'),
                  c('Secure','Insecure'),
                  c('Insecure 2','Insecure 1','Secure 2','Secure 1'),
                  c('Insecure','Secure'),
                  c('Insecure','Secure'),
                  c('Secure','Insecure'),
                  c('Falling behind','Struggling to keep up','Secure but worried','Highly Secure'),
                  c('Survey population','Highly Secure'),
                  c('Survey population','Secure but worried'),
                  c('Survey population','Struggling to keep up'),
                  c('Survey population','Falling behind')
)

revcolours<- list(c('#FEB24C','#88419D'),
                  c('#969696','#88419D'),
                  c('#969696','#FEB24C'),
                  c('#FD8D3C','#FED976','#8C6BB1','#810F7C'),
                  c('#969696','#810F7C'),
                  c('#969696','#8C6BB1'),
                  c('#969696','#FED976'),
                  c('#969696','#FD8D3C'),
                  c('#FEB24C','#88419D'),
                  c('#969696','#88419D'),
                  c('#969696','#FEB24C'),
                  c('#FD8D3C','#FED976','#8C6BB1','#810F7C'),
                  c('#FEB24C','#88419D'),
                  c('#969696','#88419D'),
                  c('#969696','#FEB24C'),
                  c('#FD8D3C','#FED976','#8C6BB1','#810F7C'),
                  c('#969696','#810F7C'),
                  c('#969696','#8C6BB1'),
                  c('#969696','#FED976'),
                  c('#969696','#FD8D3C')
)

histcolours<- list('#969696',
                  c('#969696','#88419D'),
                  c('#969696','#FEB24C'),
                  '#969696',
                  c('#969696','#810F7C'),
                  c('#969696','#8C6BB1'),
                  c('#969696','#FED976'),
                  c('#969696','#FD8D3C'),
                  '#969696',
                  c('#969696','#88419D'),
                  c('#969696','#FEB24C'),
                  '#969696',
                  '#969696',
                  c('#969696','#88419D'),
                  c('#969696','#FEB24C'),
                  '#969696',
                  c('#969696','#810F7C'),
                  c('#969696','#8C6BB1'),
                  c('#969696','#FED976'),
                  c('#969696','#FD8D3C')
)

#store all possible clusters in a vector
allclust <- c("hh09_two","hh09_four","hh10_two","hh10_four","hh11_two","hh11_four")
#store all desired variables for plotting in a vector
allvar <- c("mds1","mds2","fihhyr_a","dfihhyr_a","ustot_a","xphsdf","billscc","uncert","fisc_impact5","saving11","saving","hscntcr1")

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  #get data according to year
  dat <- reactive({
    x <- eval(parse(text=paste0('s',input$year)))
  })
  
  #get cluster, there should only be one value
  #Reactive variable requires multiple processing steps, store in function(do not apply function in reactive expression)
  clusterfinder <- function(input){
    if(input$year=='2009' & input$clustnum=='Broad')
      y <- input$hh09_two
    else if (input$year=='2009' & input$clustnum=='Detailed')
      y <- input$hh09_four
    else if (input$year=='2010' & input$clustnum=='Broad')
      y <- input$hh10_two
    else if (input$year=='2010' & input$clustnum=='Detailed')
      y <- 0
    else if (input$year=='2011' & input$clustnum=='Broad')
      y <- input$hh11_two
    else
      y <- input$hh11_four
    return(y)
  } 
 
  cluster <- reactive({
    x <- as.numeric(clusterfinder(input))
  })
  
  #get data according to number of clusters
  dataSub <- reactive({
    if(input$clustnum=='Broad')
      x <- cbind(dat()[,allvar],'clust'=dat()[,'clust2'])
    else
      x <- cbind(dat()[,allvar],'clust'=dat()[,'clust4'])
  })
  
  #remove don't know/refused/not applicable observations in financial variable
  remove <- c("don't know","refused","not applicable")
  newdataSub <- reactive({
    x <- dataSub()[!(dataSub()[,input$finance] %in% remove),]
  })
  
  #determine whether to show all clusters or specific ones
  clusterfactor <- function(cluster,data){
    #cluster <- as.numeric(cluster)
    if(cluster==0)
      x <- data[,'clust']
    else
      x <- sapply(data[,'clust'], function(y){if(y==cluster) z <- cluster else z <- cluster + 1})
    return(x)
  }
  
  newclust <- reactive({
    x <- clusterfactor(cluster(),newdataSub())
  })
  
  #get correct cluster labels and colours
  labelSub <- reactive({
    x <- all_labels[[dict[dict$Year==input$year & dict$Level==input$clustnum & dict$Value==cluster(),]$id]]
  })
  
  colourSub <- reactive({
    x <- allcolours[[dict[dict$Year==input$year & dict$Level==input$clustnum & dict$Value==cluster(),]$id]]
  })
  
  revlabelSub <- reactive({
    x <- revlabels[[dict[dict$Year==input$year & dict$Level==input$clustnum & dict$Value==cluster(),]$id]]
  })
  
  revcolourSub <- reactive({
    x <- revcolours[[dict[dict$Year==input$year & dict$Level==input$clustnum & dict$Value==cluster(),]$id]]
  })
  
  histcolourSub <- reactive({
    x <- histcolours[[dict[dict$Year==input$year & dict$Level==input$clustnum & dict$Value==cluster(),]$id]]
  })
  
  #Histogram of income/debt
  #hack to compare histogram of cluster to entire population
  histomaker <- function(cluster,newdataSub,finance){
    histdata <- newdataSub
    histdata$histclust <- 0 #make the whole survey population one 'cluster'
    if(cluster==0){
      histdata[,finance] <- log(as.numeric(as.character(histdata[,finance])))
      x <- histdata
    }  
    else{
      clustdata <- newdataSub[newdataSub$clust==cluster,]
      clustdata$histclust <- cluster
      histdata <- rbind(histdata, clustdata) #duplicate cluster observations
      histdata[,finance] <- log(as.numeric(as.character(histdata[,finance])))
      x <- histdata
    }
    return(x)
  }
  
  histodata <- reactive({
    x <- histomaker(cluster(),newdataSub(),input$finance)
  })
  
  #Turn financial variable numeric
  numFin <- reactive({
    x <- as.numeric(as.character(newdataSub()[,input$finance]))
  })
  
  #histnumFin <- reactive({
   # x <- as.numeric(as.character(histodata()[,input$finance]))
  #})
  
  #Financial label
  financelabeller <- function(finance){
    if (finance=='fihhyr_a')
      y <- 'Income in GBP'
    else if(finance=='dfihhyr_a')
      y <- 'Disposable income in GBP'
    else
      y <- 'Total unsecured debt in GBP'
    return(y)
  }
  
  labelFin <- reactive({
    x <- financelabeller(input$finance)
  })
  
  #Financial label for histogram
  histfinancelabeller <- function(finance){
    if (finance=='fihhyr_a')
      y <- 'Income score'
    else if(finance=='dfihhyr_a')
      y <- 'Disposable income score'
    else
      y <- 'Unsecured debt score'
    return(y)
  }
  
  histlabelFin <- reactive({
    x <- histfinancelabeller(input$finance)
  })
  
  
  
  ##############
  #Plot clusters
  ##############
  
  #Test whether variables are as expected
  output$clust <- renderText(y <- histcolourSub())
  #output$total <- renderText(y <- )
  #output$finance <- renderText(y <- labelFin())
  
  #MDS plot
  output$mdsplot <- renderPlot({
    hh <- ggplot(data=newdataSub(), environment=environment())
    hh1 <- hh + geom_point(aes(x=newdataSub()$mds1,y=newdataSub()$mds2,
                               colour=factor(newclust(), labels=labelSub()), 
                               size=numFin()
    ),alpha=0.5
    )
    source('theme_mine.R')
    hh2 <- hh1 + theme_mine()
    hh3 <- hh2 + scale_colour_manual('name'='Household type',values=colourSub()) + scale_size_continuous(name=labelFin(),range = c(5,25))
    hh4 <- hh3 + xlab('Dimension 1') + ylab('Dimension 2')
    print(hh4)
    
    #histogram density plot
    #make it an inset
    vp <- viewport(width=0.4,height=0.2,x=0.05,y=0.05,just=c(0,0))
    jj <- ggplot(data=histodata(), environment = environment())
    jj1 <- jj + geom_density(aes(x=histodata()[,input$finance], fill=factor(histodata()$histclust)),
                             colour='white',alpha=0.6)
    source('theme_histogram.R')
    jj2 <- jj1 + scale_fill_manual(values=histcolourSub()) + theme_histogram() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))
    jj3 <- jj2 + ggtitle('Relative frequency of households') + xlab(histlabelFin())
    print(jj3, vp=vp)
  })
  
  
  #Attitude questions
  #Split responses fourways or twoways only if 'All Types' is selected. Else compare to total survey population
  aggregator <- function(name,data,clusterset,cluster){
      data$newname <- data[,name]
      data$newclust <- clusterset
      newdata <- data.table(data)
      summary <- newdata[,list('freq'=.N),by=list(newname,newclust)]
      summary$percentage <- 0
      for (i in unique(summary$newclust)){
        summary$percentage[summary$newclust==i] <- round(summary$freq[summary$newclust==i]/sum(summary$freq[summary$newclust==i])*100,0)
      }
      if(cluster==0){
        summary$newclust<-sapply(summary$newclust, function(y) {if(y==1) z<-8 else if(y==2) z<-7 else if(y==3) z<-6 else z<-5})
        x <- summary
      }
        
      else{
        summarytot <- data.frame(newdata[,list('freq'=.N),by=list(newname)])
        summarytot$percentage <- round(summarytot$freq/sum(summarytot$freq)*100,0)
        #label total as cluster - 10 to put it after cluster name
        total <- cluster - 10
        summarytot2 <- cbind('newname'=summarytot$newname,
                             data.frame('newclust'=rep(total,nrow(summarytot))),
                            'freq'=summarytot$freq,
                            'percentage'=summarytot$percentage)
        summaryfinal <- rbind(summary,summarytot2)
        x <- summaryfinal[summaryfinal$newclust %in% c(cluster,total),]
        #x <- summaryfinal
      }
      return(x)
  }
  
  xphsdf <- reactive({
    x <- aggregator('xphsdf',newdataSub(),newclust(),cluster())
  })
  
  #show table
  output$summary <- renderTable({ y <- xphsdf()})
  #Plot attitude questions
  output$attplot <- renderPlot({
    pp1 <- ggplot(data=xphsdf(), environment=environment())
    pp2 <- pp1 + geom_bar(aes(x=xphsdf()$newname,y=xphsdf()$percentage,fill=factor(xphsdf()$newclust, labels=revlabelSub())),
                          stat='identity',position='dodge', alpha=0.6) + coord_flip()
    pp3 <- pp2 + geom_text(aes(x=xphsdf()$newname,y=xphsdf()$percentage,label=paste0(xphsdf()$percentage,'%'),group=factor(xphsdf()$newclust)),
                           position=position_dodge(width=1),hjust=0)
    source('theme_attitude.R')
    pp4 <- pp3 + scale_fill_manual(name='Household type',values=revcolourSub()) + theme_attitude()
    pp5 <- pp4 + scale_y_continuous(limits=c(0,105)) + guides(fill = guide_legend(nrow = 1))
    pp6 <- pp5 + xlab('') + ylab('percentage (%)') + ggtitle ('Difficulties paying for accomodation in the past year')
    print(pp6)
  })
  
})
  

