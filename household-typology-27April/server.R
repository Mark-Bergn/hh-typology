library(data.table)
library(gridExtra)
library(ggplot2)
library(RColorBrewer)
library(sp)
library(gpclib)
library(maptools)
gpclibPermit()

#Read data
s2009 <- read.csv('s09.csv' , fileEncoding='latin1')
s2010 <- read.csv('s10.csv' , fileEncoding='latin1')
s2011 <- read.csv('s11.csv' , fileEncoding='latin1')

#Read map and fortify
load('map.rdata')
boe <- fortify(boe, region='NAME')

#remove outlier in MDS plot
s2010 <- s2010[rownames(s2010)!=302,]

#Change order of cluster in clusternew
#clusternew <- sapply(s2009$clust4, function(x){if(x==2) y<-'3' else if(x==3) y<-'2' else y <- x})
#clusternew <- factor(as.character(clusternew))
#s2009$clust4 <- clusternew

#Read dictionary to get correct labels and colors for clusters
dict <- read.csv('clusterdict.csv')
dict$id <- c(1:nrow(dict))

#source all colour and text labels
source('labels.R')

#store all possible clusters in a vector
allclust <- c("hh09_two","hh09_four","hh10_two","hh10_four","hh11_two","hh11_four")
#store all desired variables for plotting in a vector
allvar <- c("mds1","mds2","fihhyr_a","dfihhyr_a","ustot_a",
            "xphsdf","billscc","hscntcr","xphdd6","xphdr11","xphdr6",  # 2009
            "xphsdf","billscc","uncert","saving", #2010
            "xphsdf","billscc","uncert","fisc_impact5","saving11","hscntcr1","fisc11_act3") #2011

#add in demographic vars
allvar  <- c(allvar, "age", "age_grp", "sex", "gor", "mastat", "hhsize", "nkids", "jbstat", "qual","region","tenure_grp2")

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  #get data according to year
  dat <- reactive({
    x <- eval(parse(text=paste0('s',input$year)))
  })
  
  #get cluster, there should only be one value
  #Reactive variable requires multiple processing steps, store in function(do not create function in reactive expression)
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
  
  #get data according to number of clusters chosen by user
  dataSub <- reactive({
    if(input$clustnum=='Broad')
      x <- cbind(dat()[,names(dat()) %in% allvar],'clust'=dat()[,'clust2'])
    else
      x <- cbind(dat()[,names(dat()) %in% allvar],'clust'=dat()[,'clust4'])
  })
  
  #For chloropleth map, show only 2 clusters when 'All types' is selected
  dataSubClust2 <- reactive({
      x <- cbind(dataSub(),'clust2'=dat()[,'clust2'])
  })
  
  #remove don't know/refused/not applicable observations in financial variable
  remove <- c("don't know","refused","not applicable")
  newdataSub <- reactive({
    x <- dataSub()[!(dataSub()[,input$finance] %in% remove),]
  })
  
  #determine whether to show all clusters or specific ones (depending on whtehr 'All types' or otherwise is chosen)
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
  
  percentcolourSub <- reactive({
    x <- percentcolours[[dict[dict$Year==input$year & dict$Level==input$clustnum & dict$Value==cluster(),]$id]]
  })
  
  percentlabelSub <- reactive({
    x <- percentlabels[[dict[dict$Year==input$year & dict$Level==input$clustnum & dict$Value==cluster(),]$id]]
  })
  
  
  #Histogram of income/debt
  #hack to compare histogram of cluster to entire population
  histomaker <- function(cluster,newdataSub,finance){
    histdata <- newdataSub
    histdata$histclust <- 0 #make the whole survey population one 'cluster'
    if(cluster==0){
      histdata$weight <- 1/nrow(histdata)
      histdata[,finance] <- log(as.numeric(as.character(histdata[,finance]))) #log to make dist normal-ish
      x <- histdata
    }  
    else{
      clustdata <- newdataSub[newdataSub$clust==cluster,]
      clustdata$histclust <- cluster
      histdata <- rbind(histdata, clustdata) #duplicate cluster observations
      #Get appropriate weights for each cluster so that histogram area is proportional to sample size
      histdata$scalefactor <- 1
      histdata$scalefactor[histdata$histclust==cluster] <- nrow(histdata[histdata$histclust==cluster,])/nrow(histdata[histdata$histclust!=cluster,])
      histdata$weight <- 0 #placeholder
      histdata$weight[histdata$histclust!=cluster] <- 1/(nrow(histdata[histdata$histclust!=cluster,])/histdata[histdata$histclust!=cluster,]$scalefactor)
      histdata$weight[histdata$histclust==cluster] <- 1/(nrow(histdata[histdata$histclust==cluster,])/histdata[histdata$histclust==cluster,]$scalefactor)
      # histdata$weight <- 1/nrow(histdata) # actually equivalent to setting the same weight across all clusters!
      histdata[,finance] <- log(as.numeric(as.character(histdata[,finance])))
      histdata$fmedian <- round(median(histdata[,finance]), 1)
      x <- histdata
    }
    return(x)
  }
  
  histodata <- reactive({
    x <- histomaker(cluster(),newdataSub(),input$finance)
  })
  
  #print weights
  #histweight <- reactive({
   # x <- histodata()$weight
  #})
  
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
  
  histfinancetitler <- function(finance){
    if (finance=='fihhyr_a')
      y <- 'Household distribution of income'
    else if(finance=='dfihhyr_a')
      y <- 'Household distribution of disposable income'
    else
      y <- 'Household distribution of unsecured debt'
    return(y)
  }
  
  histlabelTi <- reactive({
    x <- histfinancetitler(input$finance)
  })
  
  #Get x and y coordinates to label percentage values 
  percentx <- reactive({
    x <- min(newdataSub()$mds1) + 0.87*(max(newdataSub()$mds1)-min(newdataSub()$mds1))
  })
  
  percenty <- reactive({
    y <- min(newdataSub()$mds2) + 0.95*(max(newdataSub()$mds2)-min(newdataSub()$mds2))
  })
  
  percentcluster <- function(data,cluster){
    if (cluster==0)
      x <- 100
    else
      x <- round(nrow(data[data$clust==cluster,])/nrow(data)*100,0)
    return(x)
  }
  
  percentValue <- reactive({
    x <- percentcluster(newdataSub(),cluster())
  })
  
  #Get x and y for percent label
  percentlabelx <- reactive({
    x <- min(newdataSub()$mds1) + 0.81*(max(newdataSub()$mds1)-min(newdataSub()$mds1))
  })
  
  percentlabely <- reactive({
    y <- min(newdataSub()$mds2) + 0.86*(max(newdataSub()$mds2)-min(newdataSub()$mds2))
  })
  
  labelpercentcluster <- function(cluster,percentlabel){
    if(cluster==0)
      x <- ''
    else
      x <- paste('of the survey population \nis in',percentlabel,sep=' ')
  }
  
  percentlabelValue <- reactive({
    x <- labelpercentcluster(cluster(),percentlabelSub())
  })
  
  #mdsexplainer <- function(year){
   # if(year=='2009')
   #   x <- paste(mdsexplain1,'30',mdsexplain2,sep=' ')
    #else if(year=='2010')
    #  x <- paste(mdsexplain1,'51',mdsexplain2,sep=' ')
    #else
    #  paste(mdsexplain1,'34',mdsexplain2,sep=' ')
  #}
  
  #mdsexplain <- reactive({
   # x <- mdsexplainer(input$year)
  #})
  ##############
  #Plot clusters
  ##############
  
  #Test whether variables are as expected
  #output$clust <- renderText(y <- histweight())
  output$explain <- renderText(y <- mdsexplain())
  #output$finance <- renderText(y <- labelFin())
  
  #MDS plot
  output$mdsplot <- renderPlot({
    hh <- ggplot(data=newdataSub(), environment=environment())
    hh1 <- hh + geom_point(aes(x=newdataSub()$mds1,y=newdataSub()$mds2,
                               colour=factor(newclust(), labels=labelSub()), 
                               size=numFin()
    ),alpha=0.5,shape=19
    ) #+ coord_fixed()
    hh2 <- hh1 + geom_text(aes(x=percentx(),y=percenty(),label=paste0(percentValue(),'%')),
                           data = data.frame(), #impt! to prevent overplotting
                           size=20, colour=percentcolourSub(), alpha=0.9)
    hh3 <- hh2 + geom_text(aes(x=percentlabelx(),y=percentlabely(),label=percentlabelValue()),
                           data = data.frame(), #impt! to prevent overplotting
                           size=5, colour=percentcolourSub(), alpha=0.9, hjust=0.2)
    source('theme_mine.R')
    hh4 <- hh3 + theme_mine() + geom_hline(yintercept=0,colour="grey70") + geom_vline(xintercept=0, colour="grey70")
    hh5 <- hh4 + scale_colour_manual('name'='Household type',values=colourSub(),guide=F) + scale_size_continuous(name=labelFin(),range = c(5,25))
    hh6 <- hh5 + xlab('First partition (normalized values)') + ylab('Second partition (normalized values)') #+ ggtitle(mdstitle)
    hh7 <- hh6 + guides(size = guide_legend(label.hjust=0.5))
    print(hh7)
    
    #histogram density plot
    #make it an inset with viewport
    vp <- viewport(width=0.4,height=0.2,x=0.05,y=0.05,just=c(0,0))
    jj <- ggplot(data=histodata(), environment = environment())
    jj1 <- jj + geom_density(aes(x=histodata()[,input$finance], fill=factor(histodata()$histclust), weights=histodata()$weight),
                             colour='white',alpha=0.6)
    jj2 <- jj1 #+ geom_vline(xintercept=histodata()$fmedian,colour='black')# + annotate("text",x=unique(histodata()$fmedian),y=-0.1,label=as.character(unique(histodata()$fmedian)))
    source('theme_histogram.R')
    jj3 <- jj2 + scale_fill_manual(values=histcolourSub()) + theme_histogram() + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))
    jj4 <- jj3 + ggtitle(histlabelTi()) + xlab(histlabelFin())
    print(jj4, vp=vp)
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
  
  #For bar charts that are 'right way up'
  aggregatorup <- function(name,data,clusterset,cluster){
    data$newname <- data[,name]
    data$newclust <- clusterset
    newdata <- data.table(data)
    summary <- newdata[,list('freq'=.N),by=list(newname,newclust)]
    summary$percentage <- 0
    for (i in unique(summary$newclust)){
      summary$percentage[summary$newclust==i] <- round(summary$freq[summary$newclust==i]/sum(summary$freq[summary$newclust==i])*100,0)
    }
    if(cluster==0){
      #summary$newclust<-sapply(summary$newclust, function(y) {if(y==1) z<-8 else if(y==2) z<-7 else if(y==3) z<-6 else z<-5})
      x <- summary
    }
    
    else{
      summarytot <- data.frame(newdata[,list('freq'=.N),by=list(newname)])
      summarytot$percentage <- round(summarytot$freq/sum(summarytot$freq)*100,0)
      #label total as cluster + 10 to put it after cluster name
      total <- cluster + 10
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
  
  billscc <- reactive({
    x <- aggregatorup('billscc',newdataSub(),newclust(),cluster()) #use aggregatorup
  })
  
  #if(input$year=='2009'| input$year=='2010'){
    hscntcr1 <- reactive({
      x <- aggregator('hscntcr1',newdataSub(),newclust(),cluster())
    })
  
    hscntcr <- reactive({
    x <- aggregator('hscntcr',newdataSub(),newclust(),cluster())
    })
    
    xphdr6 <- reactive({
      x <- aggregator('xphdr6',newdataSub(),newclust(),cluster())
    })
    
    saving <- reactive({
      x <- aggregatorup('saving',newdataSub(),newclust(),cluster()) #use aggregatorup
    })
  #}
  #else{
    fisc_impact5 <- reactive({
      x <- aggregator('fisc_impact5',newdataSub(),newclust(),cluster())
    })
    
    fisc11_act3 <- reactive({
      x <- aggregator('fisc11_act3',newdataSub(),newclust(),cluster())
    })
    
    uncert <- reactive({
      x <- aggregatorup('uncert',newdataSub(),newclust(),cluster()) #use aggregatorup
    })
  #}
    
  #show table
  #output$summary <- renderTable({ y <- hscntcr()})
  
  source('theme_attitude.R')
  source('theme_attitude_upright.R')
  
  #Plot attitude questions according to year
  output$attplot <- renderPlot({

      #billscc
      bb1 <- ggplot(data=billscc(), environment=environment())
      bb2 <- bb1 + geom_bar(aes(x=billscc()$newname,y=billscc()$percentage,fill=factor(billscc()$newclust, labels=labelSub())),
                          stat='identity',position='dodge', alpha=0.6) #+ coord_flip()
      bb3 <- bb2 + geom_text(aes(x=billscc()$newname,y=billscc()$percentage,label=paste0(billscc()$percentage,'%'),group=factor(billscc()$newclust)),
                           position=position_dodge(width=1),vjust=0)
      bb4 <- bb3 + scale_fill_manual(name='Household type',values=colourSub()) + theme_attitude_upright()
      bb5 <- bb4 + scale_x_discrete(labels=billscclabels) + scale_y_continuous(limits=c(0,max(billscc()$percentage)+5)) + guides(fill = guide_legend(nrow = 2))
      bb6 <- bb5 + xlab('') + ylab('percentage (%)') + ggtitle ('How are you keeping up with your credit commitments?')
    
      #xphsdf
      pp1 <- ggplot(data=xphsdf(), environment=environment())
      pp2 <- pp1 + geom_bar(aes(x=xphsdf()$newname,y=xphsdf()$percentage,fill=factor(xphsdf()$newclust, labels=revlabelSub())),
                            stat='identity',position='dodge', alpha=0.6) + coord_flip()
      pp3 <- pp2 + geom_text(aes(x=xphsdf()$newname,y=xphsdf()$percentage,label=paste0(xphsdf()$percentage,'%'),group=factor(xphsdf()$newclust)),
                             position=position_dodge(width=1),hjust=0)
      pp4 <- pp3 + scale_fill_manual(name='Household type',values=revcolourSub(), guide=FALSE) + theme_attitude()
      pp5 <- pp4 + scale_y_continuous(limits=c(0,max(xphsdf()$percentage)+5)) #+ guides(fill = guide_legend(nrow = 1))
      pp6 <- pp5 + xlab('') + ylab('percentage (%)') + ggtitle ('Difficulties in paying for accommodation')
      
      if(input$year=='2009' | input$year=='2010'){
        #hscntcr1
        qq1 <- ggplot(data=hscntcr1(), environment=environment())
        qq2 <- qq1 + geom_bar(aes(x=hscntcr1()$newname,y=hscntcr1()$percentage,fill=factor(hscntcr1()$newclust, labels=revlabelSub())),
                              stat='identity',position='dodge', alpha=0.6) + coord_flip()
        qq3 <- qq2 + geom_text(aes(x=hscntcr1()$newname,y=hscntcr1()$percentage,label=paste0(hscntcr1()$percentage,'%'),group=factor(hscntcr1()$newclust)),
                               position=position_dodge(width=1),hjust=0)
        qq4 <- qq3 + scale_fill_manual(name='Household type',values=revcolourSub(),guide=FALSE) + theme_attitude()
        qq5 <- qq4 + scale_y_continuous(limits=c(0,max(hscntcr1()$percentage)+5)) #+ guides(fill = guide_legend(nrow = 1))
        qq6 <- qq5 + xlab('') + ylab('percentage (%)') + ggtitle ('Putting off spending because of credit concerns')
        #hscntcr
        rr1 <- ggplot(data=hscntcr(), environment=environment())
        rr2 <- rr1 + geom_bar(aes(x=hscntcr()$newname,y=hscntcr()$percentage,fill=factor(hscntcr()$newclust, labels=revlabelSub())),
                              stat='identity',position='dodge', alpha=0.6) + coord_flip()
        rr3 <- rr2 + geom_text(aes(x=hscntcr()$newname,y=hscntcr()$percentage,label=paste0(hscntcr()$percentage,'%'),group=factor(hscntcr()$newclust)),
                               position=position_dodge(width=1),hjust=0)
        rr4 <- rr3 + scale_fill_manual(name='Household type',values=revcolourSub(),guide=FALSE) + theme_attitude()
        rr5 <- rr4 + scale_y_continuous(limits=c(0,max(hscntcr()$percentage)+5)) #+ guides(fill = guide_legend(nrow = 1))
        rr6 <- rr5 + xlab('') + ylab('percentage (%)') + ggtitle ('Actual or perceived credit constraint')
        #saving
        ss1 <- ggplot(data=saving(), environment=environment())
        ss2 <- ss1 + geom_bar(aes(x=saving()$newname,y=saving()$percentage,fill=factor(saving()$newclust, labels=labelSub())),
                              stat='identity',position='dodge', alpha=0.6) #+ coord_flip()
        ss3 <- ss2 + geom_text(aes(x=saving()$newname,y=saving()$percentage,label=paste0(saving()$percentage,'%'),group=factor(saving()$newclust)),
                               position=position_dodge(width=1),vjust=0)
        ss4 <- ss3 + scale_fill_manual(name='Household type',values=colourSub(),guide=FALSE) + theme_attitude_upright()
        ss5 <- ss4 + scale_x_discrete(labels=savinglabels) + scale_y_continuous(limits=c(0,max(saving()$percentage)+5)) #+ guides(fill = guide_legend(nrow = 1))
        ss6 <- ss5 + xlab('') + ylab('percentage (%)') + ggtitle ('Planning to increase the amount of savings')
      }
      else{
        #fisc_impact5
        qq1 <- ggplot(data=fisc_impact5(), environment=environment())
        qq2 <- qq1 + geom_bar(aes(x=fisc_impact5()$newname,y=fisc_impact5()$percentage,fill=factor(fisc_impact5()$newclust, labels=revlabelSub())),
                              stat='identity',position='dodge', alpha=0.6) + coord_flip()
        qq3 <- qq2 + geom_text(aes(x=fisc_impact5()$newname,y=fisc_impact5()$percentage,label=paste0(fisc_impact5()$percentage,'%'),group=factor(fisc_impact5()$newclust)),
                               position=position_dodge(width=1),hjust=0)
        qq4 <- qq3 + scale_fill_manual(name='Household type',values=revcolourSub(), guide=FALSE) + theme_attitude()
        qq5 <- qq4 + scale_y_continuous(limits=c(0,max(fisc_impact5()$percentage)+5)) #+ guides(fill = guide_legend(nrow = 1))
        qq6 <- qq5 + xlab('') + ylab('percentage (%)') + ggtitle ('Perceived higher taxes due to govt budget measures')
        
        #fisc11_act3
        rr1 <- ggplot(data=fisc11_act3(), environment=environment())
        rr2 <- rr1 + geom_bar(aes(x=fisc11_act3()$newname,y=fisc11_act3()$percentage,fill=factor(fisc11_act3()$newclust, labels=revlabelSub())),
                              stat='identity',position='dodge', alpha=0.6) + coord_flip()
        rr3 <- rr2 + geom_text(aes(x=fisc11_act3()$newname,y=fisc11_act3()$percentage,label=paste0(fisc11_act3()$percentage,'%'),group=factor(fisc11_act3()$newclust)),
                               position=position_dodge(width=1),hjust=0)
        rr4 <- rr3 + scale_fill_manual(name='Household type',values=revcolourSub(), guide=FALSE) + theme_attitude()
        rr5 <- rr4 + scale_y_continuous(limits=c(0,max(fisc11_act3()$percentage)+5)) #+ guides(fill = guide_legend(nrow = 1))
        rr6 <- rr5 + xlab('') + ylab('percentage (%)') + ggtitle ('Responded to govt budget measures by looking for a new job')
        
        #uncert
        ss1 <- ggplot(data=uncert(), environment=environment())
        ss2 <- ss1 + geom_bar(aes(x=uncert()$newname,y=uncert()$percentage,fill=factor(uncert()$newclust, labels=labelSub())),
                              stat='identity',position='dodge', alpha=0.6) #+ coord_flip()
        ss3 <- ss2 + geom_text(aes(x=uncert()$newname,y=uncert()$percentage,label=paste0(uncert()$percentage,'%'),group=factor(uncert()$newclust)),
                               position=position_dodge(width=1),vjust=0)
        ss4 <- ss3 + scale_fill_manual(name='Household type',values=colourSub(),guide=FALSE) + theme_attitude_upright()
        ss5 <- ss4 + scale_y_continuous(limits=c(0,max(uncert()$percentage)+5))#+ scale_x_discrete(labels=savinglabels) #+ guides(fill = guide_legend(nrow = 1))
        ss6 <- ss5 + xlab('') + ylab('percentage (%)') + ggtitle ('Certainty in household income a year from now')
      }
      
      allplot <- grid.arrange(bb6,pp6,qq6,rr6,ss6,
                                ncol=1,
                                heights=c(1.8,1,1,1,1.8))
      print(allplot)
  })
  
  ###########################
  #########Plot Map##########
  ###########################
  
  mapmaker <- function(name,data,cluster){
    data$newregion <- data[,name]
    #survey population percentages
    dat <- data.table(data)
    summarytot <- dat[,list('freq'=.N),by=list(newregion)]
    summarytot$percent <- summarytot$freq/sum(summarytot$freq)*100
    #always include 2-cluster percentages
    summaryclust2 <- dat[,list('freq'=.N),by=list(newregion,clust2)]
    summaryclust2$percent <- 0
    for (i in unique(summaryclust2$clust2)){
      summaryclust2$percent[summaryclust2$clust2==i] <- summaryclust2$freq[summaryclust2$clust2==i]/sum(summaryclust2$freq[summaryclust2$clust2==i])*100
    }
    summaryclust2$poppercent <- sapply(summaryclust2$newregion,function(x){y <- summarytot[summarytot$newregion==x,]$percent})
    summaryclust2$diffpercent <- summaryclust2$percent-summaryclust2$poppercent
    #cluster percentages
    summary <- dat[,list('freq'=.N),by=list(newregion,clust)]
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
    return(x)
  }
  
  diffmap <- reactive({
    x <- mapmaker('region',dataSubClust2(),cluster())
  })
  
  #check table
  #output$table <- renderTable({
   # y <- diffmap()
  #})
  mapcolorSub <- reactive({
    x <- mapcolorshigh[[dict[dict$Year==input$year & dict$Level==input$clustnum & dict$Value==cluster(),]$id]]
  })
  
  source('theme_map.R')
  output$mapplot  <- renderPlot({
    mm <- ggplot(data=diffmap(), environment=environment()) 
    mm1 <- mm + geom_map(aes(map_id=diffmap()$newregion,fill=diffmap()$diffpercent),map=boe) + expand_limits(x = boe$long, y = boe$lat)
    if(cluster()==0)
      mm2 <- mm1 +  scale_fill_gradient2("Percentage excess of\nSecure households\ncompared to survey population",high='#238443',low='#D7301F',mid='#FFFFBF',na.value = "black")
    else
      mm2 <- mm1 + scale_fill_gradient(name=paste0("Percentage excess of\n",percentlabelSub()," households\ncompared to survey population"),high=mapcolorSub(),low='#FFFFBF',limits=c(0,max(diffmap()$diffpercent)))
    mm3 <- mm2 + theme_map() #+ coord_fixed(ratio = 3) 
    mm4 <- mm3 #+ scale_x_continuous(limits=c(-10,2)) 
    print(mm4)
    
  })
  
  ############
  #BAR CHARTS
  ############
  demclust <- reactive({
    x <- clusterfactor(cluster(),dataSub())
  })
  
  ###trying to remov non-applicables but haven't managed so far
  #   remove <- c("don't know","refused","not applicable")
    #   nkidsdataSub <- reactive({
    #     x <- dataSub()[!(dataSub()[,"nkids"] %in% remove),]
    #   })
    #   
    #   hhsdataSub <- reactive({
    #     x <- dataSub()[!(dataSub()[,"hhsize"] %in% remove),]
    #   })
  
  ##normal
  plot1 <- reactive({
    if(input$demog==1){
      x <- aggregator('age_grp', dataSub(), demclust(), cluster())

    }else{
      x <- aggregator('hhsize', dataSub(),demclust(),cluster())
    }
  })
  # 'nkids'
  
  plot1lab <- reactive({
    if(input$demog==1){
      lab1  <- 'Age Groups (years)'
    }else{
      lab1  <- 'Household Size'
    }
  })
  
  ##flipped
  plot2 <- reactive({
    if(input$demog==1){
      x <- aggregator('sex',dataSub(),demclust(),cluster())
    }else{ ##having an issue when using tenure
      x <- aggregator('tenure_grp2',dataSub(),demclust(),cluster())
    }
  })
  #'tenure'
  
  
  #flipped
  plot2lab <- reactive({
    if(input$demog==1){
      lab2  <- 'Gender'
    }else{
      lab2  <- 'Housing Situation'
    }
  })
  
  ##normal
  plot3 <- reactive({
    if(input$demog==1){
      x <- aggregator('qual',dataSub(),demclust(),cluster())
    }else{
      x <- aggregator('nkids',dataSub(),demclust(),cluster())
    }
  })
  #'hhsize'
  
  plot3lab <- reactive({
    if(input$demog==1){
      lab3  <- 'Qualifications'
    }else{
      lab3  <- 'Number of Kids'
    }
  })
  
  ##flipped
  plot4 <- reactive({
    if(input$demog==1){
      x <- aggregator('jbstat',dataSub(),demclust(),cluster())
    }else{
      x <- aggregator('mastat',dataSub(),demclust(),cluster())
    }
  })
  #'mastat'
  plot4lab <- reactive({
    if(input$demog==1){
      lab4  <- 'Job Status'
    }else{
      lab4  <- 'Married Status'
    }
  })
  
  
  
  output$demplot  <- renderPlot({
    
    ag1 <- ggplot(data=plot1(), environment=environment())
    ag2 <- ag1 + geom_bar(aes(x=plot1()$newname,y=plot1()$percentage,fill=factor(plot1()$newclust, labels=labelSub())),
                          stat='identity',position='dodge', alpha=0.6) #+ coord_flip()
    ag3 <- ag2 + geom_text(aes(x=plot1()$newname,y=plot1()$percentage,label=paste0(plot1()$percentage,'%'),group=factor(plot1()$newclust)),
                           position=position_dodge(width=1),vjust=0)
    ag4 <- ag3 + scale_fill_manual(name='Household type',values=colourSub(), guide=FALSE) + theme_attitude_upright()
    ag5 <- ag4 + scale_y_continuous(limits=c(0,max(plot1()$percentage)+5)) #+ guides(fill = guide_legend(nrow = 2))
    ag6 <- ag5 + xlab('') + ylab('percentage (%)') + ggtitle (plot1lab())
    
    se1 <- ggplot(data=plot2(), environment=environment())
    se2 <- se1 + geom_bar(aes(x=plot2()$newname,y=plot2()$percentage,fill=factor(plot2()$newclust, labels=revlabelSub())),
                          stat='identity',position='dodge', alpha=0.6) + coord_flip()
    se3 <- se2 + geom_text(aes(x=plot2()$newname,y=plot2()$percentage,label=paste0(plot2()$percentage,'%'),group=factor(plot2()$newclust)),
                           position=position_dodge(width=1),hjust=0)
    se4 <- se3 + scale_fill_manual(name='Household type',values=revcolourSub()) + theme_attitude()
    se5 <- se4 + guides(fill = guide_legend(nrow = 2)) + scale_y_continuous(limits=c(0,max(plot2()$percentage)+5)) 
    se6 <- se5 + xlab('') + ylab('percentage (%)') + ggtitle (plot2lab())
    
    
    
    qu1 <- ggplot(data=plot3(), environment=environment())
    qu2 <- qu1 + geom_bar(aes(x=plot3()$newname,y=plot3()$percentage,fill=factor(plot3()$newclust, labels=labelSub())),
                          stat='identity',position='dodge', alpha=0.6) #+ coord_flip()
    qu3 <- qu2 + geom_text(aes(x=plot3()$newname,y=plot3()$percentage,label=paste0(plot3()$percentage,'%'),group=factor(plot3()$newclust)),
                           position=position_dodge(width=1),vjust=0)
    qu4 <- qu3 + scale_fill_manual(name='Household type',values=colourSub(), guide=FALSE) + theme_attitude_upright()
    qu5 <- qu4 + scale_y_continuous(limits=c(0,max(plot3()$percentage)+5)) #+ guides(fill = guide_(nrow = 2))
    qu6 <- qu5 + xlab('') + ylab('percentage (%)') + ggtitle (plot3lab())
    
    
    jb1 <- ggplot(data=plot4(), environment=environment())
    jb2 <- jb1 + geom_bar(aes(x=plot4()$newname,y=plot4()$percentage,fill=factor(plot4()$newclust, labels=revlabelSub())),
                          stat='identity',position='dodge', alpha=0.6) + coord_flip()
    jb3 <- jb2 + geom_text(aes(x=plot4()$newname,y=plot4()$percentage,label=paste0(plot4()$percentage,'%'),group=factor(plot4()$newclust)),
                           position=position_dodge(width=1),hjust=0)
    jb4 <- jb3 + scale_fill_manual(name='Household type',values=revcolourSub(), guide=FALSE) + theme_attitude()
    jb5 <- jb4 + scale_y_continuous(limits=c(0,max(plot4()$percentage)+5)) #+ guides(fill = guide_legend(nrow = 1))
    jb6 <- jb5 + xlab('') + ylab('percentage (%)') + ggtitle (plot4lab())
    
    
    
    alldemplot  <- grid.arrange(se6, ag6, jb6, qu6,
                                ncol=1,
                                heights=c(2.5, 2.5, 2.5, 2.5) )
    
    print(alldemplot)
  })
  
})


