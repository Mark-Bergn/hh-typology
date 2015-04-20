setwd('../data')

#Packages
library(data.table)
library(dplyr)
library(ggplot2)


#Bring in everything that is from 2004 through to 2014
#We have f2f until 2011 and online from 2011 onwards
# in 2011 we have both
for (i in 2004:2014){
  
  if (i < 2011) {
    assign(paste0("f", i), fread(paste0(i, "f2f.csv")))
    
  } else if(i==2011){
    assign(paste0("f", i), fread(paste0(i, "f2f.csv")))
    assign(paste0("o", i), fread(paste0(i, "online.csv")))
    
  } else if(i > 2011){
    assign(paste0("o", i), fread(paste0(i, "online.csv")))
  }
}

#Check that 04 through to 11 have the same variables
all.equal(names(f2004), 
          names(f2005),
          names(f2006),
          names(f2007),
          names(f2008),
          names(f2009),
          names(f2010),
          names(f2011)
          )


#Combine all the f2f datasets
f2f  <- rbind_all(list(f2004, f2005,
                       f2006, f2007, f2008, f2009, f2010, f2011))

#Find all the midway numeric variables
grep("_a$", names(f2f))
li  <- names(f2f[(grep("_a$", names(f2f)))])

#Any other numberic variables add here
li  <- c(li, "age", "hhsize", "nkids")

#Convert the midway variables into numeric format for clustering
f2f[li]  <- sapply(f2f[li], as.numeric)

#save
f2fall  <- f2f
save(f2fall, file="f2fall.r")

###Now drop pre 2007 data
f2f  <- filter(f2f, year >= 2007)

save(f2f, file ="f2f.r")

#Save on memory
#rm(list=ls())
load("f2fall.r")
load("f2f.r")

######################
#FACE-TO-FACE DATASETS
######################

#focus on 2009-2011 (with financial attitude questions related to the recession)

#attitude variables + tenure group
att <- c('tenure_grp3',
         'huresp',
         'xphsdf',
         'xphpdf',
         'billscc',
         'hscntcr1',
         'hscntcr2',
         'hscntcr',
         'hscrchg',
         'saving',
         'spend',
         'debtconc',
         'chdebtconc',
         'uncert',
         'uncertch',
         'fisc_conc',
         'forebeareffsec',
         'forebeareffunsec')

#expression for enumerated binary variables
enum <- 'xphdr|xphdd|savebcs|desave11bcs|debtconc_act|fisc_conc|fisc_act|fisc_impact|fisc11_act|fisc11_conc|fisc_likeact|forebearsec|forebearunsec'

#2009-2011
for (i in 2009:2011){
  temp <- data.frame(eval(parse(text=paste0('f',i))))
  a <- names(temp)
  b <- a[grepl(enum,a)==T]
  assign(paste0('n',i), temp[,names(temp) %in% c(att,b)])
  h1 <- sprintf('No. of attitude variables in %d: %d',i,ncol(eval(parse(text=paste0('n',i)))))
  print(h1)
}

#check that all years have the same variables 
sum((names(n2009)==names(n2010))==F)
sum((names(n2009)==names(n2011))==F)
sum((names(n2010)==names(n2011))==F)

#Discard NA variables for each year

for (i in 2009:2011){
  temp <- eval(parse(text=paste0('n',i)))
  keep <- c()
  for(name in names(temp)){
    if(sum(is.na(temp[,name]))!=nrow(temp))
    keep <- c(keep,name)
  }
  temp <- temp[,keep]
  assign(paste0('m',i),temp)
  h1 <- sprintf('No. of non-NA variables in %d: %d', i,length(names(temp)))
  print(h1)
}

#Check proportion. of uninformative responses per variable (don't know/refused)
for (i in 2009:2011){
  temp <- eval(parse(text=paste0('m',i)))
  set <- c()
  for(name in names(temp)){
    uninf <- round(sum(temp[,name] %in% c("don't know","refused"))/nrow(temp)*100,2)
    set <- c(set,uninf)
    h1 <- sprintf("Year %d, %s: %.2f%% don't know/refused",i,name,uninf)
    print(h1)
  }
  h2 <- sprintf("Median percentage don't know/refused: %.2f%%", median(set))
  print(h2)
}

#Check proportion of blank responses per variable
for (i in 2009:2011){
  temp <- eval(parse(text=paste0('m',i)))
  set <- c()
  for(name in names(temp)){
    uninf <- round(sum(temp[,name]=="")/nrow(temp)*100,2)
    set <- c(set,uninf)
    h1 <- sprintf("Year %d, %s: %.2f%% blank",i,name,uninf)
    print(h1)
  }
  h2 <- sprintf("Median percentage blank: %.2f%%", median(set))
  print(h2)
}

#process variables
#make copies of datasets
for (i in 2009:2011){
  temp <- eval(parse(text=paste0('m',i)))
  assign(paste0('p',i),temp)
}

#convert 'not applicable' to 'no' in xphdr_,xphdd_
for (i in 2009:2011){
  temp <- eval(parse(text=paste0('p',i)))
  a <- names(temp)[grepl('xphdr|xphdd',names(temp))==T]
  for (name in a){
    na <- round(sum(temp[,name]=='not applicable')/nrow(temp)*100,2)
    h1 <- sprintf('Year %d, percentage not applicable in %s: %.2f%%',i,name,na)
    print(h1)
    temp[,name] <- sapply(temp[,name],function(x){if(x=='not applicable') y <- 'no' else y <- x})
    no <- round(sum(temp[,name]=='no')/nrow(temp)*100,2)
    h2 <- sprintf('Year %d, percentage no in %s: %.2f%%',i,name,no)
    print(h2)
  }
  #remove xphdd12-14 (mostly blanks)
  temp <- temp[,!(names(temp) %in% c('xphdd12','xphdd13','xphdd14'))]
  assign(paste0('p',i),temp)
}

#remove 'not applicable' observations in hscntcr
for (i in 2009:2011){
  temp <- eval(parse(text=paste0('p',i)))
  temp <- temp[temp$hscntcr!='not applicable',]
  assign(paste0('p',i),temp)
  h1 <- sprintf('Year %d, no. of obs left after removing not applicable in hscntcr: %d',i,nrow(temp))
  print(h1)
}

#remove 'not applicable' observations in hscrchg
for (i in 2009:2011){
  temp <- eval(parse(text=paste0('p',i)))
  temp <- temp[temp$hscrchg!='not applicable',]
  assign(paste0('p',i),temp)
  h1 <- sprintf('Year %d, no. of obs left after removing not applicable in hscrchg: %d',i,nrow(temp))
  print(h1)
}

#remove p2010 blank observations in debtconc, debtconc_act, chdebtconc
#temp <- p2010
#debt <- names(temp)[grepl('debtconc|debtconc_act|chdebtconc',names(temp))==T]
#for (name in debt){
 # temp <- temp[temp[,name]!='',]
#}
#sprintf('Year 2010, no. of obs left after removing blanks in debtconc: %d', nrow(temp))
#assign('p2010',temp)

#convert p2011 blank responses in fisc11_act to 'no'
temp <- p2011
fisc <- names(temp)[grepl('fisc11_act',names(temp))==T]
for(name in fisc){
  na <- round(sum(temp[,name]=='')/nrow(temp)*100,2)
  h1 <- sprintf('Year %d, percentage blank in %s: %.2f%%',i,name,na)
  print(h1)
  temp[,name] <- sapply(temp[,name], function(x){if(x=='') y <- 'no' else y <- x})
  nb <- round(sum(temp[,name]=='no')/nrow(temp)*100,2)
  h2 <- sprintf('Year %d, percentage no in %s: %.2f%%',i,name,nb)
  print(h2)
}
assign('p2011',temp)

#convert p2011 blank observations in forebearunsec_ to no
temp <- p2011
runsec <- names(temp)[grepl('forebearunsec',names(temp))==T]
for (name in runsec){
  na <- round(sum(temp[,name]=='')/nrow(temp)*100,2)
  h1 <- sprintf('Year 2011, percentage blank in %s: %.2f%%',name,na)
  print(h1)
  temp[,name] <- sapply(temp[,name],function(x){if(x=='') y <- 'no' else y <- x})
  nb <- round(sum(temp[,name]=='no')/nrow(temp)*100,2)
  h2 <- sprintf('Year 2011, percentage no in %s: %.2f%%',name,nb)
  print(h2)
}
assign('p2011',temp)

#final set of variables
attf <- c('xphsdf',
          'billscc',
          'hscntcr1',
          'hscntcr2',
          'hscntcr',
          'hscrchg',
          'saving',
          'saving11',
          #'debtconc',
          #'chdebtconc',
          'uncert',
          'uncertch')

#enumf <- 'xphdr|xphdd|debtconc_act|fisc_conc|fisc_act|fisc_impact|fisc11_act|fisc11_impact'
enumf <- 'xphdr|xphdd|fisc_conc|fisc_act|fisc_impact|fisc11_act|fisc11_impact'

for (i in 2009:2011){
  temp <- data.frame(eval(parse(text=paste0('p',i))))
  a <- names(temp)
  b <- a[grepl(enumf,a)==T]
  assign(paste0('q',i), temp[,names(temp) %in% c(attf,b)])
  h1 <- sprintf('No. of final attitude variables in %d: %d',i,ncol(eval(parse(text=paste0('q',i)))))
  print(h1)
}

#remove observations if any don't know/refused

for (i in 2009:2011){
  temp <- data.frame(eval(parse(text=paste0('q',i))))
  flag <- apply(temp,1,function(x){if(sum(x=="don't know" | x=="refused")>0) y <- 1 else y <- 0})
  temp <- temp[flag==0,]
  for (name in names(temp)){
    temp[,name] <- as.factor(temp[,name])
  }
  assign(paste0('q',i), temp)
  h1 <- sprintf("Year %d, no. of obs left after removing any don't know/refused: %d",i,nrow(temp))
  print(h1)
}

  
#################
#ONLINE DATASETS
#################

#pure balance sheet variables
pbs <- c('fihhyr2','dfihhyr','saveamount','nvesttot','hsval','mg1tot','ustot')
unsec <- c('us','usa','usb','usc','usd','use','usg','ush','usi','usj')

#classes of tenure
for (i in 2011:2014){
  temp <- data.frame(eval(parse(text=paste0('o',i))))
  h1 <- sprintf('tenure %d',i)
  print(h1)
  print(summary(factor(temp$tenure)))
}
mort <- c('Owned mortgage','Owned outright')
rent <- c('Local authority rented','Private rented')

#mortgagers/homeowners vs. renters
mpbs <- c()
rpbs <- c()
for (i in 2011:2014){
  stri <- as.character(i)
  temp <- data.frame(eval(parse(text=paste0('o',i))))
  if(i<2014){
    mpbs[[stri]] <- temp[temp$tenure %in% mort,names(temp) %in% c(pbs,unsec,'tenure')]
    rpbs[[stri]] <- temp[temp$tenure %in% rent,names(temp) %in% c(pbs,unsec,'tenure')]
  }
  else{
    pbsm <- paste(pbs,'m',sep='_')
    mpbs[[stri]] <- temp[temp$tenure %in% mort,names(temp) %in% c(pbsm,unsec)]
    rpbs[[stri]] <- temp[temp$tenure %in% rent,names(temp) %in% c(pbsm,unsec)]
  }
  h1 <- length(names(mpbs[[stri]]))
  h2 <- sprintf('No.of pbs variables in %d: %d',i,h1)
  print(h2)
  h3 <- nrow(mpbs[[stri]])
  h4 <- sprintf('No.of mortgage/owner households in %d: %d',i,h3)
  print(h4)
  h5 <- nrow(rpbs[[stri]])
  h6 <- sprintf('No.of renter households in %d: %d',i,h5)
  print(h6)
}

#convert to ordinal ranks: load dictionaries for every variable 
#Look at variable factors, compare to data descriptions and amend as necessary
for (name in names(mpbs[['2011']])){
  print(name)
  temp <- gsub('\243','GBP',mpbs[['2011']][,name])
  h1 <- data.frame(sort(unique(factor(temp))))
  print(h1)
}

#housevalue/mortgage ratio (not meaningful to use mgtot1 directly)
for (i in 2011:2014){
  if(i<2014){
    stri <- as.character(i)
    aa <- mpbs[[stri]]$mg1tot
    bb <- mpbs[[stri]]$hsval
  }
  else{
    stri <- as.character(i)
    aa <- mpbs[[stri]]$mg1tot_m
    bb <- mpbs[[stri]]$hsval_m
  }
  aa[aa %in% c("don't know","refused","not applicable")] <- NA
  aa <- gsub('\243|,|>=|<','',aa)
  aa <- sapply(aa,function(x) {
    median(as.numeric(unlist(strsplit(x,'-'))),na.rm=T)
  })
  mpbs[[stri]]$mgtot1new <- aa
  
  bb[bb %in% c("don't know","refused","not applicable")] <- NA
  bb <- gsub('\243|,|>=|<','',bb)
  bb <- sapply(bb,function(x) {
    median(as.numeric(unlist(strsplit(x,'-'))),na.rm=T)
  })
  mpbs[[stri]]$hsvalnew <- bb
  mpbs[[stri]]$hsmgdiff <- bb-aa
  
}


