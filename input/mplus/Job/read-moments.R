
rm(list=ls())

dat <- read.table("C:\\ESS\\berlin\\input\\mplus\\Job\\covariances-results.txt", header=F)
dat <- dat[,1:2]

outpath <- "C:\\ESS\\berlin\\data\\"
name <- "job"

countries <- c('BE','SI')
numvars <- 6
numpercountry <- ( numvars*(numvars+1)/2 ) + numvars

covars <- list()
means <- list()
for(cntry in 1:length(countries)) {
  covars[[cntry]] <- matrix( rep(NA,numvars^2), numvars, numvars )
  diag( covars[[cntry]] ) <- dat[1:numvars,2]
  start <- 7
  i <- 1

  for(i in 1:(numvars-1)) {
    covars[[cntry]][(i+1):numvars,i]<- dat[ start:(start+(numvars-i-1)), 2 ]
    start <- start+numvars-i
  }
  means[[cntry]] <- dat[ start:(start+(numvars-1)), 2]
  write.table(covars[[cntry]], paste(outpath,name,'_',countries[cntry],'.cov',sep=""),row.names=F,col.names=F,na="" )
    write.table(means[[cntry]], paste(outpath,name,'_',countries[cntry],'.mean',sep=""),row.names=F,col.names=F,na="" )
  start <- start+(numvars-1)
  dat <- dat[(numpercountry+1):NROW(dat),]
}
