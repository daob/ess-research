require(QCA) # for prettyString string wrap function

rm(list=ls())

experiments.vector <- c("women","job","efficacy","doctors")

for (ie in 1:length(experiments.vector)) {
  
  experiments <- experiments.vector[ie]
  print(experiments)

  thresholds <- read.table(paste("~/current-project/data/",experiments,"/thresholds.txt",sep=""), header=T)
  means <- read.table(paste("~/current-project/data/",experiments,"/means.txt",sep=""), header=T)
  questions <- read.table(paste("~/current-project/data/",experiments,"/questions.txt",sep=""), header=T, as.is=c(1,2,3))
  scales <- read.table(paste("~/current-project/data/",experiments,"/scales.txt",sep=""), header=T, as.is=c(1,2,3))

  stopifnot(length(unique(means$trait)) == length(unique(thresholds$trait)))
  stopifnot(length(unique(means$method)) == length(unique(thresholds$method)))

  traits <- unique(means$trait)
  methods <- unique(means$method)
  countries <- unique(means$country)

  numtraits <-  length(traits)
  nummethods <- length(methods)
  numcountries <- length(countries)

  numcategories <- 4
  digits <- 1
  iquestions <- 1
  numexperiments <- 1

  tau.mat <- vector()

  for ( it in 1:numtraits ) {
    for( im in 1:nummethods ) {
      quest <- questions[questions$trait==traits[it] & questions$method==methods[im] , 1]
      for( ic in 1:numcountries) {
        tau <- thresholds[thresholds$country==countries[ic] &  thresholds$trait==traits[it] &  thresholds$method==methods[im] ,1]
        nu <- means[means$country==countries[ic] &  means$trait==traits[it] &  means$method==methods[im], 1]
        tau <- tau - nu
        numtau <- length(tau)
        tau.mat <- rbind(tau.mat, c(tau,countries[ic],traits[it],methods[im]))
      }  
    }
  }

  pdf(paste("~/current-project/output/thresholds_",experiments,".pdf",sep=""),
      height=nummethods*2.5, width=numtraits*4.2 )

  par(mfcol=c(nummethods,numtraits))
  par(mar=c(6,2,2,2))
  zlim <- c(-3,3) #min and max z-scores to draw. default=3 sd's

  for(it in 1:numtraits) {
    for(im in 1:nummethods) {

      print(paste("it:",it,"im:",im))
      print(numtraits*(im-1)+it)    
      
      ref.y <- 0:(numcountries-1) #  y posititions at which to draw the reference lines
      string.vector <- unlist(strsplit(questions[numtraits*(im-1)+it,1], " "))
      title.text <- prettyString(string.vector, 60, 0, " ")
      
      plot(0,0, xlim=zlim,
           ylim=c(-.1, ref.y[length(ref.y)]+.30),
           main=title.text,
           xlab="z-score",
           ylab="Country",
           yaxt='n', pch=NA,
           frame.plot=F,
           font.main=3,
           cex.main=1,
           )
      
      axis(2, ref.y,labels=as.character(countries))
      
                                        #draw reference lines
      lapply(ref.y, function(x){ lines(zlim, c(x,x)) } )

      tau.cur <- tau.mat[
                         tau.mat[,numtau+2]==it & tau.mat[,numtau+3] == im,
                         1:numtau
                         ][1,]
      segments( tau.cur, rep(+.1, numtau), tau.cur, rep(-.1, numtau) )
      text(tau.cur, rep(+.25, numtau),
           labels=as.character(round(tau.cur,1)),
           cex=.8, adj=c(.5,1) )

      
      tau.cur <- tau.mat[
                         tau.mat[,numtau+2]==it & tau.mat[,numtau+3] == im,
                         1:numtau
                         ][2,]
      segments( tau.cur, rep(1.1, numtau), tau.cur, rep(1-.1, numtau) )
      text(tau.cur, rep(1.25, numtau),
           labels=as.character(round(tau.cur,1)),
           cex=.8, adj=c(.5,1) )    
    }
  }                                        

  dev.off()

}# loop to next experiment

                                        # men gebruikt dat deel van de schaal waar men het mee eens is
                                        # 'negatieve' vraagstelling beinvloedt de overgangen
                                        # verschillende normen bijv. gelijkheid mannen en vrouwen zorgen voor verschillen in
                                        # categorieen op verschillende manieren gebruikt
                                        #   1 door verschillen in hun ware positie zie boven
                                        #   2 door verchillen in de positie van de vraag (in dat land)
                                        # dit veroorzaakt verschillen in de true score door methoden
                                        # dus na correctie veranderen methoden effecten
                                        # kan beide kanten op
