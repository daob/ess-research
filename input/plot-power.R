rm(list=ls())

# Define the different criteria for misspecification:
alpha     <- .01 # Type I error level
effsize   <- .10 # Effect size which interests us
highpower <- .80 # What constitutes "high power"?

critical  <- qchisq( 1-alpha, 1 ) # Critical value of chisquare with 1 df

countries <- c("CH","CZ","GR","SI")

# mi.dat <- read.table("~/current-project/output/mi_CZ.txt", header=T)
 mi.dat <- read.table("~/current-project/output/women-sb2-mi", header=T)
 mi.dat$ncp <-  ( mi.dat$mi / mi.dat$stdXYEPC^2 ) * effsize^2

 beg <- c( grep("WORK\\$1",param), length(param)+1 )
 tmp <- vector()
 for ( i in 1:(length(beg)-1) ) {
   tmp <- c( tmp, rep( countries[i], beg[i+1]-beg[i] ) )
 }

 mi.dat$CNTRY <- tmp
 mi.dat$power <- round( 1 - pchisq( critical, 1, ncp=mi.dat$ncp), 2 )

 attach(mi.dat)

 stem(power)
 stem(mi)

#pdf("~/current-project/output/pics/power-sb2.pdf")

 #initially plot only the lower-than-effect size points as circles:
 plot( power[abs(stdXYEPC) < effsize],
      mi[abs(stdXYEPC) < effsize],
      xlab="Power", ylab="Modification Index (MI)",
      xlim=c(0,max(power)),ylim=c(0,max(mi)) )
 abline( critical, 0 ) # line indicating critical value of the Chi-square
 lines( c( highpower, highpower ), c(0,80) ) # line indicating "high power"
 # triangles indicate STDXYEPC >= the searched-for effect size:
 points(power[abs(stdXYEPC) >= effsize], mi[abs(stdXYEPC) >= effsize], pch=24 )
# dev.off()


# identify(power,mi, labels=paste(object,verb,subject), plot=T, cex=.5 )
identified <- identify(power,mi, labels=paste(CNTRY,":",param,sep=""), plot=T, cex=.7 )

misspecified <- ( mi >= critical & power < highpower) | (mi >= critical & power >= highpower & abs(stdXYEPC) >= effsize )
table(misspecified, mi.dat$CNTRY)
paste( mi.dat$param[misspecified], mi.dat$CNTRY[misspecified] )

detach(mi.dat)

