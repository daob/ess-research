rm(list=ls())

# Define the different criteria for misspecification:
alpha     <- .01 # Type I error level
effsize   <- .10 # Effect size which interests us
highpower <- .80 # What constitutes "high power"?

critical  <- qchisq( 1-alpha, 1 ) # Critical value of chisquare with 1 df

 mi.dat <- read.table("C:\\ESS\\berlin\\output\\women-MI", header=T)
 mi.dat$ncp <-  ( mi.dat$mi / mi.dat$stdXYEPC^2 ) * effsize^2
 mi.dat$CNTRY <- c(rep(1,24),rep(2,24),rep(3,24),rep(4,24))
 mi.dat$power <- round( 1 - pchisq( critical, 1, ncp=mi.dat$ncp), 2 )

 attach(mi.dat)

 stem(power)
 stem(mi)

# pdf("~/current-project/output/pics/power.pdf")

 #initially plot only the lower-than-effect size points as circles:
 plot( power[stdXYEPC < effsize],
      mi[stdXYEPC < effsize],
      xlab="Power", ylab="Modification Index (MI)")
 abline( critical, 0 ) # line indicating critical value of the Chi-square
 lines( c( highpower, highpower ), c(0,80) ) # line indicating "high power"
 # triangles indicate STDXYEPC >= the searched-for effect size:
 points(power[stdXYEPC >= effsize], mi[stdXYEPC >= effsize], pch=24 )

# dev.off()


identify(power,mi, labels=paste(param,CNTRY), plot=T )

misspecified <- ( mi >= critical & power < highpower) | (mi >= critical & power >= highpower & stdXYEPC >= effsize )
table(misspecified, mi.dat$CNTRY)
paste( mi.dat$param[misspecified], mi.dat$CNTRY[misspecified] )

detach(mi.dat)
