pdf("C:/ESS/berlin/presentation/i/country_A.pdf", 7, 10.5)

#par(mfrow=c(3,1), mar=c(3,3.1,3.7,2.1))
par(mfrow=c(2,1), mar=c(4,4.1,5.1,2.7))

r <- 0.5

S <- matrix(c(1, r, r, 1), 2 ,2)
mu <- c(0,0)

x <- seq(-4,4,length=25)
y <- seq(-4,4,length=25)

require(mvtnorm)
#z <- sapply(x, function(x.i) sapply(y, function(y.i) dmvnorm(c(x.i, y.i), c(0,0), S ) ) )
#contour(x,y,z, main=paste("Two variables of interest correlate",r), col="#ffff9c")
#abline(0, r, lty=2, col="gray")

#m <- seq(0,.99,length=50)
m  <- 0.4
rr <- (1-m^2)*r + m^2
#plot(m,rr)
S <- matrix(c(1, rr, rr, 1), 2 ,2)
z <- sapply(x, function(x.i) sapply(y, function(y.i) dmvnorm(c(x.i, y.i), c(0,0), S ) ) )

#contour(x,y,z, main=paste("Invalidity attenuates the correlation by a factor",1-m^2,", while method effects of",m,"add ",m^2,".\nResult: true scores correlate",round(rr,1)), col="#ffff9c")
#abline(0, rr, lty=2, col="gray")

rel <- 0.8

rrr <- rel^2 * rr
S <- matrix(c(1, rrr, rrr, 1), 2 ,2)
z <- sapply(x, function(x.i) sapply(y, function(y.i) dmvnorm(c(x.i, y.i), mu, S ) ) )

#contour(x,y,z, main=paste("Unreliability further attenuates the correlation by a factor",rel^2,".\nResult: response variables correlate",round(rrr,1)), col="#ffff9c")
contour(x,y,z, main=paste("Unreliability and invalidity attenuate the correlation by a factor",round((1-m^2)*rel^2,1),".\nwhile method effects of",m,"add ",m^2,".\nResult: response variables correlate",round(rrr,1)), col="#ffff9c")
abline(mu[1], rrr, lty=2, col="gray")



X <- rmvnorm(1000000, mu, S)
Y <- X
my.breaks<-c(-Inf, -.5, 1.3, 1.9, 2.6, Inf)
#my.breaks<-c(-Inf, -.5, 1.3, 1.9, 2.6, Inf)
Y[,1]<- cut(X[,1], breaks=my.breaks ) 
Y[,2]<- cut(X[,2], breaks=my.breaks ) 

T <- table(Y[,1], Y[,2])
T <- T/sum(T)
dT <- as.data.frame(T)
cx <- .75 + dT[,3]*4

pcor <- round(hetcor(as.factor(Y[,1]), as.factor(Y[,2]))$correlations[2],1)


#plot(jitter(Y), main=paste("The variables are split up into 5 categories at unequal intervals.\nPearson correlation:", round(cor(Y)[2],1)))
contour(1:length(table(Y[,1])), 1:length(table(Y[,2])), T, main=paste("The variables are split up into 5 categories at unequal intervals.\nPearson correlation:", round(cor(Y)[2],1),"\nPolychoric correlation:",pcor), col="#ffff9c", xlim=c(0.8,.2+length(table(Y[,1]))))
#contour( 1:length(table(Y[,1])), 1:length(table(Y[,2])), T,  
#         main=paste("Unreliability and invalidity attenuate the correlation by a factor",
#                    round((1-m^2)*rel^2,1),
#                    ".\nwhile method effects of",m,"add ",m^2,
#                    ",\nand the variables are split up into 5 categories at unequal intervals.\nPearson correlation:", round(cor(Y)[2],1)
#                   ) , col="#ffff9c", xlim=c(0.8,length(table(Y[,1])))
#        )
text(dT[,1], dT[,2], paste(round(dT[,3]*100,2),"%",sep=""), cex=cx)

#abline(lm(Y[,1]~Y[,2]), lty=2, col="gray")

dev.off()

