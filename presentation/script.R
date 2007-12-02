require(mvtnorm)
require(rgl)
require(RColorBrewer)

res <- 90
n <- res/10

x <- seq(-3, 3, length=res)
y <- x
f <- function(x,y) { dmvnorm(cbind(x,y), c(0,0), matrix(c(1, .8, .8, 1),2,2, byrow=T) )  }
z <- outer(x, y, f)
op <- par(bg = "white")

mypalette<-brewer.pal(n,"Blues")
p <- character()
for(i in 1:length(mypalette))
  p <- c(p,rep(mypalette[i],res/n))

s <- persp(x, y, z, theta = 20, phi = 40, expand = 0.45, col = p[80],
      ltheta = 120, shade = 0.75, ticktype = "detailed", border=p[90],
      xlab = "X", ylab = "Y", zlab = "Pr(X,Y)", box = F, nticks = 4, axes=T
) 

 thresh.x <- c(-1.8, -0.8, -0.3, 0.9)
 thresh.y <- c(-0.5, 1.3, 1.9, 2.6)
 zz <- rep(0,length(thresh.x))


for(i in 1:length(thresh.x)) {
  xx <- rep(thresh.x[i], res)
  lines( trans3d( xx, y, f(xx,y), pmat=s ), pch=20, col=p )
}

for(i in 1:length(thresh.y)) {
  yy <- rep(thresh.y[i], res)
  lines( trans3d( x, yy, f(x,yy), pmat=s ), pch=20, col=p)
}




rgl.open()

XY <- rmvnorm(1000, c(0,0), matrix(c(1, .8, .8, 1),2,2, byrow=T) )

nclass <- 4

breaks.x <- seq(min(x)-.001,max(x),length=(nclass+1))
breaks.y <- seq(min(y)-.001,max(y),length=(nclass+1))

hist3d(x,y,alpha=0.4,nclass=5,scale=25)

rgl.close()
