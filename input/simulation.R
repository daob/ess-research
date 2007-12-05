require(mvtnorm)

symmetrize <- function(m) {
  if (length(dim(m)) != 2)
    stop("not a matrix")
  for(i in 1:(NROW(m)-1) ) 
    for(j in (i+1):NCOL(m)) 
      m[i,j] <- m[j,i]
  m
}



n <- 1000
nTraits <- 3
nMethods <- 3
nItems <- nTraits*nMethods
dimT <- (nTraits*(nTraits+1))/2 - nTraits
# generate candidate trait correlations via Fisher transform
# reject too low ones

det.phi <- -1

while(det.phi<=0) { # ensure pos def.
  z <- rnorm(dimT*4 , 0, 1)
  r <- (exp(2*z) - 1) / (exp(2*z) + 1)
  r <- sample( r[abs(r)>.15 & abs(r)<.80], dimT,  replace = FALSE)
  
  phi <- diag(rep(1, nTraits) )
  phi[lower.tri(phi)] <- r
  phi <- symmetrize(phi)
  phi <- abs(phi)
  det.phi <- det(phi)
}

F <- rmvnorm( n, rep(0, nTraits),  phi)
M <- rmvnorm( n, rep(0, nMethods),  diag(rep(1, nMethods)))

v <- matrix( rep(0,nItems*nTraits), nItems, nTraits)
for(i in 1:nTraits)
  v[seq(i, nItems-nTraits+i, nMethods), i] <- rep(.9, nMethods)


m <- matrix( rep(0,nItems*nMethods), nItems, nMethods)
for(i in 1:nMethods) {
  start <- (i-1)*(nTraits)+1
  m[start:(start+nTraits-1), i] <- rep(.2, nTraits)
}

T <- F %*% t(v) + M %*% t(m)

L <- diag(rep(.8, nItems))
theta <- diag(1 - diag(L)^2)
E <- rmvnorm(n, rep(0, nItems), theta )

Y <- T %*% t(L) + E

write.table(cov(Y),file="C:/ESS/berlin/presentation/testcov.cov", row.names=FALSE, col.names=FALSE)
write.table(Y, file="C:/ESS/berlin/presentation/testdata.dat", row.names=FALSE, col.names=FALSE)
write.table(cor(Y),file="C:/ESS/berlin/presentation/testcor.orig.cov", row.names=FALSE, col.names=FALSE)

cor.Y <- cor(Y)
cor.Y[lower.tri(cor.Y)] <- cor.Y[lower.tri(cor.Y)] + .1
cor.Y.1 <- symmetrize(cor.Y)

cor.Y.1.m <- cor(Y)
cor.Y.1.m[ cbind(c(2,3,3),c(1,1,2)) ] <- cor.Y.1.m[ cbind(c(2,3,3),c(1,1,2)) ] + .2
cor.Y.1.m <- symmetrize(cor.Y.1.m)

all <- rbind(cor(Y), cor.Y.1, cor.Y.1.m)
write.table(all,file="C:/ESS/berlin/presentation/testcor.all.cov", row.names=FALSE, col.names=FALSE)


