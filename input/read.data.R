# This script is designed to read the raw ESS data provided by NSD 
# at http://ess.nsd.uib.no/ in SPSS .por format 
# It provides tables or correlation matrices or other wanted output
# for a subset of the variables efficiently.
# This is done by first getting the subset of interest
# and only then converting to data frame and merging. Note that
# converting the entire data set or too many subsetting variables
# into a data frame immediately can take a VERY long time.
#
# author: Daniel Oberski,
# Survey Research Centre of ESADE, Barcelona and
# Tilburg University, Tiburg, The Netherlands
# email: daniel.oberski@gmail.com
#
# This version: 20070810


# clear R memory
rm(list = ls())
basedir <- "/home/daob/work/svn/ess-research/"
analysis.name <- "women"

# needed for reading SPSS data:
require(foreign)

# read main and MTMM data files:
r2.main <- read.spss( "/home/daob/work/ess/data/ESS2e03.spss.zip_FILES/ESS2e03.por", use.value.labels = FALSE)

r2.mtmm <- read.spss( "/home/daob/work/ess/data/ESS2MTMMe03_1.spss.zip_FILES/ESS2MTMMe03_1.por", use.value.labels = FALSE)

# interviewer questionnaire: r2.int <- 

# assign each case a unique ID number based on country and ESS id number
r2.main$unique.id <- paste(r2.main$CNTRY, r2.main$IDNO, sep="")
r2.mtmm$unique.id <- paste(r2.mtmm$CNTRY, r2.mtmm$IDNO, sep="")

# select variables to be analysed
# Edit this to select different variables.
vars.main.extra <- c("DWEIGHT","CNTRY")
vars.main <- c("WMCPWRK", "MNRSPHM", "MNRGTJB")
vars.mtmm <- c("TESTA8", "TESTA9", "TESTA10", "TESTA22", "TESTA23", "TESTA24","SPLTADMA")

# The newly created unique id must always be selected
both.need <- c( "unique.id" )

# Select the wanted subsets and convert them to data frames (may take a while)
r2.main.subset <- as.data.frame( r2.main[ which( names(r2.main)%in%c( vars.main, vars.main.extra, both.need ) ) ] )
r2.mtmm.subset <- as.data.frame( r2.mtmm[ which( names(r2.mtmm)%in%c( vars.mtmm, both.need ) ) ] )

# Merge data sets
data <- merge(r2.main.subset, r2.mtmm.subset, by="unique.id" )
data <- as.data.frame(data)

# Extra conditions, transformations, etc.

   # use only the 5 categories [TODO: add DK (8)]
missing.values <- c(6:9,66,77,88,99)
analyse.vars <- c(vars.main,vars.mtmm)

for( i in 1:length(analyse.vars) ) {
  data[ data[,analyse.vars[i]]%in%missing.values,
       analyse.vars[i]
      ] <- NA
}

data <- cbind(c(1:NROW(data)),as.numeric(data$CNTRY),data$DWEIGHT,data[,analyse.vars], data$unique.id, data$CNTRY)


countries <- c("CH","CZ","DE","EE","GR","SI")
split.ballot.codes <- c(1,2)

for (country in countries) {
  for (sbcode in split.ballot.codes) {
    sb <- data[ (data[,'data$CNTRY']==country & data[,'SPLTADMA']==sbcode) ,]
    write.table(sb, paste(basedir, "data/", analysis.name, country ,"sb",sbcode, ".dat", sep=""), na="-9999",row.names=FALSE,col.names=FALSE)
  }
}



# Produce tables, note that country comes last.
write.table(data, paste(basedir, "data/", analysis.name, ".dat", sep=""), na="-9999",row.names=FALSE,col.names=FALSE)


# Utilities:
# print marginals by country
by(data[,vars.mtmm],data$CNTRY,function(x){ table(x)/sum(table(x))  })

# read in residuals from latent class analysis
res <- read.table("~/work/svn/berlin/output/doctors/residuals.tab", header=FALSE)
names(res) = c("C","A","D","observed","estimated","std.res")
res$x2 <- res$std.res^2
# print abs residuals larger than 2
print( res[ which( abs(res$x2) >= 3 ),] )

# print largest residual
print( res[which(res$std.res==max(abs(res$std.res))),] )

# read and print conditional probabilities by country
tgiv <- read.table("~/work/svn/berlin/output/doctors/tgiv", header=TRUE)
dgiv <- read.table("~/work/svn/berlin/output/doctors/dgiv", header=TRUE)

cond.print <- function(giv) {
  r <- giv$P
  dim(r) <- c(5,3)
  return(r)
}
print( by(dgiv, dgiv$C, cond.print) )
print( by(tgiv, tgiv$C, cond.print) )


d <- scan("~/work/svn/ess-research/input/mplus/women-estimates")

intercepts <- array(dim=c(0,9))
error.vars <- array(dim=c(0,9))
loadings.list <- list()
j <- 1


for ( i in seq(1,length(d),by=171) ) {

  tmp <- d[ i:(i+171) ]
  
  intercepts <- rbind(intercepts, tmp[1:9] )
  
  loadings <- tmp[10:63]
  loadings <- t(matrix(loadings, 6,9, byrow=F))
  loadings.list[[j]] <- loadings

  theta <- tmp[64:(81+63)]
  theta <- theta[theta!=0]
  error.vars <- rbind(error.vars, theta)
  j=j+1
}

row.names(intercepts) <- countries
row.names(error.vars) <- countries
names(loadings.list) <- countries
write.table(intercepts,paste(basedir,"output/intercepts-",analysis.name,sep=""))
write.table(error.vars,paste(basedir,"output/error-vars-",analysis.name,sep=""))
write.table(loadings.list,paste(basedir,"output/loadings-",analysis.name,sep=""))


s <- ""

for ( i in 1:length(analyse.vars) ) {
  str <- "["
  for( j in 1:4 ) {
    str <- paste(str, analyse.vars[i], "#", j," ", sep="")
  }
  str <- paste(str,"]", sep="")
  s <- paste(str,s)
}

write(s,"~/txt")
