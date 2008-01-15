#log(q) = a + b log(c)
#exp( log(c)*b + a )

meta <- read.table("~/current-project/data/meta.tab", header=T)
meta <- meta[,]             # tried deleting points 37, 43, 49 in sensitivity analysis.

list.models <- by(cbind(log(meta$q2),log(meta$cat_factor)), meta$topic, lm )

 predict.exp.women <- function(c) {
    b <-  list.models$women$coef[2]
    a <-  list.models$women$coef[1]
    exp( log(c)*b + a )  
 }
 
  predict.exp.doctors <- function(c) {
    b <-  list.models$doctors$coef[2]
    a <-  list.models$doctors$coef[1]
    exp( log(c)*b + a )  
 }
 
  predict.exp.job <- function(c) {
    b <-  list.models$job$coef[2]
    a <-  list.models$job$coef[1]
    exp( log(c)*b + a )  
 }
 
  predict.exp.efficacy <- function(c) {
    b <-  list.models$efficacy$coef[2]
    a <-  list.models$efficacy$coef[1]
    exp( log(c)*b + a )  
 }
 
pdf("~/current-project/latex/i/predict_q2.pdf")
 
opar<-par()
par(mfrow=c(2,2))
par(oma=c(0,0,0,0))
par(mar=c(3,3,3,1.5), mgp=c(2,1,0))

curve(predict.exp.doctors, 0.40, 2, ylim=c(min(q2),max(q2)), main="Social distance", xlab="Categorisation factor", ylab="Quality (continuous model)")
points(cat_factor[topic=='doctors'], q2[topic=='doctors'], pch=20)

curve(predict.exp.efficacy, 0.40, 2, ylim=c(min(q2),max(q2)),  main="Efficacy", xlab="Categorisation factor", ylab="Quality (continuous model)")
points(cat_factor[topic=='efficacy'], q2[topic=='efficacy'], pch=20)

curve(predict.exp.job, 0.40, 2, ylim=c(min(q2),max(q2)),  main="Job", xlab="Categorisation factor", ylab="Quality (continuous model)")
points(cat_factor[topic=='job'], q2[topic=='job'], pch=20)

curve(predict.exp.women, 0.40, 2, ylim=c(min(q2),max(q2)), main="Role of women", xlab="Categorisation factor", ylab="Quality (continuous model)")
points(cat_factor[topic=='women'], q2[topic=='women'], pch=20)

dev.off()
