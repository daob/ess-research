#log(q) = a + b log(c)
#exp( log(c)*b + a )

meta <- read.table("~/current-project/data/meta.tab", header=T)
meta$ptnames <- 1:nrow(meta)
meta <- meta[ -c( 28, 2, 11, 49 ), ]             # tried deleting points 28, 2, 11, 49


# Estimate a log-log linear model for each topic separately
list.models <- by(cbind(log(meta$q2),log(meta$cat_factor)), meta$topic, lm )

# The names of the experiments as they are to appear in the graph
experiment.name <- c(
                     doctors  = "Social distance",
                     efficacy = "Efficacy",
                     job      = "Job",
                     women    = "Role of women"
                    )

predict.exp <- function(c, subgroup) {
  # provides a function to plot for a given topic
  b <-  list.models[[subgroup]]$coef[2]
  a <-  list.models[[subgroup]]$coef[1]
  exp( log(c)*b + a )  
}
 
plot.subset <- function(subgroup) {
  # produces 1 plot of the observed data and predicted curve for a given topic
  tmp <- subset(meta, topic==subgroup)
  predict.local <- function(x) {predict.exp(x, subgroup)}
  curve(predict.local, 0.40, 2, ylim=range(meta$q2), main=experiment.name[subgroup], xlab=xlab, ylab=ylab)
# Uncomment this to get the point ID number in the graph instead of a black dot:  
#  text(tmp$cat_factor, tmp$q2, labels=as.character(tmp$ptnames) )
  points(tmp$cat_factor, tmp$q2, pch=20)  

}

plot.meta <- function(meta)  {
  # produces one graph with each of the plots corresponding to a model
  par(mfrow=c(2,2))
  par(oma=c(0,0,0,0))
  par(mar=c(3,3,3,1.5), mgp=c(2,1,0))

  xlab <- "Categorisation factor"
  ylab <- "Quality (continuous model)"
  
  lapply(levels(meta$topic), plot.subset)
}


pdf("~/current-project/latex/i/predict_q2.pdf")
plot.meta(meta)
dev.off()
