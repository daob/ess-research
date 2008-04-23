#log(q) = a + b log(c)
#exp( log(c)*b + a )

meta <- read.table("../data/meta.tab", header=T)
meta <- meta[,]             # tried deleting points 37, 43, 49 in sensitivity analysis.

msub <- subset(meta, q2>.10)

plot.pch <- (as.numeric(msub$topic)+20)
exp.names <- c("Social distance","Efficacy","Job","Role of women")

pdf("../latex/i/figure-5.pdf", width="8", height="6")
par(mar=c(4,4,0,0.05))

plot(msub$cat_factor, msub$q2, log="yx", 
     pch=plot.pch, bg="black",
     xlab="Categorization factor", ylab="Quality, continuous model")

legend(1.6, 0.2, legend=exp.names, pch = unique(plot.pch),
       pt.bg="black", cex = 0.8)
dev.off()
