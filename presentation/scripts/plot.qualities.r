pdf("c:/ESS/berlin/presentation/i/quality_plot.pdf", width=10, height=7)

w <- read.spss('C:/ESS/ESS/wave2/MTMM analysis/mtmmexpw2.sav', to.data.frame=TRUE)

summaries <- by(w, w$COUNTRY, function(x) summary(x$MAINQUAL) )
quality <- by(w, w$COUNTRY, function(x) x$MAINQUAL )

country.means <- lapply(summaries, function(cnt) cnt['Mean'] )
country.means <- as.data.frame(do.call("rbind", country.means))
country.order <- order(country.means[,'Mean'], decreasing = TRUE)

quality <- quality[country.order]
country.means <- country.means[country.order,]

plot(0,0, xlim=c(1,length(country.order)), ylim=c(0.4, 1), pch=NA, axes=FALSE, 
      xlab="Country", ylab="Quality", 
      main="Average quality of main questionnaire items in different countries, with interquartile range\n(Continuous CFA MTMM model)" )


lower.quartiles <- do.call("c", lapply(quality, function(C) quantile(C, c(.25) ) ))
upper.quartiles <- do.call("c", lapply(quality, function(C) quantile(C, c(.75) ) ))

segments(1:length(country.means), lower.quartiles,1:length(country.means), upper.quartiles)

points(1:length(country.means), country.means, pch=20, col="#ffff9c", cex=1.4)

axis(2)
axis(1, at=1:length(country.means), labels=names(quality))

dev.off()
