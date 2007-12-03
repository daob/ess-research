data <- read.table("C:/ESS/berlin/data/women.dat", header=FALSE, sep=" ", na.strings="-9999", dec=".", strip.white=TRUE)
showData(data, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)
data$country <- recode(data$V1, '3=CH 4=CZ 12=GR 22=SI; ', as.factor.result=TRUE)
showData(data, placement='-20+200', font=getRcmdr('logFont'), maxwidth=80, maxheight=30)
slovenia <- subset(data, subset=V2 == 22)
greece <- subset(data, subset=V2 == 12)

slovenia <- greece

names(slovenia)[4:12] <- c('Woman should cut down','Men as much responsibility for home', 'Men more right to job', 'Woman should not cut down', 'Women more responsibility for home', 'Women same right to job', '5pt cut down', '5pt more responsibility', '5pt same right')
opar<-par()
par(mfrow=c(3,3))
for(i in 4:12) {
 if(i<7) 
	xlab = "Agree strongly - Disagree strongly: battery" 
 else if(i<10) 
	xlab = "Agree strongly - Disagree strongly: single question" 
 else
	xlab="Direct question"
 hist(slovenia[,i], main=paste(names(slovenia)[i]), xlab=xlab)
}
