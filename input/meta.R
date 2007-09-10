meta <- read.table("c:/ess/berlin/data/meta.dat", header=T)
 attach(meta)
boxplot(factor~scale)
 m=lm(( factor )~topic+country+scale+position, data=meta[-c(28,64,37,2,8,11),])
