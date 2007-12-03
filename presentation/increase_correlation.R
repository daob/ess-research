

#Switzerland
polycor<- matrix(c(
1.000,NA,NA,NA,NA,NA,
-.369,1.000,NA,NA,NA,NA,
-.463,.420,1.000,NA,NA,NA,
.570,-.362,-.462,1.000,NA,NA,
-.318,.828,.362,-.393,1.000,NA,
-.358,.437,.695,-.493,.429,1.000),6,6,byrow=T)



pearcor <- matrix(c(
1.00,NA,NA,NA,NA,NA,
-.334,1.00,NA,NA,NA,NA,
-.340,.358,1.00,NA,NA,NA,
.552,-.350,-.453,1.00,NA,NA,
-.303,.816,.333,-.337,1.00,NA,
-.354,.413,.617,-.480,.387,1.00),6,6,byrow=T)


round( polycor/pearcor, 2)

100*round( (polycor-pearcor)/pearcor , 2)
