meta <- read.table("../data/meta.dat", header=T)
 attach(meta)
boxplot(factor~scale)
 m=lm(( factor )~topic+country+scale+position, data=meta[-c(28,64,37,2,8,11),])
 p=MCMCregress(factor~topic+country+scale+position, data=meta[-c(28,64,37,2,8,11),], B0=10)
oefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)       0.44098    0.21457   2.055  0.04454 *  
topicefficacy     0.14195    0.12485   1.137  0.26040    
topicjob          0.19598    0.18607   1.053  0.29674    
topicwomen        0.44791    0.17093   2.620  0.01128 *  
countryCH         0.30985    0.14954   2.072  0.04288 *  
countryCZ         0.28359    0.15377   1.844  0.07043 .  
countryDK         0.32762    0.17281   1.896  0.06315 .  
countryEE         0.53318    0.19331   2.758  0.00784 ** 
countryGR         0.31759    0.15377   2.065  0.04352 *  
countrySI         0.08676    0.09965   0.871  0.38763    
scaledirect       0.11833    0.09187   1.288  0.20303    
scaletruefalse    0.41716    0.14268   2.924  0.00498 ** 
positionnegative -0.48415    0.07244  -6.683 1.15e-08 ***
positionrarely   -0.07305    0.14240  -0.513  0.60998    
positionusually  -0.09638    0.14240  -0.677  0.50129


          Mean      SD  Naive SE Time-series SE
(Intercept)       0.44386 0.21968 0.0021968      0.0022186
topicefficacy     0.14068 0.12861 0.0012861      0.0012180
topicjob          0.19162 0.19000 0.0019000      0.0018765
topicwomen        0.44437 0.17531 0.0017531      0.0014626
countryCH         0.30983 0.15206 0.0015206      0.0017332
countryCZ         0.28275 0.15528 0.0015528      0.0015975
countryDK         0.32490 0.17606 0.0017606      0.0020739
countryEE         0.53021 0.19620 0.0019620      0.0020899
countryGR         0.31806 0.15467 0.0015467      0.0015706
countrySI         0.08696 0.10131 0.0010131      0.0011451
scaledirect       0.11865 0.09463 0.0009463      0.0010534
scaletruefalse    0.41894 0.14622 0.0014622      0.0014927
positionnegative -0.48418 0.07301 0.0007301      0.0008210
positionrarely   -0.07176 0.14547 0.0014547      0.0013554
positionusually  -0.09638 0.14611 0.0014611      0.0013895
sigma2            0.03496 0.00684 0.0000684      0.0000836


dm <- model.matrix(factor ~ topic + country + scale + position, data=meta)
meta$negative <- dm[,13]
m=lm(( factor )~ topic + country + scale + negative, data=meta[-c(28,64,37,2,8,11),])



me$id <- as.numeric(paste(as.numeric(me$country),me$trait,me$method,sep=""))
me.wide <- reshape(me, v.names="methodeffect", idvar="id", timevar="analysis", direction="wide")
