
source("https://raw.githubusercontent.com/namijeong/tar/main/Rm16.R")
#data = rweibull(n, shape=shape0, scale=scale0)
data = 1000* c(100,  90,  59, 117, 177,  98, 125, 118,  99, 132, 97, 87, 126, 107, 66, 186,  158,  80,  69, 109)
library(weibullness)
#data = c(15.1 ,12.2 ,17.3, 14.3 , 7.9 ,18.2 ,24.6 ,13.5, 10.0, 30.5)
 hist(data)
weibull.mle(data, threshold=0)
weibull.wp(data)
weibull.e(data)
weibull.seki(data)
weibull.wmed(data)
weibull.med1(data)
weibull.med2(data)
weibull.med3(data)
#==============================================================================
idx.sort = order( data )
data.sort = data[idx.sort]
Fall     = ppoints( data )
X=sort(data)

Sweibull <- function(x, shape,scale) {
  (1-pweibull(x,shape=shape,scale=scale))}

par(mar=c(5,5,5,5), omi=c(0,0,0,0), cex=0.6,mex=0.5)
#==============================================================================
xlim = range( log(data) );  
ylim = range( log(-log(Fall) ) )
plot(  log(data.sort),  log(-log(1-Fall)), main = "Weibull Probability", pch=16, xlim=xlim, ylim=ylim, 
       xlab="logt", ylab="log{-log(1-F(t))}",lwd = 1.5, cex =1.5  )

xxx = seq(min(X),max(X), l=100)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.mle(data,0)$shape,scale=weibull.mle(data,0)$scale))), lty=1, col ="red" ,lwd = 2)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.wp(data)$shape,scale=weibull.wp(data)$scale))), lty=2, col ="orange red" ,lwd = 2)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.med1(data)$shape,scale=weibull.med1(data)$scale))), lty=1, col ="dark blue" ,lwd = 2)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.med2(data)$shape,scale=weibull.med2(data)$scale))), lty=3, col ="navy blue" ,lwd = 2)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.med3(data)$shape,scale=weibull.med3(data)$scale))), lty=4, col ="slate blue" ,lwd = 2)

legend (11.8, -2, lty=c(1,2,1,3,4) ,col=c("red","orange red","dark blue","navy blue","slate blue"),legend=c("MLE", "WP","med1","med2","med3"), bty="n",lwd = 2, cex =1.5)
