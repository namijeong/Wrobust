source("https://raw.githubusercontent.com/namijeong/tar/main/Rm16.R")
#
data = 1000* c(100,  90,  59, 117, 177,  98, 125, 118,  99, 132, 97, 87, 126, 107, 66, 186,  158,  80,  69, 109)
library(weibullness)
#data = c(15.1 ,12.2 ,17.3, 14.3 , 7.9 ,18.2 ,24.6 ,13.5, 10.0, 30.5)
#==============================================================================
idx.sort = order( data )
data.sort = data[idx.sort]
Fall     = ppoints( data )
X=sort(data)
#X[20]=X[20]*100
Sweibull <- function(x, shape,scale) {
  (1-pweibull(x,shape=shape,scale=scale))}

pdf(file="Fig4.pdf", width=14.5, height=6.6, paper = "special", encoding = "TeXtext.enc")
par( mfrow=c(1,3), mar=c(6.5,4.8,3,1), omi=c(0,0,0,0), cex=1 ,mex=0.6 )
#==============================================================================
xlim = c(9,14)
#xlim = c(0,5)
#ylim = range( log(-log(Fall) ) )
plot(  log(data.sort),  log(-log(1-Fall)), main = "Weibull Probability", pch=16, xlim=xlim,
       xlab="logt", ylab="log{-log(1-F(t))}",lwd = 1, cex =1, sub="(a)" )

xxx = seq(min(X),max(X), l=100)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.mle(data,0)$shape,scale=weibull.mle(data,0)$scale))), lty=1, col ="red" ,lwd = 2)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.wp(data)$shape,scale=weibull.wp(data)$scale))), lty=2, col ="orange red" ,lwd = 2)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.med1(data)$shape,scale=weibull.med1(data)$scale))), lty=1, col ="dark blue" ,lwd = 2)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.med2(data)$shape,scale=weibull.med2(data)$scale))), lty=3, col ="navy blue" ,lwd = 2)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.med3(data)$shape,scale=weibull.med3(data)$scale))), lty=4, col ="slate blue" ,lwd = 2)

legend (12, -2.5, lty=c(1,2,3,4,5) ,col=c("dark gray","Dim Gray","Pale violet red2","dark Orchid2","slate blue"),legend=c("MLE", "WP","MED1","MED2","MED3"), bty="n",lwd = 2, cex =1)
#legend (0, 1.1, lty=c(1,2,1,3,4) ,col=c("red","orange red","dark blue","navy blue","slate blue"),legend=c("MLE", "WP","med1","med2","med3"), bty="n",lwd = 2, cex =1.5)
#outlier==============================================================================
data = 1000* c(100,  90,  59, 117, 177,  98, 125, 118,  99, 132, 97, 87, 126, 107, 66, 186,  158,  80,  69, 109)
#data = c(15.1 ,12.2 ,17.3, 14.3 , 7.9 ,18.2 ,24.6 ,13.5, 10.0, 30.5)
X=sort(data)
X[20]=X[20]*5
#X[10]=X[10]*5
idx.sort = order( X )
data.sort = X[idx.sort]
Fall     = ppoints( X )

plot(  log(data.sort),  log(-log(1-Fall)), main = "Weibull Probability", pch=16, xlim=xlim,
       xlab="logt", ylab="log{-log(1-F(t))}",lwd = 1, cex =1, sub="(b)" )

xxx = seq(min(X),max(X), l=100)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.mle(X,0)$shape,scale=weibull.mle(data,0)$scale))), lty=1, col ="red" ,lwd = 2)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.wp(X)$shape,scale=weibull.wp(X)$scale))), lty=2, col ="orange red" ,lwd = 2)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.med1(X)$shape,scale=weibull.med1(X)$scale))), lty=1, col ="dark blue" ,lwd = 2)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.med2(X)$shape,scale=weibull.med2(X)$scale))), lty=3, col ="navy blue" ,lwd = 2)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.med3(X)$shape,scale=weibull.med3(X)$scale))), lty=4, col ="slate blue" ,lwd = 2)

legend (12, -2.5, lty=c(1,2,3,4,5) ,col=c("dark gray","Dim Gray","Pale violet red2","dark Orchid2","slate blue"),legend=c("MLE", "WP","MED1","MED2","MED3"), bty="n",lwd = 2, cex =1)
#legend (0, 1.1, lty=c(1,2,1,3,4) ,col=c("red","orange red","dark blue","navy blue","slate blue"),legend=c("MLE", "WP","med1","med2","med3"), bty="n",lwd = 2, cex =1.5)
#inlier==============================================================================
data = 1000* c(100,  90,  59, 117, 177,  98, 125, 118,  99, 132, 97, 87, 126, 107, 66, 186,  158,  80,  69, 109)
#data = c(15.1 ,12.2 ,17.3, 14.3 , 7.9 ,18.2 ,24.6 ,13.5, 10.0, 30.5)
X=sort(data)
X[1]=X[1]*1/5
idx.sort = order( X )
data.sort = X[idx.sort]
Fall     = ppoints( X )

plot(  log(data.sort),  log(-log(1-Fall)), main = "Weibull Probability", pch=16,xlim=xlim,  
       xlab="logt", ylab="log{-log(1-F(t))}",lwd = 1, cex =1 , sub="(c)" )

xxx = seq(min(X),max(X), l=100)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.mle(X,0)$shape,scale=weibull.mle(data,0)$scale))), lty=1, col ="red" ,lwd = 2)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.wp(X)$shape,scale=weibull.wp(X)$scale))), lty=2, col ="orange red" ,lwd = 2)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.med1(X)$shape,scale=weibull.med1(X)$scale))), lty=1, col ="dark blue" ,lwd = 2)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.med2(X)$shape,scale=weibull.med2(X)$scale))), lty=3, col ="navy blue" ,lwd = 2)
lines(log(xxx), log(-log(Sweibull( xxx, shape=weibull.med3(X)$shape,scale=weibull.med3(X)$scale))), lty=4, col ="slate blue" ,lwd = 2)

legend (12, -2.5, lty=c(1,2,3,4,5) ,col=c("dark gray","Dim Gray","Pale violet red2","dark Orchid2","slate blue"),legend=c("MLE", "WP","MED1","MED2","MED3"), bty="n",lwd = 2, cex =1)
#legend (0, 1.1, lty=c(1,2,1,3,4) ,col=c("red","orange red","dark blue","navy blue","slate blue"),legend=c("MLE", "WP","med1","med2","med3"), bty="n",lwd = 2, cex =1.5)


