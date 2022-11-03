source("https://raw.githubusercontent.com/namijeong/tar/main/Rm16.R")

# DATA 
############################################################################################
library(weibullness)

n = 100
shape0 = 1.25; scale0 = 1
# Pure data
data = rweibull(n, shape=shape0, scale=scale0)
###############################################


data1.original = data[1]

## DELTA = c(1, seq(10, 10000, by=100) )
DELTA = c( seq(0.00001,0.99,length=50), seq(1,1000, length=50)  )
# The original data (no contamination)
SHAPE.mle = SHAPE.wp = SHAPE.wmed = SHAPE.e = numeric(length(DELTA))
SCALE.mle = SCALE.wp = SCALE.wmed = SCALE.e = numeric(length(DELTA))
SHAPE.med1= SHAPE.med2= SHAPE.med3= SHAPE.seki = numeric(length(DELTA))
SCALE.med1= SCALE.med2= SCALE.med3= SCALE.seki = numeric(length(DELTA))

SHAPE.mle1 = SHAPE.wp1 = SHAPE.wmed1 = SHAPE.e1 = numeric(length(DELTA))
SCALE.mle1 = SCALE.wp1 = SCALE.wmed1 = SCALE.e1 = numeric(length(DELTA))
SHAPE.med11= SHAPE.med21= SHAPE.med31= SHAPE.seki1 = numeric(length(DELTA))
SCALE.med11= SCALE.med21= SCALE.med31= SCALE.seki1 = numeric(length(DELTA))


for ( i in seq_along(DELTA) ) { 
  data[1] = data1.original * DELTA[i]
  para = weibull.mle(data, threshold=0)
  SHAPE.mle[i] = para$shape;  
  SCALE.mle[i] = para$scale
  
  para = weibull.wp(data)
  SHAPE.wp[i] = para$shape;  
  SCALE.wp[i] = para$scale
  
  para = weibull.e(data)
  SHAPE.e[i] = para$shape;  
  SCALE.e[i] = para$scale
  
  para = weibull.seki(data)
  SHAPE.seki[i] = para$shape;
  SCALE.seki[i] = para$scale
  
  para = weibull.wmed(data)
  SHAPE.wmed[i] = para$shape;  
  SCALE.wmed[i] = para$scale
  
  para = weibull.med1(data)
  SHAPE.med1[i] = para$shape; 
  SCALE.med1[i] = para$scale
  
  para = weibull.med2(data)
  SHAPE.med2[i] = para$shape;
  SCALE.med2[i] = para$scale
  
  para = weibull.med3(data)
  SHAPE.med3[i] = para$shape;
  SCALE.med3[i] = para$scale
}

#=============================================================
##pdf(file="CC-Sensitivity3.pdf", width=6.5, height=8.0, paper = "special", encoding = "TeXtext.enc")
ylim = c(-0.7,3.2)
ylim1=c(0,2)
par(mfrow=c(1,2), mar=c(7,5,3,1), omi=c(0,0,0,0), cex=1, mex=0.6)


plot(NA, NA, xlab=expression(delta),ylab="SHAPE", log="x", type="n", xlim=range(DELTA), ylim=ylim, sub="(a) shape" )
abline( h = shape0, col="black", lwd=1)    

lines(DELTA, SHAPE.mle, col="red", type="l", lwd=1)
lines(DELTA, SHAPE.wp, col="Orange red", type="l", lwd=1, lty=2)
lines(DELTA, SHAPE.seki, col="pale violet red", type="l", lwd=1,lty=3)

lines(DELTA, SHAPE.e, col="Lime green", type="l", lwd=1,lty=1)
lines(DELTA, SHAPE.wmed, col="Forest green", type="l", lwd=1,lty=3)
lines(DELTA, SHAPE.med1, col="dark blue", type="l", lwd=2,lty=1)
lines(DELTA, SHAPE.med2, col="navy blue", type="l", lwd=2,lty=3)
lines(DELTA, SHAPE.med3, col="slate blue", type="l", lwd=2,lty=4)

legend (1e-05, 3,lty=c(1,2,3,1) ,col=c("red","Orange red","pale violet red","Lime green"),
        legend=c("MLE", "WP","seki","e"), bty="n" ,lwd = 2, cex =1)
legend (1e-03, 3,lty=c(3,1,3,4) ,col=c( "Forest green","dark blue","navy blue","slate blue"),
        legend=c("W.med","med1","med2","med3"), bty="n" ,lwd = 2, cex =1)

plot(NA, NA, xlab=expression(delta),ylab="SCALE" ,log="x", type="n", xlim=range(DELTA), ylim=ylim1, sub="(b) scale" )
abline( h = scale0, col="black", lwd=1)

lines(DELTA, SCALE.mle, col="red", type="l", lwd=1)
lines(DELTA, SCALE.wp, col="Orange red", type="l", lwd=1, lty=2)
lines(DELTA, SCALE.seki, col="pale violet red", type="l", lwd=1,lty=3)

lines(DELTA, SCALE.e, col="Lime green", type="l", lwd=1,lty=1)
lines(DELTA, SCALE.wmed, col="Forest green", type="l", lwd=1,lty=3)
lines(DELTA, SCALE.med1, col="dark blue", type="l", lwd=2,lty=1)
lines(DELTA, SCALE.med2, col="navy blue", type="l", lwd=2,lty=3)
lines(DELTA, SCALE.med3, col="slate blue", type="l", lwd=2,lty=4)

legend (1e-05, 0.55, lty=c(1,2,3,1) ,col=c("red","Orange red","pale violet red","Lime green"),
        legend=c("MLE", "WP","seki","e"), bty="n"  ,lwd = 2, cex =1)
legend (1e-03, 0.55, lty=c(3,1,3,4) ,col=c( "Forest green","dark blue","navy blue","slate blue"),
        legend=c("W.med","med1","med2","med3"), bty="n"  ,lwd = 2, cex =1)
