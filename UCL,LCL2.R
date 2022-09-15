source("C:/nnn/Rm16.R")


###############################################
##Book p401, 전기설비데이터, p-value = 0.7875

data=c(1054.8, 6280.6, 2765.5, 16612.2, 1149.5,
       65.3, 9062.3, 15154.4, 17292.3, 16146.3,
       8471.4, 15015.9, 307.1, 6453.3, 6272.1, 
       15883.1, 32762.0, 1127.3, 232.5, 15572.9, 
       7082.0, 14871.1, 14589.4, 2819.4, 474.8,
       2044.4, 8568.1, 564.5, 345.1, 5683.2, 
       4666.7, 4322.5, 12211.1, 3335.3, 7102.7, 
       6115.8, 4752.8, 790.7, 4075.6, 5885.9, 
       2898.2, 4896.1, 11137.9, 1832.7, 2137.5, 
       9215.6, 7176.7, 799.5)


library(weibullness)

data1.original = data[1]

## DELTA = c(1, seq(10, 10000, by=100) )
DELTA = c( seq(0.00001,0.99,length=50), seq(1,1000, length=50)  )
a2 = 1-pnorm(3)     # alpha over 2
n = 51 # The original data (no contamination)

UCL.mle = UCL.wp = UCL.seki = numeric(length(DELTA))
CL.mle =  CL.wp =  CL.seki = numeric(length(DELTA))
LCL.mle = LCL.wp = LCL.seki = numeric(length(DELTA))
UCL.e = UCL.wmed = UCL.med1 = UCL.med2 = UCL.med3 = numeric(length(DELTA))
CL.e =  CL.wmed =  CL.med1 =  CL.med2 =  CL.med3 = numeric(length(DELTA))
LCL.e = LCL.wmed = LCL.med1 = LCL.med2 = LCL.med3 = numeric(length(DELTA))

#
for ( i in seq_along(DELTA) ) { 
  data[1] = data1.original * DELTA[i]
  para = weibull.mle(data, threshold=0)
  UCL.mle[i] = qweibull(1-a2, shape=para$shape, scale=para$scale)
  CL.mle[i] = qweibull(0.50, shape=para$shape, scale=para$scale)
  LCL.mle[i] = qweibull(  a2, shape=para$shape, scale=para$scale)
  
  para = weibull.wp(data)
  UCL.wp[i]  = qweibull(1-a2, shape=para$shape, scale=para$scale)
  CL.wp[i]  = qweibull(0.50, shape=para$shape, scale=para$scale)
  LCL.wp[i]  = qweibull(  a2, shape=para$shape, scale=para$scale)
  
  para = weibull.seki(data)
  UCL.seki[i]= qweibull(1-a2, shape=para$shape, scale=para$scale)
  CL.seki[i]= qweibull(0.50, shape=para$shape, scale=para$scale)
  LCL.seki[i]= qweibull(  a2, shape=para$shape, scale=para$scale)
  
  para = weibull.e(data)
  UCL.e[i]   = qweibull(1-a2, shape=para$shape, scale=para$scale)
  CL.e[i]   = qweibull(0.50, shape=para$shape, scale=para$scale)
  LCL.e[i]   = qweibull(  a2, shape=para$shape, scale=para$scale)
  
  para = weibull.wmed(data)
  UCL.wmed[i]= qweibull(1-a2, shape=para$shape, scale=para$scale)
  CL.wmed[i]= qweibull(0.50, shape=para$shape, scale=para$scale)
  LCL.wmed[i]= qweibull(  a2, shape=para$shape, scale=para$scale)
  
  para = weibull.med1(data)
  UCL.med1[i]= qweibull(1-a2, shape=para$shape, scale=para$scale)
  CL.med1[i]= qweibull(0.50, shape=para$shape, scale=para$scale)
  LCL.med1[i]= qweibull(  a2, shape=para$shape, scale=para$scale)
  
  para = weibull.med2(data)
  UCL.med2[i]= qweibull(1-a2, shape=para$shape, scale=para$scale)
  CL.med2[i]= qweibull(0.50, shape=para$shape, scale=para$scale)
  LCL.med2[i]= qweibull(  a2, shape=para$shape, scale=para$scale)
  
  para = weibull.med3(data)
  UCL.med3[i]= qweibull(1-a2, shape=para$shape, scale=para$scale)
  CL.med3[i]= qweibull(0.50, shape=para$shape, scale=para$scale)
  LCL.med3[i]= qweibull(  a2, shape=para$shape, scale=para$scale)
}

ylim = c(10000, 100000)
ylim1 = c(0,40)
par(mfrow=c(2,1), mar=c(5,5,3,1), omi=c(0,0,0,0), cex=0.5, mex=0.5)


plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )
lines(DELTA, UCL.mle, col="dark gray", type="l", lwd=2)
abline( h = UCL.mle[n], col="black", lwd=2)           ## MLE without noise 
lines(DELTA, UCL.wp, col="Dim Gray", type="l", lwd=2)
lines(DELTA, UCL.med1, col="Pale violet red2", type="l", lwd=2)
lines(DELTA, UCL.med2, col="dark Orchid2", type="l", lwd=2)
lines(DELTA, UCL.med3, col="slate blue", type="l", lwd=2)


plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim1 )
lines(DELTA,  LCL.mle, col="dark gray", type="l", lwd=2)
abline( h = LCL.mle[n], col="black", lwd=2)
lines(DELTA, LCL.wp, col="Dim Gray", type="l", lwd=2)
lines(DELTA, LCL.med1, col="Pale violet red2", type="l", lwd=2)
lines(DELTA, LCL.med2, col="dark Orchid2", type="l", lwd=2)
lines(DELTA, LCL.med3, col="slate blue", type="l", lwd=2)
#lines(DELTA, LCL.seki, col="red", type="l", lwd=2)

#lines(DELTA, CL.mle, col="black", type="l", lwd=2)

#abline( h =  CL.mle[n], col="red", lty=3, lwd=0.2)    ## MLE without noise 
#           ## MLE without noise 



#lines(DELTA,  CL.wp, col="black", type="l", lwd=1, lty=3)
#
#abline( h = UCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
#abline( h =  CL.mle[n], col="red", lty=3, lwd=0.2)    ## MLE without noise 
#abline( h = LCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 

#plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )
#lines(DELTA, UCL.seki, col="black", type="l", lwd=2)
#lines(DELTA,  CL.seki, col="black", type="l", lwd=1, lty=3)
#lines(DELTA, LCL.seki, col="black", type="l", lwd=2)
#abline( h = UCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
#abline( h =  CL.mle[n], col="red", lty=3, lwd=0.2)    ## MLE without noise 
#abline( h = LCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 

#plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )
#lines(DELTA, UCL.e, col="black", type="l", lwd=2)
#lines(DELTA,  CL.e, col="black", type="l", lwd=1, lty=3)
#lines(DELTA, LCL.e, col="black", type="l", lwd=2)
#abline( h = UCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
#abline( h =  CL.mle[n], col="red", lty=3, lwd=0.2)    ## MLE without noise 
#abline( h = LCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 

#plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )
#lines(DELTA, UCL.wmed, col="black", type="l", lwd=2)
#lines(DELTA,  CL.wmed, col="black", type="l", lwd=1, lty=3)
#lines(DELTA, LCL.wmed, col="black", type="l", lwd=2)
#abline( h = UCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
#abline( h =  CL.mle[n], col="red", lty=3, lwd=0.2)    ## MLE without noise 
#abline( h = LCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 

#plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )

#lines(DELTA,  CL.med1, col="black", type="l", lwd=1, lty=3)
#
#abline( h = UCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
#abline( h =  CL.mle[n], col="red", lty=3, lwd=0.2)    ## MLE without noise 
#abline( h = LCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 

#plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )

#lines(DELTA,  CL.med2, col="black", type="l", lwd=1, lty=3)
#
#abline( h = UCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
#abline( h =  CL.mle[n], col="red", lty=3, lwd=0.2)    ## MLE without noise 
#abline( h = LCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 

#plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )

#lines(DELTA,  CL.med3, col="black", type="l", lwd=1, lty=3)
#
#abline( h = UCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
#abline( h =  CL.mle[n], col="red", lty=3, lwd=0.2)    ## MLE without noise 
#abline( h = LCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 