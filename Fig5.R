source("C:/nnn/Rm16.R")



## Urinary Tract Infection Data t-chart  p-value = 0.6614

data=c( 0.57014, 0.12014, 1.07083,
        0.07431, 0.11458, 0.04514,
        0.15278, 0.00347, 0.13542,
        0.14583, 0.12014, 0.08681,
        0.13889, 0.04861, 0.40347,
        0.14931, 0.02778, 0.12639,
        0.03333, 0.32639, 0.18403,
        0.08681, 0.64931, 0.70833,
        0.33681, 0.14931, 0.15625,
        0.03819, 0.01389, 1.04653,
        1.04653, 0.03819, 0.04514,
        1.09514, 0.46806, 0.01736,
        0.11944, 1.02222, 1.08889,
        0.05208, 1.09514, 0.05208,
        0.12500, 0.53472, 0.02778,
        1.05000, 0.15139, 0.03472,
        0.40069, 0.52569, 1.03611,
        0.02500, 0.07986, 0.35972)

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

##ylim = c(0, 180)
ylim = c(0,6)
#=============================================================
pdf(file="Fig5.pdf", width=14.1, height=8.0, paper = "special", encoding = "TeXtext.enc")

par(mfrow=c(2,3), mar=c(5,5,3,1), omi=c(0,0,0,0), cex=0.5, mex=0.5)

plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )
lines(DELTA, UCL.mle, col="black", type="l", lwd=2)
lines(DELTA,  CL.mle, col="black", type="l", lwd=1, lty=3)
lines(DELTA, LCL.mle, col="black", type="l", lwd=2)
abline( h = UCL.mle[n], col="red", lwd=1.5)           ## MLE without noise 
abline( h =  CL.mle[n], col="red", lty=3, lwd=1.5)    ## MLE without noise 
abline( h = LCL.mle[n], col="red", lwd=1.5)           ## MLE without noise 
text(1e-04,5,labels='MLE',cex=2.5)

plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )
lines(DELTA, UCL.wp, col="black", type="l", lwd=2)
lines(DELTA,  CL.wp, col="black", type="l", lwd=1, lty=3)
lines(DELTA, LCL.wp, col="black", type="l", lwd=2)
abline( h = UCL.mle[n], col="red", lwd=1.5)           ## MLE without noise 
abline( h =  CL.mle[n], col="red", lty=3, lwd=1.5)    ## MLE without noise 
abline( h = LCL.mle[n], col="red", lwd=1.5)           ## MLE without noise 
text(1e-04,5,labels='WP',cex=2.5)

plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )
lines(DELTA, UCL.med1, col="black", type="l", lwd=2)
lines(DELTA,  CL.med1, col="black", type="l", lwd=1, lty=3)
lines(DELTA, LCL.med1, col="black", type="l", lwd=2)
abline( h = UCL.mle[n], col="red", lwd=1.5)           ## MLE without noise 
abline( h =  CL.mle[n], col="red", lty=3, lwd=1.5)    ## MLE without noise 
abline( h = LCL.mle[n], col="red", lwd=1.5)
text(1e-04,5,labels='med1',cex=2.5)## MLE without noise 

plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )
lines(DELTA, UCL.med2, col="black", type="l", lwd=2)
lines(DELTA,  CL.med2, col="black", type="l", lwd=1, lty=3)
lines(DELTA, LCL.med2, col="black", type="l", lwd=2)
abline( h = UCL.mle[n], col="red", lwd=1.5)           ## MLE without noise 
abline( h =  CL.mle[n], col="red", lty=3, lwd=1.5)    ## MLE without noise 
abline( h = LCL.mle[n], col="red", lwd=1.5) 
text(1e-04,5,labels='med2',cex=2.5)## MLE without noise 

plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )
lines(DELTA, UCL.med3, col="black", type="l", lwd=2)
lines(DELTA,  CL.med3, col="black", type="l", lwd=1, lty=3)
lines(DELTA, LCL.med3, col="black", type="l", lwd=2)
abline( h = UCL.mle[n], col="red", lwd=1.5)           ## MLE without noise 
abline( h =  CL.mle[n], col="red", lty=3, lwd=1.5)    ## MLE without noise 
abline( h = LCL.mle[n], col="red", lwd=1.5)
text(1e-04,5,labels='med3',cex=2.5)## MLE without noise 
dev.off()
