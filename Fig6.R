source("https://raw.githubusercontent.com/namijeong/tar/main/Rm16.R")

data = c( 0.57014, 0.12014, 0.27083,
          0.07431, 0.11458, 0.04514,
          0.15278, 0.00347, 0.13542,
          0.14583, 0.12014, 0.08681,
          0.13889, 0.04861, 0.40347,
          0.14931, 0.02778, 0.12639,
          0.03333, 0.32639, 0.18403,
          0.08681, 0.64931, 0.70833,
          0.33681, 0.14931, 0.15625,
          0.03819, 0.01389, 0.24653)

data1 = c( 0.57014, 0.12014, 0.27083,
           0.07431, 0.11458, 0.04514,
           0.15278, 0.00347, 0.13542,
           0.14583, 0.12014, 0.08681,
           0.13889, 0.04861, 0.40347,
           0.14931, 0.02778, 0.12639,
           0.03333, 0.32639, 0.18403,
           0.08681, 0.64931, 0.70833,
           0.33681, 0.14931, 0.15625,
           0.03819, 0.01389, 0.24653,
           0.24653, 0.03819, 0.04514,
           0.29514, 0.46806, 0.01736,
           0.11944, 0.22222, 1.08889,
           0.05208, 0.29514, 0.05208,
           0.12500, 0.53472, 0.02778,
           0.25000, 0.15139, 0.03472,
           0.40069, 0.52569, 0.23611,
           0.02500, 0.07986, 0.35972)
data1[53]=data1[53]*0.01

#=============================================================
library(weibullness) 

a2 = 1-pnorm(3)
para = weibull.mle(data, threshold=0)
UCL.mle = qweibull(1-a2, shape=para$shape, scale=para$scale)
CL.mle = qweibull(0.50, shape=para$shape, scale=para$scale)
LCL.mle = qweibull(  a2, shape=para$shape, scale=para$scale)

para = weibull.wp(data)
UCL.wp  = qweibull(1-a2, shape=para$shape, scale=para$scale)
CL.wp = qweibull(0.50, shape=para$shape, scale=para$scale)
LCL.wp  = qweibull(  a2, shape=para$shape, scale=para$scale)

para = weibull.seki(data)
UCL.seki= qweibull(1-a2, shape=para$shape, scale=para$scale)
CL.seki= qweibull(0.50, shape=para$shape, scale=para$scale)
LCL.seki= qweibull(  a2, shape=para$shape, scale=para$scale)

para = weibull.e(data)
UCL.e   = qweibull(1-a2, shape=para$shape, scale=para$scale)
CL.e  = qweibull(0.50, shape=para$shape, scale=para$scale)
LCL.e   = qweibull(  a2, shape=para$shape, scale=para$scale)

para = weibull.wmed(data)
UCL.wmed = qweibull(1-a2, shape=para$shape, scale=para$scale)
CL.wmed = qweibull(0.50, shape=para$shape, scale=para$scale)
LCL.wmed = qweibull(  a2, shape=para$shape, scale=para$scale)

para = weibull.med1(data)
UCL.med1 = qweibull(1-a2, shape=para$shape, scale=para$scale)
CL.med1 = qweibull(0.50, shape=para$shape, scale=para$scale)
LCL.med1 = qweibull(  a2, shape=para$shape, scale=para$scale)

para = weibull.med2(data)
UCL.med2 = qweibull(1-a2, shape=para$shape, scale=para$scale)
CL.med2  = qweibull(0.50, shape=para$shape, scale=para$scale)
LCL.med2 = qweibull(  a2, shape=para$shape, scale=para$scale)

para = weibull.med3(data)
UCL.med3 = qweibull(1-a2, shape=para$shape, scale=para$scale)
CL.med3 = qweibull(0.50, shape=para$shape, scale=para$scale)
LCL.med3 = qweibull(  a2, shape=para$shape, scale=para$scale)
pdf(file = "Fig6.pdf", width = 7, height = 5.7)
ylim = c(0, 1.4)
#=============================================================

#par(mfrow=c(4,2), mar=c(5,5,3,1), omi=c(0,0,0,0), cex=0.5, mex=0.5)

plot(data1, xlab="Observation", ylab="Time Between Events", type="o", xlim=c(0,55), ylim=ylim, cex=1.5 )
abline( h = UCL.mle, col="orange red" , lwd=1.2)           ## MLE without noise 
abline( h =  CL.mle, col="orange red", lty=3 )    ## MLE without noise 
abline( h = LCL.mle, col="orange red")           ## MLE without noise 
abline(v= 30 , col="gold", lwd=1.2)
text(15,1.3,"Phase 1", cex=1.5)
text(44,1.3,"Phase 2", cex=1.5)
abline( h = UCL.med3, col="dark green", lwd=1.2)           ## MLE without noise 
abline( h =  CL.med3, col="dark green", lty=3)    ## MLE without noise 
abline( h = LCL.med3, col="dark green") 
text(1.7,1.17,"MLE", col="orange red", cex=1.5)
text(1.7,0.9,"med3", col="dark green", cex=1.5)
points(39,data1[39], col="red",lty=3)
points(53,data1[53], col="red",lty=3)
abline( h = UCL.med1, col="dark blue", lwd=1.2 )           ## MLE without noise 
abline( h =  CL.med1, col="dark blue", lty=3)    ## MLE without noise 
abline( h = LCL.med1, col="dark blue", ) 
text(1.7,1.07,"med1", col="dark blue", cex=1.5)

dev.off()