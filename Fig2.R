source("C:/nnn/Rm16.R")
## source("S:/nnn/Rm16.R")



library(weibullness)
set.seed(1)

pdf(file="Fig2.pdf", width = 13.1, height = 4.9)

n = 20
ITER = 1000
shape0 = 2; scale0 = 1
noise = 1   ################# REPEAT THIS with different noise level

SHAPE.mle = SHAPE.wp = SHAPE.wmed = SHAPE.med1 = SHAPE.med2 = SHAPE.med3 = numeric(ITER)
SCALE.mle = SCALE.wp = SCALE.wmed = SCALE.med1 = SCALE.med2 = SCALE.med3 = numeric(ITER)

SHAPE.e = SHAPE.seki = numeric(ITER)
SCALE.e = SCALE.seki = numeric(ITER)

for ( i in seq_len(ITER) ) { 
  data = rweibull(n, shape=shape0, scale=scale0)
  
  #    Noise 
  data[1] = data[1] * noise 
  ## data[1] = data[1]/10000  ## slope method is sensitive to left outlier (close to zero).
  
  para = weibull.mle(data, threshold=0)
  SHAPE.mle[i] = para$shape;  SCALE.mle[i] = para$scale
  
  para = weibull.wp(data)
  SHAPE.wp[i] = para$shape;  SCALE.wp[i] = para$scale
  
  para = weibull.seki(data)
  SHAPE.seki[i] = para$shape;  SCALE.seki[i] = para$scale
  
  para = weibull.e(data)
  SHAPE.e[i] = para$shape;  SCALE.e[i] = para$scale
  
  para = weibull.wmed(data)
  SHAPE.wmed[i] = para$shape;  SCALE.wmed[i] = para$scale
  
  para = weibull.med1(data)
  SHAPE.med1[i] = para$shape;  SCALE.med1[i] = para$scale
  
  para = weibull.med2(data)
  SHAPE.med2[i] = para$shape;  SCALE.med2[i] = para$scale
  
  para = weibull.med3(data)
  SHAPE.med3[i] = para$shape;  SCALE.med3[i] = para$scale
}
par( mfrow=c(1,3), mar=c(7,5,3,1), omi=c(0,0,0,0), cex=1.0,mex=0.5 )

gBIASsq = c(gBIASsq1, gBIASsq2, gBIASsq3, gBIASsq4, gBIASsq5, gBIASsq6, gBIASsq7, gBIASsq8)
gVAR    = c(gVar1,    gVar2,    gVar3,    gVar4,    gVar5,    gVar6,    gVar7,    gVar8)
gMSE = rbind(gVAR, gBIASsq)

colnames(gMSE) = c("MLE", "WP", "Seki", "e", "W.med", "med1", "med2", "med3")

barplot(gMSE, xlab=NA, ylab="gMSE",ylim=c(0,0.20),  sub="(i)",
        col=c("cyan1","red3"), beside=FALSE, angle=c(0,45), density=c(10,10),
        legend.text = c("Generalized variance", "Generalized squared bias"),
        args.legend = list(x = "topright", horiz=FALSE, bty="n", cex=0.8),
        cex.lab=1, cex.axis=1  )

noise = 10   ################# REPEAT THIS with different noise level

SHAPE.mle = SHAPE.wp = SHAPE.wmed = SHAPE.med1 = SHAPE.med2 = SHAPE.med3 = numeric(ITER)
SCALE.mle = SCALE.wp = SCALE.wmed = SCALE.med1 = SCALE.med2 = SCALE.med3 = numeric(ITER)

SHAPE.e = SHAPE.seki = numeric(ITER)
SCALE.e = SCALE.seki = numeric(ITER)

for ( i in seq_len(ITER) ) { 
  data = rweibull(n, shape=shape0, scale=scale0)
  
  #    Noise 
  data[1] = data[1] * noise 
  ## data[1] = data[1]/10000  ## slope method is sensitive to left outlier (close to zero).
  
  para = weibull.mle(data, threshold=0)
  SHAPE.mle[i] = para$shape;  SCALE.mle[i] = para$scale
  
  para = weibull.wp(data)
  SHAPE.wp[i] = para$shape;  SCALE.wp[i] = para$scale
  
  para = weibull.seki(data)
  SHAPE.seki[i] = para$shape;  SCALE.seki[i] = para$scale
  
  para = weibull.e(data)
  SHAPE.e[i] = para$shape;  SCALE.e[i] = para$scale
  
  para = weibull.wmed(data)
  SHAPE.wmed[i] = para$shape;  SCALE.wmed[i] = para$scale
  
  para = weibull.med1(data)
  SHAPE.med1[i] = para$shape;  SCALE.med1[i] = para$scale
  
  para = weibull.med2(data)
  SHAPE.med2[i] = para$shape;  SCALE.med2[i] = para$scale
  
  para = weibull.med3(data)
  SHAPE.med3[i] = para$shape;  SCALE.med3[i] = para$scale
}


gBIASsq = c(gBIASsq1, gBIASsq2, gBIASsq3, gBIASsq4, gBIASsq5, gBIASsq6, gBIASsq7, gBIASsq8)
gVAR    = c(gVar1,    gVar2,    gVar3,    gVar4,    gVar5,    gVar6,    gVar7,    gVar8)
gMSE = rbind(gVAR, gBIASsq)

colnames(gMSE) = c("MLE", "WP", "Seki", "e", "W.med", "med1", "med2", "med3")

barplot(gMSE, xlab=NA, ylab="gMSE",ylim=c(0,0.20),  sub="(ii)",
        col=c("cyan1","red3"), beside=FALSE, angle=c(0,45), density=c(10,10),
        legend.text = c("Generalized variance", "Generalized squared bias"),
        args.legend = list(x = "topright", horiz=FALSE, bty="n", cex=0.8),
        cex.lab=1, cex.axis=1   )
noise = 1/100   ################# REPEAT THIS with different noise level

SHAPE.mle = SHAPE.wp = SHAPE.wmed = SHAPE.med1 = SHAPE.med2 = SHAPE.med3 = numeric(ITER)
SCALE.mle = SCALE.wp = SCALE.wmed = SCALE.med1 = SCALE.med2 = SCALE.med3 = numeric(ITER)

SHAPE.e = SHAPE.seki = numeric(ITER)
SCALE.e = SCALE.seki = numeric(ITER)

for ( i in seq_len(ITER) ) { 
  data = rweibull(n, shape=shape0, scale=scale0)
  
  #    Noise 
  data[1] = data[1] * noise 
  ## data[1] = data[1]/10000  ## slope method is sensitive to left outlier (close to zero).
  
  para = weibull.mle(data, threshold=0)
  SHAPE.mle[i] = para$shape;  SCALE.mle[i] = para$scale
  
  para = weibull.wp(data)
  SHAPE.wp[i] = para$shape;  SCALE.wp[i] = para$scale
  
  para = weibull.seki(data)
  SHAPE.seki[i] = para$shape;  SCALE.seki[i] = para$scale
  
  para = weibull.e(data)
  SHAPE.e[i] = para$shape;  SCALE.e[i] = para$scale
  
  para = weibull.wmed(data)
  SHAPE.wmed[i] = para$shape;  SCALE.wmed[i] = para$scale
  
  para = weibull.med1(data)
  SHAPE.med1[i] = para$shape;  SCALE.med1[i] = para$scale
  
  para = weibull.med2(data)
  SHAPE.med2[i] = para$shape;  SCALE.med2[i] = para$scale
  
  para = weibull.med3(data)
  SHAPE.med3[i] = para$shape;  SCALE.med3[i] = para$scale
}

gBIASsq = c(gBIASsq1, gBIASsq2, gBIASsq3, gBIASsq4, gBIASsq5, gBIASsq6, gBIASsq7, gBIASsq8)
gVAR    = c(gVar1,    gVar2,    gVar3,    gVar4,    gVar5,    gVar6,    gVar7,    gVar8)
gMSE = rbind(gVAR, gBIASsq)

colnames(gMSE) = c("MLE", "WP", "Seki", "e", "W.med", "med1", "med2", "med3")

barplot(gMSE, xlab=NA, ylab="gMSE",ylim=c(0,0.20),  sub="(iii)",
        col=c("cyan1","red3"), beside=FALSE, angle=c(0,45), density=c(10,10),
        legend.text = c("Generalized variance", "Generalized squared bias"),
        args.legend = list(x = "topright", horiz=FALSE, bty="n", cex=0.8),
        cex.lab=1, cex.axis=1   )

dev.off()


