
##source("Rm16.R")
 source("https://raw.githubusercontent.com/namijeong/tar/main/Rm16.R")



library(weibullness)
set.seed(1)

n = 50
ITER = 1000
shape0 = 2; scale0 = 1
noise = 5   ################# REPEAT THIS with different noise level

SHAPE.mle = SHAPE.wp = SHAPE.wmed = SHAPE.med1 = SHAPE.med2 = SHAPE.med3 = numeric(ITER)
SCALE.mle = SCALE.wp = SCALE.wmed = SCALE.med1 = SCALE.med2 = SCALE.med3 = numeric(ITER)

SHAPE.e = SHAPE.seki = numeric(ITER)
SCALE.e = SCALE.seki = numeric(ITER)

for ( i in seq_len(ITER) ) { 
    data = rweibull(n, shape=shape0, scale=scale0)

#    Noise 
    data[1] = data[1] + noise 
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

#----------------------------------------------------------------
par( mfrow=c(4,2), mar=c(5,5,3,1), omi=c(0,0,0,0), cex=0.5,mex=0.5 )
 plot( SHAPE.mle, SCALE.mle, xlim=c(0,9), ylim=c(0,4) )
     abline(v=shape0, h=scale0, col="gold")
 plot( SHAPE.wp,  SCALE.wp,  xlim=c(0,9), ylim=c(0,4) )
     abline(v=shape0, h=scale0, col="gold")
 plot( SHAPE.seki,  SCALE.seki,  xlim=c(0,9), ylim=c(0,4) )
     abline(v=shape0, h=scale0, col="gold")
 plot( SHAPE.e,  SCALE.e,  xlim=c(0,9), ylim=c(0,4) )
     abline(v=shape0, h=scale0, col="gold")
 plot( SHAPE.wmed,  SCALE.wmed,  xlim=c(0,9), ylim=c(0,4) )
     abline(v=shape0, h=scale0, col="gold")
 plot( SHAPE.med1,  SCALE.med1,  xlim=c(0,9), ylim=c(0,4) )
     abline(v=shape0, h=scale0, col="gold")
 plot( SHAPE.med2,  SCALE.med2,  xlim=c(0,9), ylim=c(0,4) )
     abline(v=shape0, h=scale0, col="gold")
 plot( SHAPE.med3,  SCALE.med3,  xlim=c(0,9), ylim=c(0,4) )
     abline(v=shape0, h=scale0, col="gold")
#----------------------------------------------------------------

# Bias 
B.SHAPE1 = mean(SHAPE.mle) - shape0
B.SHAPE2 = mean(SHAPE.wp)  - shape0
B.SHAPE3 = mean(SHAPE.seki) - shape0
B.SHAPE4 = mean(SHAPE.e) - shape0
B.SHAPE5 = mean(SHAPE.wmed) - shape0
B.SHAPE6 = mean(SHAPE.med1) - shape0
B.SHAPE7 = mean(SHAPE.med2) - shape0
B.SHAPE8 = mean(SHAPE.med3) - shape0
         
B.SCALE1 = mean(SCALE.mle) - scale0
B.SCALE2 = mean(SCALE.wp)  - scale0
B.SCALE3 = mean(SCALE.seki) - scale0
B.SCALE4 = mean(SCALE.e) - scale0
B.SCALE5 = mean(SCALE.wmed) - scale0
B.SCALE6 = mean(SCALE.med1) - scale0
B.SCALE7 = mean(SCALE.med2) - scale0
B.SCALE8 = mean(SCALE.med3) - scale0

# Var
V.SHAPE1 = var(SHAPE.mle)         
V.SHAPE2 = var(SHAPE.wp)  
V.SHAPE3 = var(SHAPE.seki)  
V.SHAPE4 = var(SHAPE.e) 
V.SHAPE5 = var(SHAPE.wmed) 
V.SHAPE6 = var(SHAPE.med1) 
V.SHAPE7 = var(SHAPE.med2) 
V.SHAPE8 = var(SHAPE.med3) 
         
V.SCALE1 = var(SCALE.mle) 
V.SCALE2 = var(SCALE.wp)  
V.SCALE3 = var(SCALE.seki)  
V.SCALE4 = var(SCALE.e) 
V.SCALE5 = var(SCALE.wmed) 
V.SCALE6 = var(SCALE.med1) 
V.SCALE7 = var(SCALE.med2) 
V.SCALE8 = var(SCALE.med3) 


##### PLOT ################################################################################
par( mfrow=c(1,2), mar=c(5,5,3,1), omi=c(0,0,0,0), cex=0.5,mex=0.5 )

BIAS = c(B.SHAPE1, B.SHAPE2, B.SHAPE3, B.SHAPE4, B.SHAPE5, B.SHAPE6, B.SHAPE7, B.SHAPE8)
VAR  = c(V.SHAPE1, V.SHAPE2, V.SHAPE3, V.SHAPE4, V.SHAPE5, V.SHAPE6, V.SHAPE7, V.SHAPE8)
MSE = rbind(VAR, BIAS^2)
colnames(MSE) = c("MLE", "WP", "Seki", "e", "W.med", "med1", "med2", "med3")

barplot(MSE, xlab="(a) shape", ylab="MSE",
        col=c("cyan3","pink3"), beside=FALSE, border=NA,
        legend.text = c("Variance", "Squared Bias"), 
        args.legend = list(x = "topright", horiz=FALSE, bty="n", cex=1.5),
        cex.lab=1.5, cex.axis=1.25   )

rbind(VAR, BIAS^2, VAR+BIAS^2)

BIAS = c(B.SCALE1, B.SCALE2, B.SCALE3, B.SCALE4, B.SCALE5, B.SCALE6, B.SCALE7, B.SCALE8)
VAR  = c(V.SCALE1, V.SCALE2, V.SCALE3, V.SCALE4, V.SCALE5, V.SCALE6, V.SCALE7, V.SCALE8)
MSE = rbind(VAR, BIAS^2)
colnames(MSE) = c("MLE", "WP", "Seki", "e", "W.med", "med1", "med2", "med3")
barplot(MSE, xlab="(b) Scale", ylab="MSE",
        col=c("cyan3","pink3"), beside=FALSE, border=NA,
        legend.text = c("Variance", "Squared Bias"),
        args.legend = list(x = "topright", horiz=FALSE, bty="n", cex=1.5),
        cex.lab=1.5, cex.axis=1.25   )

rbind(VAR, BIAS^2, VAR+BIAS^2)
###########################################################################################





# MSE
(mean(SHAPE.mle) - shape0)^2  + var(SHAPE.mle)     
(mean(SHAPE.wp)  - shape0)^2  + var(SHAPE.wp)  
(mean(SHAPE.seki) - shape0)^2 + var(SHAPE.seki)  
(mean(SHAPE.e) - shape0)^2 + var(SHAPE.e) 
(mean(SHAPE.wmed) - shape0)^2 + var(SHAPE.wmed) 
(mean(SHAPE.med1) - shape0)^2 + var(SHAPE.med1) 
(mean(SHAPE.med2) - shape0)^2 + var(SHAPE.med2) 
(mean(SHAPE.med3) - shape0)^2 + var(SHAPE.med3) 
                                 
(mean(SCALE.mle) - scale0)^2  + var(SCALE.mle) 
(mean(SCALE.wp)  - scale0)^2  + var(SCALE.wp)  
(mean(SCALE.wmed) - scale0)^2 + var(SCALE.wmed) 
(mean(SCALE.med1) - scale0)^2 + var(SCALE.med1) 
(mean(SCALE.med2) - scale0)^2 + var(SCALE.med2) 
(mean(SCALE.med3) - scale0)^2 + var(SCALE.med3) 
(mean(SCALE.e) - scale0)^2 + var(SCALE.e) 


# MSE (general)
gMSE1 = MSE.gen(SHAPE.mle, SCALE.mle,     shape0, scale0)
gMSE2 = MSE.gen(SHAPE.wp,  SCALE.wp,      shape0, scale0)
gMSE3 = MSE.gen(SHAPE.seki,  SCALE.seki,  shape0, scale0)
gMSE4 = MSE.gen(SHAPE.e,     SCALE.e,     shape0, scale0)
gMSE5 = MSE.gen(SHAPE.wmed,  SCALE.wmed,  shape0, scale0)
gMSE6 = MSE.gen(SHAPE.med1,  SCALE.med1,  shape0, scale0)
gMSE7 = MSE.gen(SHAPE.med2,  SCALE.med2,  shape0, scale0)
gMSE8 = MSE.gen(SHAPE.med3,  SCALE.med3,  shape0, scale0)

gVar1 = det( cov( cbind(SHAPE.mle,   SCALE.mle ) ) )
gVar2 = det( cov( cbind(SHAPE.wp,    SCALE.wp  ) ) )
gVar3 = det( cov( cbind(SHAPE.seki,  SCALE.seki) ) )
gVar4 = det( cov( cbind(SHAPE.e,     SCALE.e   ) ) )
gVar5 = det( cov( cbind(SHAPE.wmed,  SCALE.wmed) ) )
gVar6 = det( cov( cbind(SHAPE.med1,  SCALE.med1) ) )
gVar7 = det( cov( cbind(SHAPE.med2,  SCALE.med2) ) )
gVar8 = det( cov( cbind(SHAPE.med3,  SCALE.med3) ) )

gBIASsq1 = gMSE1 - gVar1
gBIASsq2 = gMSE2 - gVar2
gBIASsq3 = gMSE3 - gVar3
gBIASsq4 = gMSE4 - gVar4
gBIASsq5 = gMSE5 - gVar5
gBIASsq6 = gMSE6 - gVar6
gBIASsq7 = gMSE7 - gVar7
gBIASsq8 = gMSE8 - gVar8


re1 = g/gMSE1*100
re2 = g/gMSE2*100
re3 = g/gMSE3*100
re4 = g/gMSE4*100
re5 = g/gMSE5*100
re6 = g/gMSE6*100
re7 = g/gMSE7*100
re8 = g/gMSE8*100


RE = c(re1, re2, re3, re4, re5, re6, re7, re8)
name= c("MLE", "WP", "Seki", "e", "W.med", "med1", "med2", "med3")
rbind(name, RE)

##### PLOT ################################################################################
par( mfrow=c(1,1), mar=c(5,5,3,1), omi=c(0,0,0,0), cex=0.5,mex=0.5 )

gBIASsq = c(gBIASsq1, gBIASsq2, gBIASsq3, gBIASsq4, gBIASsq5, gBIASsq6, gBIASsq7, gBIASsq8)
gVAR    = c(gVar1,    gVar2,    gVar3,    gVar4,    gVar5,    gVar6,    gVar7,    gVar8)
gMSE = rbind(gVAR, gBIASsq)

colnames(gMSE) = c("MLE", "WP", "Seki", "e", "W.med", "med1", "med2", "med3")

barplot(gMSE, xlab=NA, ylab="MSE",
        col=c("cyan3","pink3"), beside=FALSE, border=NA,
        legend.text = c("Generalized variance", "Generalized squared bias"),
        args.legend = list(x = "topright", horiz=FALSE, bty="n", cex=1.5),
        cex.lab=1.5, cex.axis=1.25   )

###########################################################################################
rbind(gVAR, gBIASsq,  apply(gMSE,2,sum) )






