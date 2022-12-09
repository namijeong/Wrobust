
##source("https://raw.githubusercontent.com/AppliedStat/SCD/master/SCD.R")
## source("Rm16.R")
source("https://raw.githubusercontent.com/namijeong/Wrobust/main/Rm16.R")
#------------------------------

library(weibullness)
set.seed(1)
n = 20
ITER = 200
shape0 = 2; scale0 = 1
noise = 10

SHAPE.mle0 = SHAPE.wp0 = SHAPE.wmed0 = SHAPE.e0 = numeric(ITER)
SCALE.mle0 = SCALE.wp0 = SCALE.wmed0 = SCALE.e0 = numeric(ITER)
SHAPE.med10= SHAPE.med20= SHAPE.med30= SHAPE.seki0 = numeric(ITER)
SCALE.med10= SCALE.med20= SCALE.med30= SCALE.seki0 = numeric(ITER)

SHAPE.mle1 = SHAPE.wp1 = SHAPE.wmed1 = SHAPE.e1 = numeric(ITER)
SCALE.mle1 = SCALE.wp1 = SCALE.wmed1 = SCALE.e1 = numeric(ITER)
SHAPE.med11= SHAPE.med21= SHAPE.med31= SHAPE.seki1 = numeric(ITER)
SCALE.med11= SCALE.med21= SCALE.med31= SCALE.seki1 = numeric(ITER)

for ( i in seq_len(ITER) ) { 
    # Pure data
    data = rweibull(n, shape=shape0, scale=scale0)

    para = weibull.mle(data, threshold=0)
    SHAPE.mle0[i] = para$shape;  
    SCALE.mle0[i] = para$scale

    para = weibull.wp(data)
    SHAPE.wp0[i] = para$shape;  
    SCALE.wp0[i] = para$scale

    para = weibull.e(data)
    SHAPE.e0[i] = para$shape;  
    SCALE.e0[i] = para$scale

    para = weibull.seki(data)
    SHAPE.seki0[i] = para$shape;
    SCALE.seki0[i] = para$scale

    para = weibull.wmed(data)
    SHAPE.wmed0[i] = para$shape;  
    SCALE.wmed0[i] = para$scale

    para = weibull.med1(data)
    SHAPE.med10[i] = para$shape; 
    SCALE.med10[i] = para$scale

    para = weibull.med2(data)
    SHAPE.med20[i] = para$shape;
    SCALE.med20[i] = para$scale

    para = weibull.med3(data)
    SHAPE.med30[i] = para$shape;
    SCALE.med30[i] = para$scale

    # Noise 
    data[1] = data[1] + noise 

    para = weibull.mle(data, threshold=0)
    SHAPE.mle1[i] = para$shape;  
    SCALE.mle1[i] = para$scale

    para = weibull.wp(data)
    SHAPE.wp1[i] = para$shape;  
    SCALE.wp1[i] = para$scale

    para = weibull.e(data)
    SHAPE.e1[i] = para$shape;  
    SCALE.e1[i] = para$scale

    para = weibull.seki(data)
    SHAPE.seki1[i] = para$shape;  
    SCALE.seki1[i] = para$scale

    para = weibull.wmed(data)
    SHAPE.wmed1[i] = para$shape;  
    SCALE.wmed1[i] = para$scale

    para = weibull.med1(data)
    SHAPE.med11[i] = para$shape; 
    SCALE.med11[i] = para$scale

    para = weibull.med2(data)
    SHAPE.med21[i] = para$shape; 
    SCALE.med21[i] = para$scale

    para = weibull.med3(data)
    SHAPE.med31[i] = para$shape; 
    SCALE.med31[i] = para$scale
}

#----------------------------------------------------------------
layout(mat = matrix(c(2, 4, 1, 3), nrow = 2, ncol = 2, byrow=TRUE), 
       heights = c(1, 2), # Heights of the two rows
       widths  = c(2, 1)) # Widths of the two columns
## layout.show(3)

par(mar=c(5,5,0,0),omi=c(0,0,0,0),cex=0.5,mex=0.5)
 plot (SHAPE.mle0, SCALE.mle0, xlim=c(0,5), ylim=c(0,2), pch=20, col="cyan4", 
       xlab=expression(lambda), ylab=expression(theta))
points(SHAPE.mle1, SCALE.mle1, pch=3, col="red")
abline(v=shape0, h=scale0, col="cyan2")

boxplot(SHAPE.mle0, SHAPE.mle1, notch=T, frame=F, horizontal=T, lwd=0.5, xaxt="n", yaxt="n", outline=F,
        ylim=c(0,6), col=c("cyan4", "red") )
boxplot(SCALE.mle0, SCALE.mle1, notch=T, frame=F, horizontal=F, lwd=0.5, xaxt="n", yaxt="n", outline=F,
        ylim=c(0,2), col=c("cyan4", "red") )
plot(NA,NA, frame=F, xlab=NA, ylab=NA, xaxt="n", yaxt="n", xlim=c(0,1), ylim=c(0,1))
legend(0, 0.5, bty="n", legend=c("No contamination", "Contamination"), pch=c(20,3), col=c("cyan4","red") )
text(0.3, 0.7, "Method (MLE)", cex=2)


#----------------------------------------------------------------
layout(mat = matrix(c(2, 4, 1, 3), nrow = 2, ncol = 2, byrow=TRUE),
       heights = c(1, 2), # Heights of the two rows
       widths  = c(2, 1)) # Widths of the two columns
## layout.show(3)

par(mar=c(5,5,0,0),omi=c(0,0,0,0),cex=0.5,mex=0.5)
 plot (SHAPE.wp0, SCALE.wp0, xlim=c(0,5), ylim=c(0,2), pch=20, col="cyan4",
       xlab=expression(lambda), ylab=expression(theta))
points(SHAPE.wp1, SCALE.wp1, pch=3, col="red")
abline(v=shape0, h=scale0, col="cyan2")

boxplot(SHAPE.wp0, SHAPE.wp1, notch=T, frame=F, horizontal=T, lwd=0.5, xaxt="n", yaxt="n", outline=F,
        ylim=c(0,6), col=c("cyan4", "red") )
boxplot(SCALE.wp0, SCALE.wp1, notch=T, frame=F, horizontal=F, lwd=0.5, xaxt="n", yaxt="n", outline=F,
        ylim=c(0,2), col=c("cyan4", "red") )
plot(NA,NA, frame=F, xlab=NA, ylab=NA, xaxt="n", yaxt="n", xlim=c(0,1), ylim=c(0,1))
legend(0, 0.5, bty="n", legend=c("No contamination", "Contamination"), pch=c(20,3), col=c("cyan4","red") )
text(0.3, 0.7, "Method (WP)", cex=2)


#----------------------------------------------------------------
layout(mat = matrix(c(2, 4, 1, 3), nrow = 2, ncol = 2, byrow=TRUE),
       heights = c(1, 2), # Heights of the two rows
       widths  = c(2, 1)) # Widths of the two columns
## layout.show(3)

par(mar=c(5,5,0,0),omi=c(0,0,0,0),cex=0.5,mex=0.5)
 plot (SHAPE.seki0, SCALE.seki0, xlim=c(0,5), ylim=c(0,2), pch=20, col="cyan4",
       xlab=expression(lambda), ylab=expression(theta))
points(SHAPE.seki1, SCALE.seki1, pch=3, col="red")
abline(v=shape0, h=scale0, col="cyan2")

boxplot(SHAPE.seki0, SHAPE.seki1, notch=T, frame=F, horizontal=T, lwd=0.5, xaxt="n", yaxt="n", outline=F,
        ylim=c(0,6), col=c("cyan4", "red") )
boxplot(SCALE.seki0, SCALE.seki1, notch=T, frame=F, horizontal=F, lwd=0.5, xaxt="n", yaxt="n", outline=F,
        ylim=c(0,2), col=c("cyan4", "red") )
plot(NA,NA, frame=F, xlab=NA, ylab=NA, xaxt="n", yaxt="n", xlim=c(0,1), ylim=c(0,1))
legend(0, 0.5, bty="n", legend=c("No contamination", "Contamination"), pch=c(20,3), col=c("cyan4","red") )
text(0.3, 0.7, "Method (Seki)", cex=2)


#----------------------------------------------------------------
layout(mat = matrix(c(2, 4, 1, 3), nrow = 2, ncol = 2, byrow=TRUE), 
       heights = c(1, 2), # Heights of the two rows
       widths  = c(2, 1)) # Widths of the two columns
## layout.show(3)

par(mar=c(5,5,0,0),omi=c(0,0,0,0),cex=0.5,mex=0.5)
 plot (SHAPE.e0, SCALE.e0, xlim=c(0,5), ylim=c(0,2), pch=20, col="cyan4",
       xlab=expression(lambda), ylab=expression(theta))
points(SHAPE.e1, SCALE.e1, pch=3, col="red")
abline(v=shape0, h=scale0, col="cyan2")

boxplot(SHAPE.e0, SHAPE.e1, notch=T, frame=F, horizontal=T, lwd=0.5, xaxt="n", yaxt="n", outline=F,
        ylim=c(0,6), col=c("cyan4", "red") )
boxplot(SCALE.e0, SCALE.e1, notch=T, frame=F, horizontal=F, lwd=0.5, xaxt="n", yaxt="n", outline=F,
        ylim=c(0,2), col=c("cyan4", "red") )
plot(NA,NA, frame=F, xlab=NA, ylab=NA, xaxt="n", yaxt="n", xlim=c(0,1), ylim=c(0,1))
legend(0, 0.5, bty="n", legend=c("No contamination", "Contamination"), pch=c(20,3), col=c("cyan4","red") )
text(0.2, 0.7, "Method (e)", cex=2)


#----------------------------------------------------------------
layout(mat = matrix(c(2, 4, 1, 3), nrow = 2, ncol = 2, byrow=TRUE), 
       heights = c(1, 2), # Heights of the two rows
       widths  = c(2, 1)) # Widths of the two columns
## layout.show(3)

par(mar=c(5,5,0,0),omi=c(0,0,0,0),cex=0.5,mex=0.5)
 plot (SHAPE.wmed0, SCALE.wmed0, xlim=c(0,5), ylim=c(0,2), pch=20, col="cyan4",
       xlab=expression(lambda), ylab=expression(theta))
points(SHAPE.wmed1, SCALE.wmed1, pch=3, col="red")
abline(v=shape0, h=scale0, col="cyan2")

boxplot(SHAPE.wmed0, SHAPE.wmed1, notch=T, frame=F, horizontal=T, lwd=0.5, xaxt="n", yaxt="n", outline=F,
        ylim=c(0,6), col=c("cyan4", "red") )
boxplot(SCALE.wmed0, SCALE.wmed1, notch=T, frame=F, horizontal=F, lwd=0.5, xaxt="n", yaxt="n", outline=F,
        ylim=c(0,2), col=c("cyan4", "red") )
plot(NA,NA, frame=F, xlab=NA, ylab=NA, xaxt="n", yaxt="n", xlim=c(0,1), ylim=c(0,1))
legend(0, 0.5, bty="n", legend=c("No contamination", "Contamination"), pch=c(20,3), col=c("cyan4","red") )
text(0.45, 0.7, "Method(weighted median)", cex=2)

#----------------------------------------------------------------
layout(mat = matrix(c(2, 4, 1, 3), nrow = 2, ncol = 2, byrow=TRUE), 
       heights = c(1, 2), # Heights of the two rows
       widths  = c(2, 1)) # Widths of the two columns
## layout.show(3)

par(mar=c(5,5,0,0),omi=c(0,0,0,0),cex=0.5,mex=0.5)
 plot (SHAPE.med10, SCALE.med10, xlim=c(0,5), ylim=c(0,2), pch=20, col="cyan4",
       xlab=expression(lambda), ylab=expression(theta))
points(SHAPE.med11, SCALE.med11, pch=3, col="red")
abline(v=shape0, h=scale0, col="cyan2")

boxplot(SHAPE.med10, SHAPE.med11, notch=T, frame=F, horizontal=T, lwd=0.5, xaxt="n", yaxt="n", outline=F,
        ylim=c(0,6), col=c("cyan4", "red") )
boxplot(SCALE.med10, SCALE.med11, notch=T, frame=F, horizontal=F, lwd=0.5, xaxt="n", yaxt="n", outline=F,
        ylim=c(0,2), col=c("cyan4", "red") )
plot(NA,NA, frame=F, xlab=NA, ylab=NA, xaxt="n", yaxt="n", xlim=c(0,1), ylim=c(0,1))
legend(0, 0.5, bty="n", legend=c("No contamination", "Contamination"), pch=c(20,3), col=c("cyan4","red") )
text(0.2, 0.7, "Method 1", cex=2)

#----------------------------------------------------------------
layout(mat = matrix(c(2, 4, 1, 3), nrow = 2, ncol = 2, byrow=TRUE), 
       heights = c(1, 2), # Heights of the two rows
       widths  = c(2, 1)) # Widths of the two columns
## layout.show(3)

par(mar=c(5,5,0,0),omi=c(0,0,0,0),cex=0.5,mex=0.5)
 plot (SHAPE.med20, SCALE.med20, xlim=c(0,5), ylim=c(0,2), pch=20, col="cyan4",
       xlab=expression(lambda), ylab=expression(theta))
points(SHAPE.med21, SCALE.med21, pch=3, col="red")
abline(v=shape0, h=scale0, col="cyan2")

boxplot(SHAPE.med20, SHAPE.med21, notch=T, frame=F, horizontal=T, lwd=0.5, xaxt="n", yaxt="n", outline=F,
        ylim=c(0,6), col=c("cyan4", "red") )
boxplot(SCALE.med20, SCALE.med21, notch=T, frame=F, horizontal=F, lwd=0.5, xaxt="n", yaxt="n", outline=F,
        ylim=c(0,2), col=c("cyan4", "red") )
plot(NA,NA, frame=F, xlab=NA, ylab=NA, xaxt="n", yaxt="n", xlim=c(0,1), ylim=c(0,1))
legend(0, 0.5, bty="n", legend=c("No contamination", "Contamination"), pch=c(20,3), col=c("cyan4","red") )
text(0.2, 0.7, "Method 2", cex=2)

#----------------------------------------------------------------
layout(mat = matrix(c(2, 4, 1, 3), nrow = 2, ncol = 2, byrow=TRUE), 
       heights = c(1, 2), # Heights of the two rows
       widths  = c(2, 1)) # Widths of the two columns
## layout.show(3)

par(mar=c(5,5,0,0),omi=c(0,0,0,0),cex=0.5,mex=0.5)
 plot (SHAPE.med30, SCALE.med30, xlim=c(0,5), ylim=c(0,2), pch=20, col="cyan4",
       xlab=expression(lambda), ylab=expression(theta))
points(SHAPE.med31, SCALE.med31, pch=3, col="red")
abline(v=shape0, h=scale0, col="cyan2")

boxplot(SHAPE.med30, SHAPE.med31, notch=T, frame=F, horizontal=T, lwd=0.5, xaxt="n", yaxt="n", outline=F,
        ylim=c(0,6), col=c("cyan4", "red") )
boxplot(SCALE.med30, SCALE.med31, notch=T, frame=F, horizontal=F, lwd=0.5, xaxt="n", yaxt="n", outline=F,
        ylim=c(0,2), col=c("cyan4", "red") )
plot(NA,NA, frame=F, xlab=NA, ylab=NA, xaxt="n", yaxt="n", xlim=c(0,1), ylim=c(0,1) )
legend(0, 0.5, bty="n", legend=c("No contamination", "Contamination"), pch=c(20,3), col=c("cyan4","red") )
text(0.2, 0.7, "Method 3", cex=2)



