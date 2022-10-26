
##source("https://raw.githubusercontent.com/AppliedStat/SCD/master/SCD.R")
## source("Rm16.R")
source("https://raw.githubusercontent.com/namijeong/tar/main/Rm16.R")
#------------------------------
library(weibullness)

n=10000
shape0 = 3; scale0 = 1
    # Pure data
 data = rweibull(n, shape=shape0, scale=scale0)
 #data = 1000* c(100,  90,  59, 117, 177,  98, 125, 118,  99, 132, 97, 87, 126, 107, 66, 186,  158,  80,  69, 109)
 #data = c(15.1 ,12.2 ,17.3, 14.3 , 7.9 ,18.2 ,24.6 ,13.5, 10.0, 30.5)
 weibull.mle(data, threshold=0)
   weibull.wp(data)
    
    weibull.seki(data)
weibull.e(data)
   weibull.wmed(data)
     weibull.med1(data)
    weibull.med2(data)
   weibull.med3(data)
   
    #   # Noise
   n=10000
   shape0 = 3; scale0 = 1
   # Pure data
   data = rweibull(n, shape=shape0, scale=scale0)
   #data = 1000* c(100,  90,  59, 117, 177,  98, 125, 118,  99, 132, 97, 87, 126, 107, 66, 186,  158,  80,  69, 109)
   
 
   noise = 0.0001
    data[1] = data[1] * noise 

  weibull.mle(data, threshold=0)
     weibull.wp(data)
  weibull.e(data)
 weibull.seki(data)
 weibull.wmed(data)
   weibull.med1(data)
  weibull.med2(data)
weibull.med3(data)
  

hist(data)
#----------------------------------------------------------------
layout(mat = matrix(c(2, 4, 1, 3), nrow = 2, ncol = 2, byrow=TRUE), 
       heights = c(1, 2), # Heights of the two rows
       widths  = c(2, 1)) # Widths of the two columns
## layout.show(3)

par(mar=c(5,5,0,0),omi=c(0,0,0,0),cex=0.5,mex=0.5)
 plot (SHAPE.mle0, SCALE.mle0, xlim=c(0,5), ylim=c(0,200000), pch=20, col="cyan4", 
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



