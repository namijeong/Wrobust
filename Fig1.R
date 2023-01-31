
## source("Rm16.R")
source("C:/nnn/Rm16.R")



library(weibullness)
set.seed(2)

n = 20
ITER = 200
shape0 = 2  ; scale0 = 1
noise = 0   ################# REPEAT THIS with different noise level

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
pdf(file = "Fig1.pdf", width = 10.2, height = 11)
noise = 10   ################# REPEAT THIS with different noise level

SHAPE.mle1 = SHAPE.wp1 = SHAPE.wmed1 = SHAPE.med11 = SHAPE.med21 = SHAPE.med31 = numeric(ITER)
SCALE.mle1 = SCALE.wp1 = SCALE.wmed1 = SCALE.med11 = SCALE.med21 = SCALE.med31 = numeric(ITER)

SHAPE.e1 = SHAPE.seki1 = numeric(ITER)
SCALE.e1 = SCALE.seki1 = numeric(ITER)

for ( i in seq_len(ITER) ) { 
  data = rweibull(n, shape=shape0, scale=scale0)
  
  #    Noise 
  data[1] = data[1] * noise 
  ## data[1] = data[1]/10000  ## slope method is sensitive to left outlier (close to zero).
  
  para = weibull.mle(data, threshold=0)
  SHAPE.mle1[i] = para$shape;  SCALE.mle1[i] = para$scale
  
  para = weibull.wp(data)
  SHAPE.wp1[i] = para$shape;  SCALE.wp1[i] = para$scale
  
  para = weibull.seki(data)
  SHAPE.seki1[i] = para$shape;  SCALE.seki1[i] = para$scale
  
  para = weibull.e(data)
  SHAPE.e1[i] = para$shape;  SCALE.e1[i] = para$scale
  
  para = weibull.wmed(data)
  SHAPE.wmed1[i] = para$shape;  SCALE.wmed1[i] = para$scale
  
  para = weibull.med1(data)
  SHAPE.med11[i] = para$shape;  SCALE.med11[i] = para$scale
  
  para = weibull.med2(data)
  SHAPE.med21[i] = para$shape;  SCALE.med21[i] = para$scale
  
  para = weibull.med3(data)
  SHAPE.med31[i] = para$shape;  SCALE.med31[i] = para$scale
}

noise =  100  ################# REPEAT THIS with different noise level

SHAPE.mle11 = SHAPE.wp11 = SHAPE.wmed11 = SHAPE.med111 = SHAPE.med211 = SHAPE.med311 = numeric(ITER)
SCALE.mle11 = SCALE.wp11 = SCALE.wmed11 = SCALE.med111 = SCALE.med211 = SCALE.med311 = numeric(ITER)

SHAPE.e11 = SHAPE.seki11 = numeric(ITER)
SCALE.e11 = SCALE.seki11 = numeric(ITER)

for ( i in seq_len(ITER) ) { 
  data = rweibull(n, shape=shape0, scale=scale0)
  
  #    Noise 
  data[1] = data[1] / noise 
  ## data[1] = data[1]/10000  ## slope method is sensitive to left outlier (close to zero).
  
  para = weibull.mle(data, threshold=0)
  SHAPE.mle11[i] = para$shape;  SCALE.mle11[i] = para$scale
  
  para = weibull.wp(data)
  SHAPE.wp11[i] = para$shape;  SCALE.wp11[i] = para$scale
  
  para = weibull.seki(data)
  SHAPE.seki11[i] = para$shape;  SCALE.seki11[i] = para$scale
  
  para = weibull.e(data)
  SHAPE.e11[i] = para$shape;  SCALE.e11[i] = para$scale
  
  para = weibull.wmed(data)
  SHAPE.wmed11[i] = para$shape;  SCALE.wmed11[i] = para$scale
  
  para = weibull.med1(data)
  SHAPE.med111[i] = para$shape;  SCALE.med111[i] = para$scale
  
  para = weibull.med2(data)
  SHAPE.med211[i] = para$shape;  SCALE.med211[i] = para$scale
  
  para = weibull.med3(data)
  SHAPE.med311[i] = para$shape;  SCALE.med311[i] = para$scale
}

#----------------------------------------------------------------
par( mfrow=c(4,2), mar=c(5,5,3,1), omi=c(0,0,0,0), cex=0.7,mex=0.6 )
plot( SHAPE.mle, SCALE.mle, xlim=c(0,5), ylim=c(0,3), col="dark gray", pch=3)
title(main= "MLE")
points( SHAPE.mle1, SCALE.mle1,  col="red", pch=4)
points( SHAPE.mle11, SCALE.mle11,  col="dark cyan", pch=4)
abline(v=shape0, h=scale0, col="gray")

plot( SHAPE.wp,  SCALE.wp,  xlim=c(0,5), ylim=c(0,3), col="dark gray", pch=3)
title(main= "WP")
points(  SHAPE.wp1,  SCALE.wp1,  col="red", pch=4)
points(  SHAPE.wp11,  SCALE.wp11,  col="dark cyan", pch=4)
abline(v=shape0, h=scale0, col="gray")

plot( SHAPE.seki,  SCALE.seki,  xlim=c(0,5), ylim=c(0,3), col="dark gray", pch=3) 
title(main= "Seki")
points( SHAPE.seki1,  SCALE.seki1,  col="red", pch=4  ) 
points( SHAPE.seki11,  SCALE.seki11,  col="dark cyan", pch=4  ) 
abline(v=shape0, h=scale0, col="gray")

plot( SHAPE.e,  SCALE.e,  xlim=c(0,5), ylim=c(0,3), col="dark gray", pch=3) 
title(main= "e")
points( SHAPE.e1,  SCALE.e1,  col="red", pch=4  )
points( SHAPE.e11,  SCALE.e11,  col="dark cyan", pch=4  )
abline(v=shape0, h=scale0, col="gray")

plot( SHAPE.wmed,  SCALE.wmed,  xlim=c(0,5), ylim=c(0,3), col="dark gray", pch=3) 
title(main= "W.med")
points(  SHAPE.wmed1,  SCALE.wmed1,  col="red", pch=4  )
points(  SHAPE.wmed11,  SCALE.wmed11,  col="dark cyan", pch=4  )
abline(v=shape0, h=scale0, col="gray")

plot( SHAPE.med1,  SCALE.med1,  xlim=c(0,5), ylim=c(0,3), col="dark gray", pch=3) 
title(main= "med1")
points( SHAPE.med11,  SCALE.med11,  col="red", pch=4  )
points( SHAPE.med111,  SCALE.med111,  col="dark cyan", pch=4  )
abline(v=shape0, h=scale0, col="gray")

plot( SHAPE.med2,  SCALE.med2,  xlim=c(0,5), ylim=c(0,3), col="dark gray", pch=3)
title(main="med2")
points(SHAPE.med21,  SCALE.med21,  col="red", pch=4  )
points(SHAPE.med211,  SCALE.med211,  col="dark cyan", pch=4  )
abline(v=shape0, h=scale0, col="gray")

plot( SHAPE.med3,  SCALE.med3,  xlim=c(0,5), ylim=c(0,3), col="dark gray", pch=3) 
title(main= "med3")
points(SHAPE.med31,  SCALE.med31,  col="red", pch=4  )
points(SHAPE.med311,  SCALE.med311,  col="dark cyan", pch=4  )
abline(v=shape0, h=scale0, col="gray")
#----------------------------------------------------------------

dev.off()
