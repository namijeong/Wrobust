
#source("https://raw.githubusercontent.com/namijeong/tar/blob/main/Rm16.R")
## source("Rm16.R")
source("C:/nnn/Rm16.R")
#------------------------------
library(weibullness)


 # Pure data
 data = 1000* c(100,  90,  59, 117, 177,  98, 125, 118,  99, 132, 97, 87, 126, 107, 66, 186,  158,  80,  69, 109)
 #data = c(15.1 ,12.2 ,17.3, 14.3 , 7.9 ,18.2 ,24.6 ,13.5, 10.0, 30.5)
 weibull.mle(data, threshold=0)
 weibull.wp(data)
 weibull.e(data)
 weibull.seki(data)
 weibull.wmed(data)
 weibull.med1(data)
 weibull.med2(data)
 weibull.med3(data)
   
#MTTF
MF=function(shape,scale){
  scale*gamma(1+1/shape)
}

MF( shape=weibull.mle(data,0)$shape, scale=weibull.mle(data,0)$scale)
MF( shape=weibull.wp(data)$shape, scale=weibull.wp(data)$scale)
MF( shape=weibull.seki(data)$shape, scale=weibull.seki(data)$scale)
MF( shape=weibull.e(data)$shape, scale=weibull.e(data)$scale)
MF( shape=weibull.wmed(data)$shape, scale=weibull.wmed(data)$scale)
MF( shape=weibull.med1(data)$shape, scale=weibull.med1(data)$scale)
MF( shape=weibull.med2(data)$shape, scale=weibull.med2(data)$scale)
MF( shape=weibull.med3(data)$shape, scale=weibull.med3(data)$scale)

#Blife
prob = 0.1

1 - (1-qweibull(prob, shape=weibull.mle(data,0)$shape, scale=weibull.mle(data,0)$scale))
1 - (1-qweibull(prob, shape=weibull.wp(data)$shape, scale=weibull.wp(data)$scale))
1 - (1-qweibull(prob, shape=weibull.seki(data)$shape, scale=weibull.seki(data)$scale))
1 - (1-qweibull(prob, shape=weibull.e(data)$shape, scale=weibull.e(data)$scale))
1 - (1-qweibull(prob, shape=weibull.wmed(data)$shape, scale=weibull.wmed(data)$scale))
1 - (1-qweibull(prob, shape=weibull.med1(data)$shape, scale=weibull.med1(data)$scale))
1 - (1-qweibull(prob, shape=weibull.med2(data)$shape, scale=weibull.med2(data)$scale))
1 - (1-qweibull(prob, shape=weibull.med3(data)$shape, scale=weibull.med3(data)$scale))






