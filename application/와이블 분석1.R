
source("https://raw.githubusercontent.com/namijeong/main/Rm16.R")
## source("Rm16.R")
##source("C:/nnn/Rm16.R")
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
MF=function(x, shape,scale){
  scale*gamma(1+1/shape)
}

MF(x, shape=weibull.mle(data)$shape, scale=weibull.mle(data)$scale)
MF(x, shape=weibull.wp(data)$shape, scale=weibull.wp(data)$scale)
MF(x, shape=weibull.seki(data)$shape, scale=weibull.seki(data)$scale)
MF(x, shape=weibull.e(data)$shape, scale=weibull.e(data)$scale)
MF(x, shape=weibull.wmed(data)$shape, scale=weibull.wmed(data)$scale)
MF(x, shape=weibull.med1(data)$shape, scale=weibull.med1(data)$scale)
MF(x, shape=weibull.med2(data)$shape, scale=weibull.med2(data)$scale)
MF(x, shape=weibull.med3(data)$shape, scale=weibull.med3(data)$scale)









