#source("C:/nnn/Rm16.R")
source("https://raw.githubusercontent.com/namijeong/tar/main/Rm16.R")
# DATA 
############################################################################################
# ?˜ˆ? œ 9.2 (ê´€ë¦¬ë„/?´ë¡ ê³¼ ? ?š©) page 184.

##install.packages("chron")

library(chron)

days  = dates(c(
"01/02/00", "01/10/00", "01/17/00", "01/21/00", "01/21/00",
"01/29/00", "02/04/00", "02/06/00", "02/07/00", "02/09/00",
"02/09/00", "02/11/00", "02/14/00", "02/14/00", "02/19/00",
"02/19/00", "02/20/00", "02/21/00", "02/25/00", "02/28/00",
"03/05/00", "03/06/00", "03/09/00", "03/10/00", "03/21/00",
"03/23/00", "03/28/00", "03/29/00", "04/10/00", "04/11/00",
"04/23/00", "04/29/00", "05/04/00", "05/11/00", "05/21/00",
"06/04/00", "06/10/00", "06/13/00", "06/17/00", "06/19/00",
"06/27/00", "06/30/00", "07/05/00", "07/11/00", "07/13/00",
"07/14/00", "07/15/00", "07/17/00", "07/22/00", "07/26/00",
"07/30/00", "08/05/00", "08/13/00", "08/25/00", "08/29/00",
"09/12/00", "09/14/00", "09/19/00", "09/20/00", "09/20/00",
"09/21/00", "09/22/00", "09/23/00", "09/24/00", "09/30/00",
"10/13/00", "10/18/00", "10/26/00", "11/04/00", "11/08/00",
"11/13/00", "11/14/00", "11/21/00", "12/03/00", "12/04/00",
"12/04/00", "12/05/00", "12/09/00", "12/12/00", "12/21/00",
"12/23/00", "12/29/00", "12/31/00" ))
times = times(c(
"12:00:00", "03:10:00", "11:33:00", "04:02:00", "09:00:00",
"14:59:00", "07:01:00", "08:17:00", "20:06:00", "04:58:00",
"22:53:00", "03:26:00", "00:44:00", "12:20:00", "08:31:00",
"11:18:00", "03:30:00", "22:27:00", "17:52:00", "14:32:00",
"11:39:00", "09:16:00", "06:13:00", "16:43:00", "10:23:00",
"14:07:00", "18:31:00", "08:43:00", "03:27:00", "23:21:00",
"20:30:00", "16:24:00", "04:16:00", "15:15:00", "17:34:00",
"01:36:00", "08:59:00", "18:58:00", "23:10:00", "12:50:00",
"01:15:00", "04:43:00", "18:55:00", "07:14:00", "05:10:00",
"18:03:00", "05:18:00", "22:16:00", "05:45:00", "09:24:00",
"19:59:00", "15:52:00", "06:04:00", "07:02:00", "23:24:00",
"13:40:00", "22:44:00", "22:19:00", "03:50:00", "20:06:00",
"00:31:00", "01:25:00", "17:34:00", "15:35:00", "16:44:00",
"18:40:00", "13:42:00", "09:16:00", "01:19:00", "06:35:00",
"05:42:00", "20:27:00", "20:43:00", "22:03:00", "01:27:00",
"02:19:00", "15:32:00", "21:02:00", "13:47:00", "19:38:00",
"22:03:00", "03:32:00", "20:12:00" ))
days.times = chron(dates.= days, times.=times)
days.times
data = as.numeric( diff(days.times) )
###############################################



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

ylim = c(0, 100)
#=============================================================
##pdf(file="CC-Sensitivity3.pdf", width=6.5, height=8.0, paper = "special", encoding = "TeXtext.enc")

par(mfrow=c(3,2), mar=c(5,5,3,1), omi=c(0,0,0,0), cex=0.5, mex=0.5)

plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )
lines(DELTA, UCL.mle, col="black", type="l", lwd=2)
lines(DELTA,  CL.mle, col="black", type="l", lwd=1, lty=3)
lines(DELTA, LCL.mle, col="black", type="l", lwd=2)
abline( h = UCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
abline( h =  CL.mle[n], col="red", lty=3, lwd=0.2)    ## MLE without noise 
abline( h = LCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
text(1e-04,90,labels='MLE',cex=2)

plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )
lines(DELTA, UCL.wp, col="black", type="l", lwd=2)
lines(DELTA,  CL.wp, col="black", type="l", lwd=1, lty=3)
lines(DELTA, LCL.wp, col="black", type="l", lwd=2)
abline( h = UCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
abline( h =  CL.mle[n], col="red", lty=3, lwd=0.2)    ## MLE without noise 
abline( h = LCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
text(1e-04,90,labels='WP',cex=2)

plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )
lines(DELTA, UCL.seki, col="black", type="l", lwd=2)
lines(DELTA,  CL.seki, col="black", type="l", lwd=1, lty=3)
lines(DELTA, LCL.seki, col="black", type="l", lwd=2)
abline( h = UCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
abline( h =  CL.mle[n], col="red", lty=3, lwd=0.2)    ## MLE without noise 
abline( h = LCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 

plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )
lines(DELTA, UCL.e, col="black", type="l", lwd=2)
lines(DELTA,  CL.e, col="black", type="l", lwd=1, lty=3)
lines(DELTA, LCL.e, col="black", type="l", lwd=2)
abline( h = UCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
abline( h =  CL.mle[n], col="red", lty=3, lwd=0.2)    ## MLE without noise 
abline( h = LCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 

plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )
lines(DELTA, UCL.wmed, col="black", type="l", lwd=2)
lines(DELTA,  CL.wmed, col="black", type="l", lwd=1, lty=3)
lines(DELTA, LCL.wmed, col="black", type="l", lwd=2)
abline( h = UCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
abline( h =  CL.mle[n], col="red", lty=3, lwd=0.2)    ## MLE without noise 
abline( h = LCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 

plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )
lines(DELTA, UCL.med1, col="black", type="l", lwd=2)
lines(DELTA,  CL.med1, col="black", type="l", lwd=1, lty=3)
lines(DELTA, LCL.med1, col="black", type="l", lwd=2)
abline( h = UCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
abline( h =  CL.mle[n], col="red", lty=3, lwd=0.2)    ## MLE without noise 
abline( h = LCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
text(1e-04,90,labels='med1',cex=2)

plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )
lines(DELTA, UCL.med2, col="black", type="l", lwd=2)
lines(DELTA,  CL.med2, col="black", type="l", lwd=1, lty=3)
lines(DELTA, LCL.med2, col="black", type="l", lwd=2)
abline( h = UCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
abline( h =  CL.mle[n], col="red", lty=3, lwd=0.2)    ## MLE without noise 
abline( h = LCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
text(1e-04,90,labels='med2',cex=2)

plot(NA, NA, xlab=expression(delta), log="x", ylab="CC", type="n", xlim=range(DELTA), ylim=ylim )
lines(DELTA, UCL.med3, col="black", type="l", lwd=2)
lines(DELTA,  CL.med3, col="black", type="l", lwd=1, lty=3)
lines(DELTA, LCL.med3, col="black", type="l", lwd=2)
abline( h = UCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
abline( h =  CL.mle[n], col="red", lty=3, lwd=0.2)    ## MLE without noise 
abline( h = LCL.mle[n], col="red", lwd=0.2)           ## MLE without noise 
text(1e-04,90,labels='med3',cex=2)


