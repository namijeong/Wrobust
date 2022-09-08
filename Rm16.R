#===================================================================
# Weighted median
#===================================================================
#--------------
# new version on Oct. 31. 2019
# NOTE: a = 10L; b = exp(log(10)) ;  c(a,b); a==b
#  tiny=.Machine$double.eps; abs(a-b)<tiny  # good idea, but does not wrok
#  tiny = sqrt(.Machine$double.eps); abs(a-b)<tiny # It works!
#--------------
weighted.median <- function(x, w, interpolation=0.5) { 
  # Preparation
  if (missing(w)) w = rep(1L,length(x))
  if (length(w) != length(x)) stop("'x' and 'w' must have the same length")

  x = as.double(as.vector(x))
  w = as.double(as.vector(w))
  ok= complete.cases(x,w); x=x[ok]; w=w[ok]

  stopifnot(all(w >= 0))
  if(all(w <= 0)) stop("All weights are zero", call.=FALSE)

  orderx = order(x)
  x = x[orderx]
  w = w[orderx] / sum(w)
  Fn = cumsum(w)
  tiny = sqrt(.Machine$double.eps)

  # Main part
  if ( all( abs(Fn-0.5)>tiny ) ) {  # any values of Fn is not 1/2.
      k = sum( Fn < 0.5 )
      return( x[k+1] )
  } else {
    k = which.min ( signif(abs(Fn-0.5),digits=12) ) # Find k with Fn=0.5
    if (w[k+1] < tiny) {   # check if w[k+1] == 0 
        return( x[k+1] )
    } else {
      return( (1-interpolation)*x[k] + interpolation*x[k+1] )
    }
  }
}
#===================================================================

#------------------------------
weibull.wmed = function(x, a=0.5) { 
   x = log(sort(x));
   y = log(-log(1-ppoints(x, a=a)))
   xtilde = median(x)
   ytilde = median(y)
   alpha.hat = weighted.median( (y-ytilde)/(x-xtilde), w = (x-xtilde)^2 )
   intercept = median(y-alpha.hat*x)
   beta.hat = exp(-intercept/alpha.hat)
   structure( list(shape=alpha.hat, scale=beta.hat),  class = "weibull.robust.estimate")
}
#------------------------------
weibull.med1 = function(x, a=0.5) {    # Breakdown point is only 29%
   x = log(sort(x));
   y = log(-log(1-ppoints(x, a=a)))
   DX = outer(x,x,"-"); DY = outer(y,y,"-")
   diag(DX) = NA
   idx = upper.tri(DY)
   alpha.hat = median( (DY[idx]/DX[idx]) )  # slope 
   intercept = median(y-alpha.hat*x) ## This intercept is better than the below. 
   ## intercept = median( apply((outer(x,y,"*")-outer(y,x,"*"))/DX, 2, median, na.rm=TRUE) )
   beta.hat = exp(-intercept/alpha.hat)
   structure( list(shape=alpha.hat, scale=beta.hat),  class = "weibull.robust.estimate")
}

#------------------------------
weibull.med2 = function(x, a=0.5) {    ## 50% breakdown point 
   x = log(sort(x)); 
   y = log(-log(1-ppoints(x, a=a)))

   Sxy = median( (x-median(x))*(y-median(y)) )
   Sxx = median( (x-median(x))^2 )

   alpha.hat = Sxy / Sxx
   intercept = median(y - alpha.hat*x)
   beta.hat = exp(-intercept/alpha.hat)
   structure( list(shape=alpha.hat, scale=beta.hat),  class = "weibull.robust.estimate")
}
#------------------------------
weibull.med3 = function(x, a=0.5) {    # 50% breakdown point
   x = log(sort(x));
   y = log(-log(1-ppoints(x, a=a)))

   DX = outer(x,x,"-"); DY = outer(y,y,"-")

   diag(DX) = NA

   Sxx = median( apply(DX^2, 2, median, na.rm=TRUE) )   # robust version of Sxx
   Sxy = median( apply(DX*DY,2, median, na.rm=TRUE) )   # robust version of Sxy

   alpha.hat = Sxy / Sxx
   intercept = median(y-alpha.hat*x) ## This intercept is better than the below. 
   ## intercept = median( apply((outer(x,y,"*")-outer(y,x,"*"))/DX, 2, median, na.rm=TRUE) )

   beta.hat = exp(-intercept/alpha.hat)
   structure( list(shape=alpha.hat, scale=beta.hat),  class = "weibull.robust.estimate")
}
#------------------------------
weibull.e = function(x, a=0.5) {    # 36.7% breakdown point
   scale = quantile(x, 1-exp(-1))
   shape = log(log(2)) / log(median(x)/scale)
   structure( list(shape=shape, scale=scale), class = "weibull.robust.estimate")
}
#------------------------------
weibull.seki  <- function(x) {
  qq = quantile(x, probs=c(0.31 ,0.63) )
  alpha = 1 / ( log(qq[2]/qq[1]) )
  theta = qq[2]
  structure( list ( shape=alpha, scale=theta ),  class = "weibull.robust.estimate")

}
#------------------------------


#------------------------------
MSE.gen = function(x,y, mux, muy) {
   N = length(x)
   a11 = sum( (x-mux)^2 )
   a22 = sum( (y-muy)^2 )
   a12 = sum( (x-mux)*(y-muy) )
   S = 1/N * matrix( c(a11,a12,a12,a22), nrow=2)
   det(S)
}
#------------------------------



#==========================================================
print.weibull.robust.estimate <-
function(x, digits = getOption("digits"), ...)
{ 
   ans = format(x, digits=digits)
   print(ans, quote=FALSE)
   invisible(x)
}
#==========================================================




