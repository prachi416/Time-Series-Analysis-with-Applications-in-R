# Chapter 6

library(TSA)
#Exhibit 6.5
data(ma1.1.s)
win.graph(width=4.875,height=3,pointsize=8)
acf(ma1.1.s,xaxp=c(0,20,10))
acf(ma1.1.s)
#Exhibit 6.6
acf(ma1.1.s,ci.type='ma',xaxp=c(0,20,10))
#Exhibit 6.7
data(ma1.2.s); acf(ma1.2.s,xaxp=c(0,20,10))
#Exhibit 6.8
data(ma2.s); acf(ma2.s,xaxp=c(0,20,10))
#Exhibit 6.9
acf(ma2.s,ci.type='ma',xaxp=c(0,20,10))
#Exhibit 6.10
data(ar1.s); 
acf(ar1.s,xaxp=c(0,20,10))
#Exhibit 6.11
pacf(ar1.s,xaxp=c(0,20,10))
#Exhibit 6.12
library(TSA)
data(ar2.s)

acf(ar2.s,xaxp=c(0,20,10))
#Exhibit 6.13
pacf(ar2.s,xaxp=c(0,20,10))
#Exhibit 6.14
data(arma11.s)
plot(arma11.s, type='o',ylab=expression(Y[t]))
#Exhibit 6.15
acf(arma11.s,xaxp=c(0,20,10))
#Exhibit 6.16
pacf(arma11.s,xaxp=c(0,20,10))
#Exhibit 6.17
eacf(arma11.s)
#Exhibit 6.18
data(oil.price)
acf(as.vector(oil.price),xaxp=c(0,24,12))
#Exhibit 6.19
acf(diff(as.vector(log(oil.price))),xaxp=c(0,24,12))
#Exhibit 6.20
data(rwalk)
acf(diff(rwalk,difference=2),ci.type='ma', xaxp=c(0,18,9))
#Exhibit 6.21
acf(diff(rwalk),ci.type='ma',xaxp=c(0,18,9))
ar(diff(rwalk))
library(uroot)
library(tseries)
adf.test(rwalk)
#Exhibit 6.22 
set.seed(92397)
test=arima.sim(model=list(ar=c(rep(0,11),.8),
                          ma=c(rep(0,11),0.7)),n=120)
res= TSA::armasubsets(y=test,nar=14,nma=14,y.name='test', ar.method='ols')
plot(res)
title(main="Exhibit 6.22", line=6)
#Exhibit 6.23
data(larain); 
win.graph(width=2.5,height=2.5,pointsize=8)
qqnorm(log(larain)); qqline(log(larain))
qqline(log(larain))
#Exhibit 6.24
win.graph(width=4.875,height=3,pointsize=8)
acf(log(larain),xaxp=c(0,20,10))
#Exhibit 6.25
data(color); 
acf(color,ci.type='ma')
#Exhibit 6.26
pacf(color)
#Exhibit 6.27
win.graph(width=3,height=3,pointsize=8)
data(hare); 
bxh = BoxCox.ar(hare)
bxh$mle
bxh$ci
#Exhibit 6.28
acf(hare^.5)
#Exhibit 6.29
pacf(hare^.5)
#Exhibit 6.30
eacf(diff(log(oil.price)))
#Exhibit 6.31
res=armasubsets(y=diff(log(oil.price)),nar=7,nma=7,
                y.name='test', ar.method='ols')
plot(res)
#Exhibit 6.32
acf(as.vector(diff(log(oil.price))),xaxp=c(0,22,11))
#Exhibit 6.33
pacf(as.vector(diff(log(oil.price))),xaxp=c(0,22,11))

# Chapter 7

library(TSA)
estimate.ma1.mom=function(x){r=acf(x,plot=F)$acf[1]; if (abs(r)<0.5) 
  return((-1+sqrt(1-4*r^2))/(2*r)) else return(NA)}
#Exhibit 7.1
data(ma1.2.s)
estimate.ma1.mom(ma1.2.s)
data(ma1.1.s)
estimate.ma1.mom(ma1.1.s)
set.seed(1234)
ma1.3.s=arima.sim(list(ma=c(-0.9)),n=60)
estimate.ma1.mom(ma1.3.s)
set.seed(200)
ma1.4.s=arima.sim(list(ma=c(0.5)),n=60) 
estimate.ma1.mom(ma1.4.s)
arima(ma1.4.s,order=c(0,0,1),method='CSS',include.mean=F)
data(ar1.s); 
data(ar1.2.s)
ar(ar1.s,order.max=1,AIC=F,method='yw')
ar(ar1.2.s,order.max=1,AIC=F,method='yw')
data(ar2.s)
ar(ar2.s,order.max=2,AIC=F,method='yw')
#Exhibit 7.4
data(ar1.s); 
data(ar1.2.s)
ar(ar1.s,order.max=1,AIC=F,method='yw')
ar(ar1.s,order.max=1,AIC=F,method='ols')
ar(ar1.s,order.max=1,AIC=F,method='mle')
ar(ar1.2.s,order.max=1,AIC=F,method='yw')
ar(ar1.2.s,order.max=1,AIC=F,method='ols')
ar(ar1.2.s,order.max=1,AIC=F,method='mle')
#Exhibit 7.5
data(ar2.s)
ar(ar2.s,order.max=2,AIC=F,method='yw')
ar(ar2.s,order.max=2,AIC=F,method='ols')
ar(ar2.s,order.max=2,AIC=F,method='mle')
#Exhibit 7.6
data(arma11.s)
arima(arma11.s, order=c(1,0,1),method='CSS')
arima(arma11.s, order=c(1,0,1),method='ML')
#Exhibit 7.7
data(color)
ar(color,order.max=1,AIC=F,method='yw')
ar(color,order.max=1,AIC=F,method='ols')
ar(color,order.max=1,AIC=F,method='mle')
#Exhibit 7.8
data(hare)
arima(sqrt(hare),order=c(3,0,0))
#Exhibit 7.9
data(oil.price)
arima(log(oil.price),order=c(0,1,1),method='CSS')
arima(log(oil.price),order=c(0,1,1),method='ML')
#Exhibit 7.10
res=arima(sqrt(hare),order=c(3,0,0),include.mean=T)
set.seed(12345)
coefm.cond.norm=arima.boot(res,cond.boot=T,is.normal=T,B=1000,init=sqrt(hare))
signif(apply(coefm.cond.norm,2,function(x){quantile(x,c(.025,.975),na.rm=T)}),3)
# Method I
coefm.cond.replace=arima.boot(res,cond.boot=T,is.normal=F,B=1000,init=sqrt(hare))
signif(apply(coefm.cond.replace,2,function(x){quantile(x,c(.025,.975),na.rm=T)}),3)
# Method II
coefm.norm=arima.boot(res,cond.boot=F,is.normal=T,ntrans=100,B=1000,init=sqrt(hare))
signif(apply(coefm.norm,2,function(x){quantile(x,c(.025,.975),na.rm=T)}),3)
# Method III
coefm.replace=arima.boot(res,cond.boot=F,is.normal=F,ntrans=100,B=1000,init=sqrt(hare))
signif(apply(coefm.replace,2,function(x){quantile(x,c(.025,.975),na.rm=T)}),3)
# Method IV
dim(coefm.replace)
# The theoretical confidence intervals were computed by the output in Exhibit 7.8.
period.replace=apply(coefm.replace,1,function(x){
  roots=polyroot(c(1,-x[1:3]))
  min1=1.e+9
  rootc=NA
  for (root in roots) {
    if( abs(Im(root))<1e-10) next
    if (Mod(root)< min1) {min1=Mod(root); rootc=root}
  }
  if(is.na(rootc)) period=NA else period=2*pi/abs(Arg(rootc))
  period
})
#number of bootstap series that do not admit a  well-defined quasi-period.
sum(is.na(period.replace)) 
quantile(period.replace, c(.025,.975),na.rm=T)
#Exhibit 7.11
win.graph(width=3.9,height=3.8,pointsize=8)
hist(period.replace,prob=T,main="",xlab="quasi-period",axes=F,xlim=c(5,16))
axis(2)
axis(1,c(4,6,8,10,12,14,16),c(4,6,8,10,12,14,NA))
#Exhibit 7.12
win.graph(width=3,height=3,pointsize=8)
qqnorm(period.replace,main="") 
#Normal Q-Q Plot for the Bootstrap Quasi-period Estimates")
qqline(period.replace)

# Chapter 8

#Exhibit 8.1
win.graph(width=4.875, height=3,pointsize=8)
data(color)
m1.color=arima(color,order=c(1,0,0))
m1.color
plot(rstandard(m1.color),ylab='Standardized residuals',type='b')
abline(h=0)
#Exhibit 8.2
data(hare)
m1.hare=arima(sqrt(hare),order=c(3,0,0))
m1.hare 
m2.hare=arima(sqrt(hare),order=c(3,0,0),fixed=c(NA,0,NA,NA)) 
m2.hare
plot(rstandard(m2.hare),ylab='Standardized residuals',type='b')
abline(h=0)
#Exhibit 8.3
data(oil.price)
m1.oil=arima(log(oil.price),order=c(0,1,1))
plot(rstandard(m1.oil),ylab='Standardized residuals',type='l')
abline(h=0)
#Exhibit 8.4
win.graph(width=3, height=3,pointsize=8)
qqnorm(residuals(m1.color))
qqline(residuals(m1.color))
#Exhibit 8.5
qqnorm(residuals(m1.hare))
qqline(residuals(m1.hare))
#Exhibit 8.6
qqnorm(residuals(m1.oil))
qqline(residuals(m1.oil))
#Exhibit 8.9
win.graph(width=4.875, height=3,pointsize=8)
acf(residuals(m1.color),main='Sample ACF of Residuals from AR(1) Model for Color')
#Exhibit 8.10
acf(residuals(arima(sqrt(hare),order=c(2,0,0))),main='Sample ACF of Residuals from AR(2) Model for Hare
')
#Exhibit 8.11
acf(residuals(m1.color),plot=F)$acf
signif(acf(residuals(m1.color),plot=F)$acf[1:6],2)
#Exhibit 8.12 
win.graph(width=4.875, height=4.5)
tsdiag(m1.color,gof=15,omit.initial=F)
#Exhibit 8.13
m1.color 
# Exhibit 8.14
m2.color=arima(color,order=c(2,0,0))
m2.color
#Exhibit 8.15
m3.color=arima(color,order=c(1,0,1))
m3.color
#Exhibit 8.16
m4.color=arima(color,order=c(2,0,1))
m4.color

# Problems Chaper 6
#6.20a
n <- 48
phi <- 0.7
theoretical_autocorr_1 <- phi
theoretical_autocorr_5 <- phi^5
theoretical_autocorr_1
theoretical_autocorr_5

#6.20b
set.seed(0)
Y <- arima.sim(model = list(ar = 0.7), n = 48)
lags = acf(Y, lag.max=5)
lags
se_ar1 <- function(phi, k, n) {
  sqrt(((1 + phi**2) * (1 - phi**(2*k)) / (1 - phi**2) - 2 * k * phi**(2*k)) / n)
}
t1 = 0.7
t5 = 0.7^5
s1 = se_ar1(0.7, 1, 48)
s5 = se_ar1(0.7, 5, 48)
print(c('Standard deviation at lag 1: ', round(s1, digits=3)), quote=F)
print(c('Standard deviation at lag 5: ', round(s5, digits=3)), quote=F)
r1 = lags$acf[2]
r5 = lags$acf[6]
print(c('Standard deviation distance on lag 1 measurement: ', round(((r1 - t1) / s1), digits=2)), quote=F)
print(c('Standard deviation distance on lag 5 measurement: ', round(((r5 - t5) / s5), digits=2)), quote=F)
#The first estimate is extremely close to the theoretical value, while the second estimate is less than 2 standard deviations away from the theoretical value.

#6.20c
B = 100000
set.seed(0)
r1=rep(NA, B); r5=r1

for (k in 1:B) {
  Yk = arima.sim(model=list(ar=0.7), n=48)
  lags = acf(Yk, lag.max=5, plot=F)
  r1[k] = lags$acf[2]
  r5[k] = lags$acf[6]
}
hist(r1, breaks=100)
hist(r5, breaks=100)
#The actual distribution of r1 and r5 approximately follow the theoretical distribution estimates -- with a large variance.

#6.21a
n1 <- 60
theta <- 0.5
theoretical_autocorr <- -(theta/(1 + theta^2))
theoretical_autocorr

#6.21b
set.seed(14)
Y = arima.sim(model=list(ma=-0.5), n=60)
lags = acf(Y, lag.max=1)
lags
se_ma1 <- function(theta, k, n) {
  rho = -theta / (1 + theta**2)
  if (k == 0) {
    sqrt((1 - 3 * rho**2 + 4 * rho**4)/n)
  } else {
    sqrt((1 + 2 * rho**2)/n)
  }
}
t1 = -0.5 / (1 + 0.5**2)
s1 = se_ma1(theta=0.5, k=1, n=60)
print(c('Standard deviation at lag 1: ', round(s1, digits=3)), quote=F)
r1 = lags$acf[2]
print(c('Standard deviation distance on lag 1 measurement: ', round(((r1 - t1) / s1), digits=2)), quote=F)
#The sample autocorrelation is close to the theoretical value. It is less than one standard deviation away.

#6.21c
B = 100000
set.seed(0)
r1=rep(NA, B)

for (k in 1:B) {
  Yk = arima.sim(model=list(ma=-0.5), n=60)
  lags = acf(Yk, lag.max=1, plot=F)
  r1[k] = lags$acf[2]
}
hist(r1, breaks=100)

#6.25a
n <- 36
phi <- 0.7
#rho = phi^k for k>1
set.seed(0)
plot_acf <- function(acf, title='ACF') {
  max_k = length(acf)
  k = seq(1, max_k)
  
  plot(k, acf, type='h', lwd=2, col='blue', xlab='Lag', ylab='Autocorrelation', 
       main=title)
  abline(h=0, lty=2, col='blue')
}

set.seed(0)
Y <- arima.sim(model=list(ar=0.7), n=36)
theoretical_acf <- ARMAacf(ar=0.7, lag.max=10)[-1]
plot_acf(theoretical_acf, title='Theoretical ACF for AR(1)')

#6.25b
plot_acf(acf(Y, lag.max=10, plot=F)$acf[-1], title='Sample ACF for AR(1)')
#The sample ACF are still  noisy, but it is a good fit at lower lags.

#6.25c
#phi(11) = phi = 0.7 & phi(kk) = 0 for k > 1
lag_max <- 10
theoretical_pacf <- ARMAacf(ar = 0.7, lag.max = lag_max, pacf = TRUE)
print(theoretical_pacf)
theoretical_pacf <- ARMAacf(ar = 0.7, lag.max = lag_max, pacf = TRUE)[-1]
print(theoretical_pacf)

#6.25d
plot_acf_with_bounds <- function(acf, lower, upper, title='ACF') {
  max_k = length(acf)
  k = seq(1, max_k)
  
  options(repr.plot.width=12, repr.plot.height=4)
  p = ggplot() + geom_ribbon(aes(x=k, ymin=lower, ymax=upper), fill='blue', alpha=0.1) +
    geom_point(aes(x=k, y=acf), color='blue')
  for (i in 1:max_k) {
    p = p + geom_line(aes_string(x=k[i], y=c(0, acf[i])), color='blue')
  }
  p + geom_line(aes(x=k, y=0), color='blue', linetype='dashed') +
    xlab('Lag') + ylab('rho') +
    ggtitle(title) +
    theme_bw() + theme(text = element_text(size=16), plot.title = element_text(hjust = 0.5))
}
a <- acf(Y, lag.max=10, plot=F)$acf[-1]
rho <- (0.7)^(1:10)
se <- sapply(1:10, function(k) { se_ar1(0.7, k, 36) })
min_length <- min(length(a), length(rho - 2 * se), length(rho + 2 * se))
a <- a[1:min_length]
rho <- rho[1:min_length]
lower <- rho - 2 * se[1:min_length]
upper <- rho + 2 * se[1:min_length]
plot_acf_with_bounds(a, lower, upper, title='Sample ACF for AR(1)')

#6.25e
a = pacf(Y, lag.max=10, plot=F)$acf
rho = c(0.7, rep(0, 9))
se = 1 / sqrt(36)
plot_acf_with_bounds(a, rho - 2 * se, rho + 2 * se, title='Sample PACF for AR(1)')

#6.36a
library(ggplot2)
data(robot)
Y <- robot
plot(robot, type = "l", xlab = "Index", ylab = "Distance (inches)", main = "Time Series Plot of Robot Data")
#This series does appear stationary, though there is perhaps some "drift" in the later half.

#6.36b
acf_robot <- acf(robot, lag.max = 20, plot = FALSE)
plot(acf_robot, main = "Sample ACF of Robot Data")
pacf_robot <- pacf(robot, lag.max = 20, plot = FALSE)
plot(pacf_robot, main = "Sample PACF of Robot Data")
#The PACF plot above suggests an AR(3) model for the series.

#6.36c
eacf_result <- eacf(Y)
#The EACF suggests an ARMA(1, 1) process for the series.

#6.37
data(larain)
larain_log <- log(larain)
eacf_result <- eacf(larain_log)
#Yes EACF suggests this is a white noise process.

# Problems Chaper 8

#8.4a
set.seed(100)
Y = arima.sim(model=list(ar=0.5), n=30)
model = arima(Y, order=c(1,0,0), method='ML')
plot(1:length(model$resid), model$resid, type = "l", col = "blue", xlab = "", ylab = "Residuals")

#8.4b
options(repr.plot.width=12, repr.plot.height=8)
qqnorm(model$resid)
qqline(model$resid, col = "red")
#Tails look suspect on both ends
shapiro.test(model$resid)
#The test fails to reject normality.

#8.4c
plot_acf_with_bounds <- function(acf, lower, upper, title='ACF') {
  max_k <- length(acf)
  k <- seq(1, max_k)
  
  options(repr.plot.width=12, repr.plot.height=4)
  p <- ggplot() + geom_ribbon(aes(x=k, ymin=lower, ymax=upper), fill='blue', alpha=0.1) +
    geom_point(aes(x=k, y=acf), color='blue')
  for (i in 1:max_k) {
    p <- p + geom_line(aes_string(x=k[i], y=c(0, acf[i])), color='blue')
  }
  p <- p + geom_line(aes(x=k, y=0), color='blue', linetype='dashed') +
    xlab('Lag') + ylab('Residuals\n') +
    ggtitle(title) +
    theme_bw() + theme(text = element_text(size=16), plot.title = element_text(hjust = 0.5))
  print(p)
}

r <- acf(model$resid, lag.max=15, plot=F)$acf[-1]
se <- 1 / sqrt(length(Y))
plot_acf_with_bounds(r, - 2 * se, + 2*se, title='Sample ACF for residuals')
#ACF indicates residuals are random.

#8.4d
ljung_box_test <- function(r, n, k_ar = 0, k_ma = 0) {
  nlags <- length(r)
  denominators <- (n - 1):(n - nlags)
  Qstar <- n * (n - 2) * sum((r^2) / denominators)
  df <- nlags - k_ar - k_ma
  p_value <- 1 - pchisq(Qstar, df = df)
  
  list('X-squared' = Qstar, df = df, 'p-value' = p_value)
}
ljung_box_test_result <- ljung_box_test(acf(model$resid, lag.max = 8, plot = FALSE)$acf[-1], n = length(Y), k_ar = 1)
ljung_box_test_result
#The test does not reject randomness of error based on the first 8 autocorrelations.

#8.5a
set.seed(2000)
Y = arima.sim(model=list(ma=-0.5), n=36)
model = arima(Y, order=c(0,0,1), method='ML')
plot(1:length(model$resid), model$resid, type = "l", col = "blue", xlab = "", ylab = "Residuals")

#8.5b
options(repr.plot.width=12, repr.plot.height=8)
qqnorm(model$resid)
qqline(model$resid, col = "red")
#The residuals appear to be normally distributed, although there might be some deviation in the first point of the lower tail. Despite this observation, let's proceed with a Shapiro-Wilk test to formally assess the normality of the residuals.
shapiro.test(model$resid)
#The Shapiro-Wilk test fails to reject normality.

#8.5c
r = acf(model$resid, lag.max=15, plot=F)$acf[-1]
se = 1 / sqrt(length(Y))
plot_acf_with_bounds(r, - 2 * se, + 2*se, title='Sample ACF for residuals')
#ACF suggests the residuals are white noise.

#8.5d
ljung_box_test <- function(r, n, k_ar = 0, k_ma = 0) {
  nlags <- length(r)
  denominators <- (n - 1):(n - nlags)
  Qstar <- n * (n - 2) * sum((r^2) / denominators)
  df <- nlags - k_ar - k_ma
  p_value <- 1 - pchisq(Qstar, df = df)
  
  list('X-squared' = Qstar, 'df' = df, 'p-value' = p_value)
}
ljung_box_test(acf(model$resid, lag.max=6, plot=F)$acf[-1], n=length(Y), k_ma=1)
#The test does not reject randomness of error based on the first 6 autocorrelations.





acf(model$residuals)

# Step 6: Calculate Ljung-Box statistic
lag_max <- 6
lb_test <- Box.test(model$residuals, lag = lag_max, type = "Ljung-Box")

# Step 7: Check Ljung-Box statistic
lb_statistic <- lb_test$statistic
critical_value <- qchisq(0.95, df = lag_max)
if (lb_statistic > critical_value) {
  print("The Ljung-Box statistic suggests significant autocorrelation, indicating a lack of fit for the MA(1) model.")
} else {
  print("The Ljung-Box statistic does not suggest significant autocorrelation, supporting the MA(1) specification.")
}

