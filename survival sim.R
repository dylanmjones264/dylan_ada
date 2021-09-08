install.packages('survival')
library('ggplot2')
library('simsurv')
library('survival')
rm(list = ls())
N <- 1000
covs <- data.frame(id=1:N,trt = rbinom(N,1,.5))
t <- data.frame(id=1:N)
pars <- c(trt=-1.5)
times <- simsurv(dist = 'weibull',lambdas = .1,gammas = 1.5,x=covs,betas = pars)
df <-merge(covs,times)
df$trt <- factor(df$trt)
cumd1 <- ecdf(df$eventtime[df$trt==1])
cumd0 <- ecdf(df$eventtime[df$trt==0])
x <- seq(0,5,by=.1)
y0 <- cumd0(x)
y1 <- cumd1(x)
plot(x,y0,type = 'l')
lines(x,y1)

str(df)
ggplot(df,aes(x=eventtime))+geom_density()
plot(ecdf(log(df$eventtime)))
ggplot(df,aes(x=eventtime))+stat_ecdf(geom = 'line')




coxph(Surv(eventtime,status)~trt,data = df)
sr <-survreg(Surv(eventtime,status)~trt,data = df,dist = 'weibull')
sr
1/0.6624697


