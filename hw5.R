setwd("C:/users/11231/Desktop/stat424")

library(knitr)
dat14 = data.frame(A = rep(c(-1,1),times=2),
                   B = rep(rep(c(-1,1),each=2),times=1),
                   I = c(14.037,13.880,14.821,14.888),
                   II = c(16.165,13.860,14.757,14.921),
                   III = c(13.972,14.032,14.843,14.415),
                   IV = c(13.907,13.914,14.878,14.932))
kable(dat14)
library(tidyverse)
dat14<-dat14%>%
  pivot_longer(cols = c(I,II,III,IV),names_to="replicates",values_to="thickness")
fit14<-lm(thickness~A*B, data=dat14)
anova(fit14)
fit14$coefficients
dat14<-dat14%>%
  mutate(residual = fit14$residuals,
         fit.val = fit14$fitted.values)
dat14%>%ggplot(aes(sample=residual))+geom_qq()+geom_qq_line()
dat14%>%ggplot(aes(x=fit.val,y=residual))+geom_point()


nd<-expand.grid(A = seq(from = -1, to = 1, by = .1),
                B = c(-1,1))
nd$predTDS<-predict(fit14,newdata = nd)
ggplot(data=nd,aes(x=A, y = B, z = predTDS))+geom_contour_filled()


dat32 = data.frame(A = rep(c(-1,1),times=8),
                   B = rep(rep(c(-1,1),each=2),times=4),
                   C = rep(rep(c(-1,1),each=4),times = 2),
                   D = rep(c(-1,1),each=8),
                   roughness=c(0.00340,0.00362,0.00301,0.00182,
                                 0.00280,0.00290,0.00252,0.00160,
                                 0.00336,0.00344,0.00308,0.00184,
                                 0.00269,0.00284,0.00253,0.00163))
m32<-lm(roughness~A*B*C*D, data = dat32)
effects<-2*coef(m32)[-1]
daniel_plot<-function(effects,p=c(0.3,0.7)){
  qq<-qqnorm(effects, datax = TRUE)
  qqline(effects,col="red",probs=p,datax=TRUE)
  text(qq$x,qq$y,names(effects),pos=1)
}
daniel_plot(effects)
m32$coefficients
f1<-lm(roughness~A+B+C+A*B, data=dat32)
anova(f1)
plot(f1)
dat32$residuals<-resid(f1)
ggplot(dat32, aes(x=roughness, y=residuals))+geom_point()
dat32c = data.frame(A = rep(c(-1,1),times=8),
                    B = rep(rep(c(-1,1),each=2),times=4),
                    C = rep(rep(c(-1,1),each=4),times = 2),
                    D = rep(c(-1,1),each=8),
                    roughness=1/c(0.00340,0.00362,0.00301,0.00182,
                                0.00280,0.00290,0.00252,0.00160,
                                0.00336,0.00344,0.00308,0.00184,
                                0.00269,0.00284,0.00253,0.00163))
m32c<-lm(roughness~A*B*C*D, data = dat32c)
effects<-2*coef(m32c)[-1]
daniel_plot<-function(effects,p=c(0.3,0.7)){qq<-qqnorm(effects, datax = TRUE)
  qqline(effects,col="red",probs=p,datax=TRUE)
  text(qq$x,qq$y,names(effects),pos=1)}
daniel_plot(effects)
m32c$coefficients
f2<-lm(roughness~A+B+C+A*B, data=dat32c)
anova(f2)
plot(f2)
dat32c$residuals<-resid(f2)
ggplot(dat32c, aes(x=roughness, y=residuals))+geom_point()


dat2 = data.frame(
  A = rep(c(-1,1),times=2),B = rep(rep(c(-1,1),each=2),times=1),
  label=c("(1)","a","b","ab"),I = c(18.2,27.2,15.9,41.0),
  II = c(18.9,24.0,14.5,43.9),III = c(12.9,22.4,15.1,36.3),
  IV = c(14.4,22.5,14.2,39.9))
dat2<-pivot_longer(dat2,
                   cols=c('I','II','III','IV'),
                   names_to = 'replicate',
                   values_to='vibration')
dat2$block=factor(rep(c(1,2,3,4),times=4))
kable(dat2)
fit2 = lm(vibration~block+A+B+A*B, dat2)
anova(fit2)

