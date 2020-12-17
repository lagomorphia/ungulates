#import data
data<-read.table("C:\\Users\\Tim\\Documents\\R\\BEHAVE.csv", header=T, sep=",")

names(data)
head(data)
nrow(data)
data

hist(data$ROS)
treatment<-factor(data$treatment)

m1<-lm(ROS~treatment+map+mat+time_since_excl, data=data)
summary(m1)

wt<-(data$ROS)^.7
hist(wt)

data$ROS<-(data$ROS + 10)^.7
hist(data$ROS)

#run simple regression with transformed response variable
m1a<-lm(data$ROS~treatment+map+mat+time_since_excl, data=data)

summary(m1a)
hist(resid(m1a))

#Run a mixed model using nlme package

library(nlme)

#Same fixed effects (predictors) as above, but now with Site as a random effect

mm1<-lme(fixed = FL~treatment+map+mat+time_since_excl, data=data, random = ~ 1 | Site)
summary(mm1)

#Explained deviance (more or less like R^2) - proposed by the author of the nlme packge (Pinheiro)
cor(fitted(mm1), getResponse(mm1))^2    


#try with interaction term (allows slope also to change with respect to co-variate (treatment)

mm1a<-lme(fixed = ROS~treatment + map + treatment:map + mat + time_since_excl, data=data, random = ~ 1 | Site)
summary(mm1a)

#Explained deviance
cor(fitted(mm1a), getResponse(mm1a))^2 

#Multi model selection
#Run models with all combinations of predictors
#model with ALL variables is global model
#Compare all combination with the null model (ie no predictor)

mm1a<-lme(fixed = ROS~treatment + map + treatment:map + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm2a<-lme(fixed = ROS~treatment + map + treatment:map + mat, data=data, random = ~ 1 | Site)
mm3a<-lme(fixed = ROS~treatment + map + treatment:map + time_since_excl, data=data, random = ~ 1 | Site)
mm4a<-lme(fixed = ROS~treatment + map + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm5a<-lme(fixed = ROS~treatment + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm6a<-lme(fixed = ROS~ map  + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm7a<-lme(fixed = ROS~treatment + time_since_excl, data=data, random = ~ 1 | Site)
mm8a<-lme(fixed = ROS~treatment + mat , data=data, random = ~ 1 | Site)
mm9a<-lme(fixed = ROS~ map + mat, data=data, random = ~ 1 | Site)
mm10a<-lme(fixed = ROS~ map + time_since_excl, data=data, random = ~ 1 | Site)
mm11a<-lme(fixed = ROS~treatment + map + treatment:map, data=data, random = ~ 1 | Site)
mm12a<-lme(fixed = ROS~treatment , data=data, random = ~ 1 | Site)
mm13a<-lme(fixed = ROS~map , data=data, random = ~ 1 | Site)
mm14a<-lme(fixed = ROS~ mat , data=data, random = ~ 1 | Site)
mm15a<-lme(fixed = ROS~ time_since_excl, data=data, random = ~ 1 | Site)
null<-lme(fixed = ROS~1, data=data, random = ~ 1 | Site)

#look at residuals
hist(resid(mm1a))
plot(resid(mm1a))

library(MuMIn)
Cand.mods<- list(mm1a,mm2a,mm3a,mm4a,mm5a,mm6a,mm7a,mm8a,mm9a,mm10a,mm11a,mm12a,mm13a,mm14a,mm15a,null)
aictab<- model.sel(Cand.mods)
a<-as.data.frame(aictab)

#best supported model for ROS is actually null

summary(null)

#TRY MODEL ABOVE WTIH MOISTURE ZONE AS A PREDICTOR
#create numeric vector of moisture zones
data$mz2<-as.numeric(substr(data$mz,1, 2))

#First try as factor
data$mz<-factor(data$mz)

mm1b<-lme(fixed = ROS~treatment + mz + treatment:mz + mat + time_since_excl, data=data, random = ~ 1 | Site)
summary(mm1b)

#try with mz as continuous
mm1c<-lme(fixed = ROS~treatment + mz2 + treatment:mz2 + mat + time_since_excl, data=data, random = ~ 1 | Site)
summary(mm1c)

data$mz
m<-data.frame(substr(data$mz,1, 2),2)

#Multi model selection
#Run models with all combinations of predictors w/ rate-of-spread as response
#model with ALL variables is global model
#Compare all combination with the null model (ie no predictor)

mm1c<-lme(fixed = ROS~treatment + mz2 + treatment:mz2 + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm2c<-lme(fixed = ROS~treatment + mz2 + treatment:mz2 + mat, data=data, random = ~ 1 | Site)
mm3c<-lme(fixed = ROS~treatment + mz2 + treatment:mz2 + time_since_excl, data=data, random = ~ 1 | Site)
mm4c<-lme(fixed = ROS~treatment + mz2 + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm5c<-lme(fixed = ROS~treatment + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm6c<-lme(fixed = ROS~ mz2  + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm7c<-lme(fixed = ROS~treatment + time_since_excl, data=data, random = ~ 1 | Site)
mm8c<-lme(fixed = ROS~treatment + mat , data=data, random = ~ 1 | Site)
mm9c<-lme(fixed = ROS~ mz2 + mat, data=data, random = ~ 1 | Site)
mm10c<-lme(fixed = ROS~ mz2 + time_since_excl, data=data, random = ~ 1 | Site)
mm11c<-lme(fixed = ROS~treatment + mz2 + treatment:mz2, data=data, random = ~ 1 | Site)
mm12c<-lme(fixed = ROS~treatment , data=data, random = ~ 1 | Site)
mm13c<-lme(fixed = ROS~ mz2 , data=data, random = ~ 1 | Site)
mm14c<-lme(fixed = ROS~ mat , data=data, random = ~ 1 | Site)
mm15c<-lme(fixed = ROS~ time_since_excl, data=data, random = ~ 1 | Site)
nullc<-lme(fixed = ROS~1, data=data, random = ~ 1 | Site)

#look at residuals
hist(resid(mm11c))

plot(resid(mm11c))

mm11c
summary(mm11c)

Cand.modsc<- list(mm1c,mm2c,mm3c,mm4c,mm5c,mm6c,mm7c,mm8c,mm9c,mm10c,mm11c,mm12c,mm13c,mm14c,mm15c,nullc)
aictabc<- model.sel(Cand.modsc)
ac<-as.data.frame(aictabc)

#Multi model selection
#Run models with all combinations of predictors with flame length as response
#model with ALL variables is global model
#Compare all combination with the null model (ie no predictor)

mm1d<-lme(fixed = FL~treatment + mz2 + treatment:mz2 + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm2d<-lme(fixed = FL~treatment + mz2 + treatment:mz2 + mat, data=data, random = ~ 1 | Site)
mm3d<-lme(fixed = FL~treatment + mz2 + treatment:mz2 + time_since_excl, data=data, random = ~ 1 | Site)
mm4d<-lme(fixed = FL~treatment + mz2 + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm5d<-lme(fixed = FL~treatment + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm6d<-lme(fixed = FL~ mz2  + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm7d<-lme(fixed = FL~treatment + time_since_excl, data=data, random = ~ 1 | Site)
mm8d<-lme(fixed = FL~treatment + mat , data=data, random = ~ 1 | Site)
mm9d<-lme(fixed = FL~ mz2 + mat, data=data, random = ~ 1 | Site)
mm10d<-lme(fixed = FL~ mz2 + time_since_excl, data=data, random = ~ 1 | Site)
mm11d<-lme(fixed = FL~treatment + mz2 + treatment:mz2, data=data, random = ~ 1 | Site)
mm12d<-lme(fixed = FL~treatment , data=data, random = ~ 1 | Site)
mm13d<-lme(fixed = FL~ mz2 , data=data, random = ~ 1 | Site)
mm14d<-lme(fixed = FL~ mat , data=data, random = ~ 1 | Site)
mm15d<-lme(fixed = FL~ time_since_excl, data=data, random = ~ 1 | Site)
nulld<-lme(fixed = FL~1, data=data, random = ~ 1 | Site)

Cand.modsd<- list(mm1d,mm2d,mm3d,mm4d,mm5d,mm6d,mm7d,mm8d,mm9d,mm10d,mm11d,mm12d,mm13d,mm14d,mm15d,nulld)
aictabd<- model.sel(Cand.modsd)
ad<-as.data.frame(aictabd)

#best supported model for FL is mm11d
#look at residuals
hist(resid(mm11d))
plot(resid(mm11d))
mm11d
summary(mm11d)

#plot results
library(ggplot2)

attach(data)
plot(mz, FL, group = treatment)
qplot(x = mz, y = FL, group = treatment, data = data, geom = "point") + theme_bw()
ggplot(data, aes(mz, FL)) + geom_point(aes(color = treatment)) + scale_color_grey() + theme_bw() + 
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t=0, r=10, b=0, l=0))) +
  labs(y="Flame Length (m)", x = "Moisture Zone", color = "Treatment") +
  theme(axis.title = element_text(size = rel(1.2)))
