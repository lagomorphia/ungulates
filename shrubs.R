#import data
data<-read.table("C:\\Users\\Tim\\Documents\\R\\shrubs.csv", header=T, sep=",")

names(data)
head(data)
nrow(data)
data
hist(data$Weight)

m1<-lm(Weight~treatment+map+mat+time_since_excl, data=data)
summary(m1)

summary(m1a)
hist(resid(m1a))

#Run a mixed model using nlme package

library(nlme)

#Same fixed effects (predictors) as above, but now with Site as a random effect

mm1<-lme(fixed = Weight~treatment+map+mat+time_since_excl, data=data, random = ~ 1 | Site)
summary(mm1)

#Explained deviance (more or less like R^2) - proposed by the author of the nlme packge (Pinheiro)
cor(fitted(mm1), getResponse(mm1))^2    

#try with interaction term (allows slope also to change with respect to co-variate (treatment)

mm1a<-lme(fixed = Weight~treatment + map + treatment:map + mat + time_since_excl, data=data, random = ~ 1 | Site)
summary(mm1a)

#Explained deviance
cor(fitted(mm1a), getResponse(mm1a))^2 

#Multi model selection
#Run models with all combinations of predictors
#model with ALL variables is  global model
#Compare all combination with the null model (ie no predictor)

mm1a<-lme(fixed = Weight~treatment + map + treatment:map + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm2a<-lme(fixed = Weight~treatment + map + treatment:map + mat, data=data, random = ~ 1 | Site)
mm3a<-lme(fixed = Weight~treatment + map + treatment:map + time_since_excl, data=data, random = ~ 1 | Site)
mm4a<-lme(fixed = Weight~treatment + map + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm5a<-lme(fixed = Weight~treatment + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm6a<-lme(fixed = Weight~ map  + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm7a<-lme(fixed = Weight~treatment + time_since_excl, data=data, random = ~ 1 | Site)
mm8a<-lme(fixed = Weight~treatment + mat , data=data, random = ~ 1 | Site)
mm9a<-lme(fixed = Weight~ map + mat, data=data, random = ~ 1 | Site)
mm10a<-lme(fixed = Weight~ map + time_since_excl, data=data, random = ~ 1 | Site)
mm11a<-lme(fixed = Weight~treatment + map + treatment:map, data=data, random = ~ 1 | Site)
mm12a<-lme(fixed = Weight~treatment , data=data, random = ~ 1 | Site)
mm13a<-lme(fixed = Weight~map , data=data, random = ~ 1 | Site)
mm14a<-lme(fixed = Weight~ mat , data=data, random = ~ 1 | Site)
mm15a<-lme(fixed = Weight~ time_since_excl, data=data, random = ~ 1 | Site)
null<-lme(fixed = Weight~1, data=data, random = ~ 1 | Site)

#look at residuals
hist(resid(mm1a))

plot(resid(mm1a))

library(MuMIn)
Cand.mods<- list(mm1a,mm2a,mm3a,mm4a,mm5a,mm6a,mm7a,mm8a,mm9a,mm10a,mm11a,mm12a,mm13a,mm14a,mm15a,null)
aictab<- model.sel(Cand.mods)
a<-as.data.frame(aictab)

#best supported model is actually mm12a - JUST TREATMENT
summary(mm12a)
hist(resid(mm12a))

#Explained deviance
cor(fitted(mm12a), getResponse(mm12a))^2 

#TRY MODEL ABOVE WTIH MOISTURE ZONE AS A PREDICTOR
#create numeric vector of moisture zones
data$mz2<-as.numeric(substr(data$mz,1, 2))

#First try as factor
data$mz<-factor(data$mz)

mm1b<-lme(fixed = Weight~treatment + mz + treatment:mz + mat + time_since_excl, data=data, random = ~ 1 | Site)
summary(mm1b)

#try with mz as continuous
mm1c<-lme(fixed = Weight~treatment + mz2 + treatment:mz2 + mat + time_since_excl, data=data, random = ~ 1 | Site)
summary(mm1c)

data$mz
m<-data.frame(substr(data$mz,1, 2),2)

#PLOTS
plot(data$Weight[data$treatment=="F"]~data$mz2[data$treatment=="F"])
points(data$Weight[data$treatment=="U"]~data$mz2[data$treatment=="U"], col=2)

fenced<-data$Weight[data$treatment=="F"]
fenced
unfenced<-data$Weight[data$treatment=="U"]
unfenced

#Density plots / histogram of fine fuel load increase over MAP

p1<-hist(fenced, breaks=10)
p2<-hist(unfenced, breaks=10)
plot(p1, col=rgb(0,0,1,1/4), xlim=c(0,30))
plot(p2, col=rgb(1,0,0,1/4), add=T)


#Multi model selection
#Run models with all combinations of predictors
#model with ALL variables is global model
#Compare all combination with the null model (ie no predictor)

mm1c<-lme(fixed = Weight~treatment + mz2 + treatment:mz2 + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm2c<-lme(fixed = Weight~treatment + mz2 + treatment:mz2 + mat, data=data, random = ~ 1 | Site)
mm3c<-lme(fixed = Weight~treatment + mz2 + treatment:mz2 + time_since_excl, data=data, random = ~ 1 | Site)
mm4c<-lme(fixed = Weight~treatment + mz2 + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm5c<-lme(fixed = Weight~treatment + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm6c<-lme(fixed = Weight~ mz2  + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm7c<-lme(fixed = Weight~treatment + time_since_excl, data=data, random = ~ 1 | Site)
mm8c<-lme(fixed = Weight~treatment + mat , data=data, random = ~ 1 | Site)
mm9c<-lme(fixed = Weight~ mz2 + mat, data=data, random = ~ 1 | Site)
mm10c<-lme(fixed = Weight~ mz2 + time_since_excl, data=data, random = ~ 1 | Site)
mm11c<-lme(fixed = Weight~treatment + mz2 + treatment:mz2, data=data, random = ~ 1 | Site)
mm12c<-lme(fixed = Weight~treatment , data=data, random = ~ 1 | Site)
mm13c<-lme(fixed = Weight~ mz2 , data=data, random = ~ 1 | Site)
mm14c<-lme(fixed = Weight~ mat , data=data, random = ~ 1 | Site)
mm15c<-lme(fixed = Weight~ time_since_excl, data=data, random = ~ 1 | Site)
nullc<-lme(fixed = Weight~1, data=data, random = ~ 1 | Site)

#look at residuals
hist(resid(mm11c))

plot(resid(mm11c))

mm11c
summary(mm11c)

library(MuMIn)
Cand.modsc<- list(mm1c,mm2c,mm3c,mm4c,mm5c,mm6c,mm7c,mm8c,mm9c,mm10c,mm11c,mm12c,mm13c,mm14c,mm15c,nullc)
aictabc<- model.sel(Cand.modsc)
ac<-as.data.frame(aictabc)

#best supported model is mm12a - JUST TREATMENT

cor(fitted(mm12c), getResponse(mm12c))^2

r.squaredGLMM(mm12c)

#various plots

library(ggplot2)

ggplot(data, aes(map,Weight,group=treatment, colour=treatment)) + 
  geom_smooth(method="lm",se=T,size=1) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw() +
  xlab("Moisture Zone") +
  ylab("Fine Fuel Loading (Transformed)") +
  ggtitle("Moisture Zone vs. Fine Fuel Loading (Transformed) by Fencing Treatment")

boxplot(Weight~treatment, data=data, main="Boxplots of Fuel Loading by Fencing Treatment",
        xlab="Fencing Treatment", ylab="Fuel Loading")

library(dplyr)
data_summary <- data %>% # the names of the new data frame and the data frame to be summarised
  group_by(treatment) %>%
  summarise(mean_PL = mean(Weight),  # calculates the mean of each group
            sd_PL = sd(Weight), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(Weight)/sqrt(n())) # calculates the standard error of each group

summary(data_summary$mean_PL)

NewPlot <- ggplot(data_summary, aes(treatment, mean_PL)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = mean_PL - SE_PL, ymax = mean_PL + SE_PL), width=0.2)

NewPlot + labs(y="Fine Fuel Loading ± s.d.", x = "Moisture Zone") + theme_classic()


limits <- aes(ymax = data_summary$mean_PL + data_summary$SE_PL,
              ymin = data_summary$mean_PL - data_summary$SE_PL)

p <- ggplot(data = data_summary, aes(x = factor(treatment), y = mean_PL))

p + geom_bar(stat = "identity",
             position = position_dodge(0.9),
             fill="#BBBBBB",
             width = .66) +
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "Treatment", y = "Shrub Fuel Loading (Mg/ha)") +
  scale_fill_grey(name = "Treatment", start = .55, end =.8) + theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t=0, r=10, b=0, l=0)))+
  theme(axis.title = element_text(size = rel(1.2)))


library(ggeffects)
mmmm<-lm(Weight~treatment, data=data)
dat <- ggpredict(mmmm, terms = c("treatment"))
plot(dat, colors="bw", ci=TRUE, show.title=FALSE) +
  (labs(
    x = "Treatment",
    y = "Fine Fuel Loading (mg/ha)"
  ))

