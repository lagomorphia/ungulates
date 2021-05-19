library(Hmisc)
library(nlme)
library(MuMIn)
library(ggplot2)
library(lme4)
library(ggeffects)
library(ez)
library(dplyr)

#import data
table <- "finefuel.csv"
data<-read.table(table, header=T, sep=",")

hist(data$Weight)

#treatment<-factor(data$treatment)

m1<-lm(Weight~treatment+map+mat+time_since_excl, data=data)
summary(m1)

hist(resid(m1))

wt<-(data$Weight)^.7
hist(wt)

data$wttrans<-(data$Weight + 10)^.7
hist(data$wttrans)

#run simple regression with transformed response variable
m1a<-lm(wttrans~treatment+map+mat+time_since_excl, data=data)

hist(resid(m1a))
summary(m1a)

#Run a mixed model using nlme package
#Same fixed effects (predictors) as above, but now with Site as a random effect

mm1<-lme(fixed = wttrans~treatment+map+mat+time_since_excl, data=data, random = ~ 1 | Site)
summary(mm1)

#Explained deviance (more or less like R^2) - proposed by the author of the nlme packge (Pinheiro)
cor(fitted(mm1), getResponse(mm1))^2    

#try with interaction term (allows slope also to change with respect to co-variate (treatment)

mm1a<-lme(fixed = wttrans~treatment + map + treatment:map + mat + time_since_excl, data=data, random = ~ 1 | Site)
summary(mm1a)

#Explained deviance
cor(fitted(mm1a), getResponse(mm1a))^2 

#Multi model selection
#Run models with all combinations of predictors
#model with ALL variables is global model
#Compare all combination with the null model (ie no predictor)

mm1a<-lme(fixed = wttrans~treatment + map + treatment:map + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm2a<-lme(fixed = wttrans~treatment + map + treatment:map + mat, data=data, random = ~ 1 | Site)
mm3a<-lme(fixed = wttrans~treatment + map + treatment:map + time_since_excl, data=data, random = ~ 1 | Site)
mm4a<-lme(fixed = wttrans~treatment + map + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm5a<-lme(fixed = wttrans~treatment + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm6a<-lme(fixed = wttrans~ map  + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm7a<-lme(fixed = wttrans~treatment + time_since_excl, data=data, random = ~ 1 | Site)
mm8a<-lme(fixed = wttrans~treatment + mat , data=data, random = ~ 1 | Site)
mm9a<-lme(fixed = wttrans~ map + mat, data=data, random = ~ 1 | Site)
mm10a<-lme(fixed = wttrans~ map + time_since_excl, data=data, random = ~ 1 | Site)
mm11a<-lme(fixed = wttrans~treatment + map + treatment:map, data=data, random = ~ 1 | Site)
mm12a<-lme(fixed = wttrans~treatment , data=data, random = ~ 1 | Site)
mm13a<-lme(fixed = wttrans~map , data=data, random = ~ 1 | Site)
mm14a<-lme(fixed = wttrans~ mat , data=data, random = ~ 1 | Site)
mm15a<-lme(fixed = wttrans~ time_since_excl, data=data, random = ~ 1 | Site)
null<-lme(fixed = wttrans~1, data=data, random = ~ 1 | Site)

#look at residuals
hist(resid(mm1a))
plot(resid(mm1a))

#MuMin
Cand.mods<- list(mm1a,mm2a,mm3a,mm4a,mm5a,mm6a,mm7a,mm8a,mm9a,mm10a,mm11a,mm12a,mm13a,mm14a,mm15a,null)
aictab<- model.sel(Cand.mods)
a<-as.data.frame(aictab)

#best supported model is mm11a
summary(mm11a)

#TRY MODEL ABOVE WTIH MOISTURE ZONE AS A PREDICTOR
#create numeric vector of moisture zones
data$mz2<-as.numeric(substr(data$mz,1, 2))

#First try as factor
data$mz<-factor(data$mz)

mm1b<-lme(fixed = wttrans~treatment + mz + treatment:mz + mat + time_since_excl, data=data, random = ~ 1 | Site)
summary(mm1b)

mm2b <- lme(fixed = wttrans~treatment + mz + treatment:mz, data=data, random = ~ 1 | Site)

#try with mz as continuous
mm1c<-lme(fixed = wttrans~treatment + mz2 + treatment:mz2 + mat + time_since_excl, data=data, random = ~ 1 | Site)
summary(mm1c)

data$mz
m<-data.frame(substr(data$mz,1, 2),2)

#Multi model selection
#Run models with all combinations of predictors
#model with ALL variables is global model
#Compare all combination with the null model (ie no predictor)

mm1c<-lme(fixed = wttrans~treatment + mz + treatment:mz + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm2c<-lme(fixed = wttrans~treatment + mz + treatment:mz + mat, data=data, random = ~ 1 | Site)
mm3c<-lme(fixed = wttrans~treatment + mz + treatment:mz + time_since_excl, data=data, random = ~ 1 | Site)
mm4c<-lme(fixed = wttrans~treatment + mz + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm5c<-lme(fixed = wttrans~treatment + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm6c<-lme(fixed = wttrans~ mz  + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm7c<-lme(fixed = wttrans~treatment + time_since_excl, data=data, random = ~ 1 | Site)
mm8c<-lme(fixed = wttrans~treatment + mat , data=data, random = ~ 1 | Site)
mm9c<-lme(fixed = wttrans~ mz + mat, data=data, random = ~ 1 | Site)
mm10c<-lme(fixed = wttrans~ mz + time_since_excl, data=data, random = ~ 1 | Site)
mm11c<-lme(fixed = wttrans~treatment + mz + treatment:mz, data=data, random = ~ 1 | Site)
mm12c<-lme(fixed = wttrans~treatment , data=data, random = ~ 1 | Site)
mm13c<-lme(fixed = wttrans~ mz, data=data, random = ~ 1 | Site)
mm14c<-lme(fixed = wttrans~ mat , data=data, random = ~ 1 | Site)
mm15c<-lme(fixed = wttrans~ time_since_excl, data=data, random = ~ 1 | Site)
nullc<-lme(fixed = wttrans~1, data=data, random = ~ 1 | Site)

mm1d<-lme(fixed = wttrans~treatment + mz + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm2d<-lme(fixed = wttrans~treatment + mz + mat, data=data, random = ~ 1 | Site)
mm3d<-lme(fixed = wttrans~treatment + mz + time_since_excl, data=data, random = ~ 1 | Site)
mm4d<-lme(fixed = wttrans~treatment + mz, data=data, random = ~ 1 | Site)
mm5d<-lme(fixed = wttrans~treatment + map + mz + treatment:map + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm6d<-lme(fixed = wttrans~treatment + map + mz + treatment:map + mat, data=data, random = ~ 1 | Site)
mm7d<-lme(fixed = wttrans~treatment + map + mz + treatment:map + time_since_excl, data=data, random = ~ 1 | Site)
mm8d<-lme(fixed = wttrans~treatment + map + mz + treatment:map, data=data, random = ~ 1 | Site)
mm9d<-lme(fixed = wttrans~treatment + map + treatment:map + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm10d<-lme(fixed = wttrans~treatment + map + treatment:map + mat, data=data, random = ~ 1 | Site)
mm11d<-lme(fixed = wttrans~treatment + map + treatment:map + time_since_excl, data=data, random = ~ 1 | Site)
mm12d<-lme(fixed = wttrans~treatment + map + treatment:map, data=data, random = ~ 1 | Site)
mm13d<-lme(fixed = wttrans~treatment + map, data=data, random = ~ 1 | Site)


mm16c<-lme(fixed = wttrans~treatment + map + mz + treatment:mz + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm17c<-lme(fixed = wttrans~treatment + map + mz + treatment:mz + mat, data=data, random = ~ 1 | Site)
mm18c<-lme(fixed = wttrans~treatment + map + mz + treatment:mz + time_since_excl, data=data, random = ~ 1 | Site)
mm19c<-lme(fixed = wttrans~treatment + map + mz + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm20c<-lme(fixed = wttrans~treatment + map + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm21c<-lme(fixed = wttrans~ mz  + mat + map + time_since_excl, data=data, random = ~ 1 | Site)
mm22c<-lme(fixed = wttrans~treatment + map + time_since_excl, data=data, random = ~ 1 | Site)
mm23c<-lme(fixed = wttrans~treatment + map + mat , data=data, random = ~ 1 | Site)
mm24c<-lme(fixed = wttrans~ mz + map + mat, data=data, random = ~ 1 | Site)
mm25c<-lme(fixed = wttrans~ mz + map + time_since_excl, data=data, random = ~ 1 | Site)
mm26c<-lme(fixed = wttrans~treatment + map + mz + treatment:mz, data=data, random = ~ 1 | Site)
mm27c<-lme(fixed = wttrans~treatment + map , data=data, random = ~ 1 | Site)
mm28c<-lme(fixed = wttrans~ mz + map, data=data, random = ~ 1 | Site)
mm29c<-lme(fixed = wttrans~ mat + map, data=data, random = ~ 1 | Site)
mm30c<-lme(fixed = wttrans~ time_since_excl + map, data=data, random = ~ 1 | Site)

Cand.modsc<- list(mm1c,mm2c,mm3c,mm4c,mm5c,mm6c,mm7c,mm8c,mm9c,mm10c,mm11c,mm12c,mm13c,mm14c,mm15c,
                  mm16c, mm17c, mm18c, mm19c, mm20c, mm21c, mm22c, mm23c, mm24c, mm25c, mm26c, mm27c,
                  mm28c, mm29c, mm30c, nullc, mm1d, mm2d, mm3d, mm4d, mm5d,
                  mm6d, mm7d, mm8d, mm9d, mm10d, mm11d, mm12d, mm13d)
aictabc<- model.sel(Cand.modsc)
ac<-as.data.frame(aictabc)

#best supported model is mm11c
summary(mm11c)
cor(fitted(mm11c), getResponse(mm11c))^2
r.squaredGLMM(mm11c)

### plots

ggplot(data, aes(x = mz, y = Weight, colour=treatment)) +
  geom_point(alpha = 0.3) +
  labs(x = "Moisture Zone", y = "Fine Fuel Loading (Mg/ha)") +
  ggtitle("Fine Fuel Loading by Moisture Zone and Treatment") +
  scale_fill_grey(name = "Treatment", start = .55, end =.8)

ggplot(data, aes(map,Weight,group=treatment, colour=treatment)) + 
  geom_smooth(method="lm",se=T,size=1) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw() +
  xlab("Mean Annual Precipitation (mm/year)") +
  ylab("Fine Fuel Loading (Mg/ha)") +
  ggtitle("Fine Fuel Loading by Mean Annual Precipitation and Treatment")

### predicted model results
mmmm<-lmer(Weight~treatment + mz + treatment:mz + ( 1 | Site), data=data)

rt_preds <- ezPredict(fit = mmmm, iterations = 100)
summary(rt_preds)
f <- ezPlot2(CI=.95, #mean_se?
             CI_alpha = 1,
             preds = rt_preds,
             x = mz,
             split = treatment,
             do_lines = FALSE) + geom_point(shape = 16, size = 2)+
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1.0,
                width = 0.6, position=position_dodge(width=0.6))

plot(f)
summary(f)

f +
  theme_bw() + scale_color_grey(start = 0.2,end = 0.65) +
  labs(y="Fine Fuel Loading (Mg/ha)", x = "Moisture Zone") +
  theme(axis.title = element_text(size = rel(1.2))) +
  scale_y_continuous(limits = c(0, 25)) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

### bootstrap predicted results
boot<-as.data.frame(rt_preds)
g <- ggplot(boot, aes(x = boots.mz, y = boots.value,
                      color = boots.treatment,
                      group = boots.treatment,
                      fill = boots.treatment,
                      shape = boots.treatment)) + 
  theme_bw() + scale_color_grey(start = 0.2,end = 0.65) +
  labs(y="Fine Fuel Loading (Mg/ha)", x = "Moisture Zone", color="Treatment") +
  theme(axis.title = element_text(size = rel(1.2)))

g + geom_errorbar(stat="summary", fun.data="mean_cl_normal", size = 1.0,
                  width = 0.6, position=position_dodge(width=0.6)) +
  scale_y_continuous(limits = c(0, 25)) + 
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t=0, r=10, b=0, l=0)))

### actual results + SE bars
t <- ggplot(data, aes(x = mz, y = Weight,
                      color = treatment,
                      group = treatment)) + 
  geom_point(size = 1, alpha = 0.6, position = position_jitterdodge(dodge.width=0.6)) +
  theme_bw() + scale_color_grey(start = 0.2,end = 0.65) +
  labs(y="Fine Fuel Loading (Mg/ha)", x = "Moisture Zone", color = "Treatment") +
  theme(axis.title = element_text(size = rel(1.2)))

t + geom_errorbar(stat="summary", fun.data="mean_se", size = 1.0,
                  width = 0.6, position=position_dodge(width=0.6)) +
  scale_y_continuous(limits = c(0, 25)) + 
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t=0, r=10, b=0, l=0)))

### bar plot with means and SEs
summary(data)
data_summary <- data %>% # the names of the new data frame and the data frame to be summarised
  group_by(mz, treatment) %>%
  summarise(mean_PL = mean(Weight),  # calculates the mean of each group
            sd_PL = sd(Weight), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(Weight)/sqrt(n())) # calculates the standard error of each group

summary(data_summary$mean_PL)
summary(data_summary)

limits <- aes(ymax = data_summary$mean_PL + data_summary$SE_PL,
              ymin = data_summary$mean_PL - data_summary$SE_PL)

p <- ggplot(data = data_summary, aes(x = factor(mz), y = mean_PL,
                                     fill = factor(treatment)))

p + geom_bar(stat = "identity",
             position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "Moisture Zone", y = "Fine Fuel Loading (Mg/ha)") +
  ggtitle("Fine Fuel Loading by Moisture Zone and Treatment") +
  scale_fill_grey(name = "Treatment", start = .55, end =.8)

