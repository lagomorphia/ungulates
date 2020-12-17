#import data

data<-read.table("C:\\Users\\Tim\\Documents\\R\\livedead.csv", header=T, sep=",")

names(data)
head(data)
nrow(data)

data

hist(data$Weight)

treatment<-factor(treatment)


m1<-lm(Weight~treatment+mz+livedead, data=data)
summary(m1)

hist(resid(m1))# NOT EXACTLY NORMAL

wt<-(data$Weight)^.7
hist(wt)

data$wttrans<-(data$Weight + 10)^.7
hist(data$wttrans)

#run simple regression with transformed response variable
m1a<-lm(Weight~treatment+mz+treatment:mz, data=data)

hist(resid(m1a)) #close-ish to normal

summary(m1a)

#Run a mixed model using nlme package

library(nlme)

#Same fixed effects (predictors) as above, but now with Site as a random effect

mm1<-lme(fixed = Weight~treatment + mz + livedead, data=data, random = ~ 1 | Site)
summary(mm1)

#Explained deviance (more or less like R^2) - proposed by the author of the nlme packge (Pinheiro)
cor(fitted(mm1), getResponse(mm1))^2    


#try with interaction term (allows slope also to change with respect to your co-variate (treatment)

mm1a<-lme(fixed = Weight~treatment + mz + treatment:mz, data=data, random = ~ 1 | Site)
summary(mm1a)

#Explained deviance
cor(fitted(mm1a), getResponse(mm1a))^2 

#Multi model selection
#Run models with all combinations of predictors
#model with ALL variables is your global model
#Compare all combination with the null model (ie no predictor)

mm1a<-lme(fixed = Weight~treatment + map + treatment:map + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm2a<-lme(fixed = Weight~treatment + map + treatment:map + mat, data=data, random = ~ 1 | Site)
mm3a<-lme(fixed = Weight~treatment + map + treatment:map + time_since_excl, data=data, random = ~ 1 | Site)
mm4a<-lme(fixed = Weight~treatment + map + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm5a<-lme(fixed = Weight~treatment + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm6a<-lme(fixed = Weight~ map  + mat + time_since_excl, data=data, random = ~ 1 | Site)
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

library(MuMIn)
Cand.mods<- list(mm1a,mm2a,mm3a,mm4a,mm5a,mm6a,mm7a,mm8a,mm9a,mm10a,mm11a,mm12a,mm13a,mm14a,mm15a,null)
aictab<- model.sel(Cand.mods)
a<-as.data.frame(aictab)

#best supported model is actually mm12a - JUST TREATMENT

summary(mm12a)

#Explained deviance
cor(fitted(mm12a), getResponse(mm12a))^2 

#Explained deviance
cor(fitted(mm11a), getResponse(mm11a))^2 


#TRY MODEL ABOVE WTIH MOISTURE ZONE AS A PREDICTOR
#create numeric vector of moisture zones
data$mz2<-as.numeric(substr(data$mz,1, 2))

#First try as factor
data$mz<-factor(data$mz)

mm1b<-lme(fixed = wttrans~treatment + mz + treatment:mz + mat + time_since_excl, data=data, random = ~ 1 | Site)
summary(mm1b)

#try with mz as continuous
mm1c<-lme(fixed = wttrans~treatment + mz2 + treatment:mz2 + mat + time_since_excl, data=data, random = ~ 1 | Site)
summary(mm1c)

data$mz
?splitstr
m<-data.frame(substr(data$mz,1, 2),2)

?substr

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


mm1c<-lme(fixed = wttrans~treatment + mz2 + treatment:mz2 + mat + time_since_excl, data=data, random = ~ 1 | Site)
summary(mm1c)


#Multi model selection
#Run models with all combinations of predictors
#model with ALL variables is your global model
#Compare all combination with the null model (ie no predictor)

mm1c<-lme(fixed = wttrans~treatment + mz2 + treatment:mz2 + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm2c<-lme(fixed = wttrans~treatment + mz2 + treatment:mz2 + mat, data=data, random = ~ 1 | Site)
mm3c<-lme(fixed = wttrans~treatment + mz2 + treatment:mz2 + time_since_excl, data=data, random = ~ 1 | Site)
mm4c<-lme(fixed = wttrans~treatment + mz2 + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm5c<-lme(fixed = wttrans~treatment + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm6c<-lme(fixed = wttrans~ mz2  + mat + time_since_excl, data=data, random = ~ 1 | Site)
mm7c<-lme(fixed = wttrans~treatment + time_since_excl, data=data, random = ~ 1 | Site)
mm8c<-lme(fixed = wttrans~treatment + mat , data=data, random = ~ 1 | Site)
mm9c<-lme(fixed = wttrans~ mz2 + mat, data=data, random = ~ 1 | Site)
mm10c<-lme(fixed = wttrans~ mz2 + time_since_excl, data=data, random = ~ 1 | Site)
mm11c<-lme(fixed = wttrans~treatment + mz2 + treatment:mz2, data=data, random = ~ 1 | Site)
mm12c<-lme(fixed = wttrans~treatment , data=data, random = ~ 1 | Site)
mm13c<-lme(fixed = wttrans~ mz2 , data=data, random = ~ 1 | Site)
mm14c<-lme(fixed = wttrans~ mat , data=data, random = ~ 1 | Site)
mm15c<-lme(fixed = wttrans~ time_since_excl, data=data, random = ~ 1 | Site)
nullc<-lme(fixed = wttrans~1, data=data, random = ~ 1 | Site)

#look at residuals
hist(resid(mm11c))

plot(resid(mm11c))

mm11c
summary(mm11c)

library(MuMIn)
Cand.modsc<- list(mm1c,mm2c,mm3c,mm4c,mm5c,mm6c,mm7c,mm8c,mm9c,mm10c,mm11c,mm12c,mm13c,mm14c,mm15c,nullc)
aictabc<- model.sel(Cand.modsc)
ac<-as.data.frame(aictabc)

#best supported model is actually mm12a - JUST TREATMENT

cor(fitted(mm11c), getResponse(mm11c))^2

r.squaredGLMM(mm11c)

library(ggplot2)

ggplot(data, aes(map,Weight,group=treatment, colour=treatment)) + 
  geom_smooth(method="lm",se=T,size=1) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw() +
  xlab("Mean Annual Precipitation (mm/year)") +
  ylab("Fine Fuel Loading (Mg/ha)") +
  ggtitle("Fine Fuel Loading by Mean Annual Precipitation and Treatment")

boxplot(wttrans~mz2, data=data, main="Boxplots of Fuel Loading across Moisture Zones",
        xlab="Moisture Zone", ylab="Fuel Loading")


ggplot(data, aes(group, weight))+
  stat_boxplot( aes(group, weight), 
                geom='errorbar', linetype=1, width=0.5)+  #whiskers
  geom_boxplot( aes(group, weight),outlier.shape=1) +    
  stat_summary(fun.y=mean, geom="point", size=2) + 
  stat_summary(fun.data = mean_se, geom = "errorbar")

t <- ggplot(data, aes(x = treatment, y = Weight,
                      color = livedead,
                      group = livedead)) + 
  facet_grid(. ~ mz) +
  geom_point(size = 1, alpha = 0.6, position = position_jitterdodge(dodge.width=0.6)) +
  theme_bw() + scale_color_grey(start = 0.2,end = 0.65) +
  labs(y="Fine Fuel Loading (Mg/ha)", color = "Treatment") +
  theme(axis.title = element_text(size = rel(1.2))) +
  ggtitle("Moisture Zone") +
  theme(plot.title = element_text(hjust = 0.5))

t + geom_errorbar(stat="summary", fun.data="mean_se", size = 1.0,
                  width = 0.6, position=position_dodge(width=0.6)) +
  scale_y_continuous(limits = c(0, 25)) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(t=0, r=10, b=0, l=0)))


mm11c
summary(mm11c)
summary(aictabc)
write.csv(aictabc, "c:/users/tim/documents/aictab.csv")
write.table(Cand.modsc, "C:/users/tim/documents/modslist.txt")
mm11csummary <- summary(mm11c)
capture.output(mm11csummary, file = "C:/users/tim/documents/summary.txt")


citation("nlme")
citation("r")
R.version$year

cor(fitted(mm11c), 
    getResponse(mm11c))^2

data(iris)
summary(iris)
summary(data)
detach(package:plyr)
library(dplyr)
data_summary <- data %>% # the names of the new data frame and the data frame to be summarised
  group_by(Site, treatment, livedead, mz) %>%
  summarise(mean_PL = mean(Weight),  # calculates the mean of each group
            sd_PL = sd(Weight), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(Weight)/sqrt(n())) # calculates the standard error of each group

summary(data_summary$mean_PL)

attach(data_summary)
sort <- data_summary[order(mz),]
sort <- data_summary[order(mz, treatment, livedead),]
sort


data_summary
write.csv(data_summary, "c:/users/tim/documents/data_summary60318.csv")


limits <- aes(ymax = sort$mean_PL + sort$SE_PL,
              ymin = sort$mean_PL - sort$SE_PL)

p <- ggplot(data = data_summary, aes(x = factor(treatment), y = mean_PL,
                               fill = factor(livedead)))
p + geom_bar(stat = "identity",
             position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "Treatment", y = "Fine Fuel Loading (Mg/ha)") +
  ggtitle("Fine Fuel Loading by Fencing Treatment, Moisture Zone, and Live/Dead ") +
  scale_fill_discrete(name = "Live vs. Dead") + facet_grid(~ mz)

library(ggeffects)
mmmm<-lm(Weight~treatment + mz + treatment:mz + livedead, data=data)
dat <- ggaverage(mmmm, terms = c("treatment", "livedead", "mz"))
plot(dat, dodge=TRUE, facets = TRUE, colors="bw", show.title=FALSE) +
  (labs(
    x = "Moisture Zone",
    y = "Fine Fuel Loading (mg/ha)",
    colour = "Live/Dead"
  ))


dl<-read.table("C:\\Users\\Tim\\documents\\R\\deadfuel.csv", header=T, sep=",")

mm1d<-lme(fixed = Weight~treatment + mz + treatment:mz + mat + yse, data=dl, random = ~ 1 | Site)
mm2d<-lme(fixed = Weight~treatment + mz + treatment:mz + mat, data=dl, random = ~ 1 | Site)
mm3d<-lme(fixed = Weight~treatment + mz + treatment:mz + yse, data=dl, random = ~ 1 | Site)
mm4d<-lme(fixed = Weight~treatment + mz + mat + yse, data=dl, random = ~ 1 | Site)
mm5d<-lme(fixed = Weight~treatment + mat + yse, data=dl, random = ~ 1 | Site)
mm6d<-lme(fixed = Weight~ mz  + mat + yse, data=dl, random = ~ 1 | Site)
mm7d<-lme(fixed = Weight~treatment + yse, data=dl, random = ~ 1 | Site)
mm8d<-lme(fixed = Weight~treatment + mat , data=dl, random = ~ 1 | Site)
mm9d<-lme(fixed = Weight~ mz + mat, data=dl, random = ~ 1 | Site)
mm10d<-lme(fixed = Weight~ mz + yse, data=dl, random = ~ 1 | Site)
mm11d<-lme(fixed = Weight~treatment + mz + treatment:mz, data=dl, random = ~ 1 | Site)
mm12d<-lme(fixed = Weight~treatment , data=dl, random = ~ 1 | Site)
mm13d<-lme(fixed = Weight~ mz, data=dl, random = ~ 1 | Site)
mm14d<-lme(fixed = Weight~ mat, data=dl, random = ~ 1 | Site)
mm15d<-lme(fixed = Weight~ yse, data=dl, random = ~ 1 | Site)
nulld<-lme(fixed = Weight~1, data=dl, random = ~ 1 | Site)

#look at residuals
hist(resid(mm1d))

plot(resid(mm1d))

mm1d
summary(mm2d)

library(MuMIn)
Dand.modsc<- list(mm1d, mm2d, mm3d, mm4d, mm5d, mm6d, mm7d, mm8d, mm9d, mm10d, mm11d, mm12d, mm13d, mm14d, mm15d, nulld)
aictabd<- model.sel(Dand.modsc)
ad<-as.data.frame(aictabd)


