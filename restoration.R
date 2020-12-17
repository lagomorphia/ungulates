#import data
data<-read.table("C:\\Users\\Tim\\Documents\\R\\restoration.csv", header=T, sep=",")

#inspect data
names(data)
head(data)
nrow(data)
data
hist(data$Weight)

#transform data
treatment<-factor(treatment)
wt<-(data$Weight)^.7
hist(wt)
data$wttrans<-(data$Weight + 10)^.7
hist(data$wttrans)

#run simple regression with transformed response variable
m1a<-lm(wttrans~treatment+time_since_excl, data=data)

hist(resid(m1a)) # close to normal distribution

summary(m1a)

#Run a mixed model using nlme package

library(nlme)

#Same fixed effects (predictors) as above, but now with Site as a random effect

mm1<-lme(fixed = wttrans~treatment+time_since_excl, data=data, random = ~ 1 | Site)
summary(mm1)

#Explained deviance (more or less like R^2) - proposed by the author of the nlme packge (Pinheiro)
cor(fitted(mm1), getResponse(mm1))^2    


#try with interaction term (allows slope also to change with respect to co-variate (treatment)

mm1a<-lme(fixed = wttrans~treatment + time_since_excl, data=data, random = ~ 1 | Site)
summary(mm1a)

#Explained deviance
cor(fitted(mm1a), getResponse(mm1a))^2 

#Multi model selection
#Run models with all combinations of predictors
#model with ALL variables is global model
#Compare all combination with the null model (ie no predictor)

mm1a<-lme(fixed = wttrans~treatment + time_since_excl, data=data, random = ~ 1 | Site)
mm2a<-lme(fixed = wttrans~treatment, data=data, random = ~ 1 | Site)
null<-lme(fixed = wttrans~1, data=data, random = ~ 1 | Site)

#look at residuals
hist(resid(mm1a))

plot(resid(mm1a))

library(MuMIn)
Cand.mods<- list(mm1a,mm2a,null)
aictab<- model.sel(Cand.mods)
a<-as.data.frame(aictab)

#best supported model is mm1a

summary(mm1a)

#Explained deviance
cor(fitted(mm1a), getResponse(mm1a))^2 


# various plots to illustrate change in fuel loading over time

library(ggplot2)

ggplot(data, aes(time_since_excl,wttrans)) + 
  geom_smooth(method="lm",se=T,size=1) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme_bw() +
  xlab("Time Since Exclusion") +
  ylab("Fine Fuel Loading (Transformed)") +
  ggtitle("Time Since Exclusion vs. Fine Fuel Loading (Transformed)")

boxplot(wttrans~time_since_excl, data=data, main="Boxplots of Fuel Loading by Time Since Exclusion",
        xlab="Time Since Exclusion", ylab="Fuel Loading")

data$time_since_excl <- factor(data$time_since_excl)

ggplot(aes(y = Weight, x = time_since_excl, fill = time_since_excl), data = data) +
  xlab("Years Since Exclusion") +
  ylab("Fine Fuel Loading (Mg/ha)") +
  ggtitle("Fine Fuel Loading at Restoration Sites by Years Since Exclusion", subtitle = NULL)

data$time_since_excl <- factor(data$time_since_excl)

library(dplyr)
data_summary <- data %>% # the names of the new data frame and the data frame to be summarised
  group_by(time_since_excl) %>%
  summarise(mean_PL = mean(Weight),  # calculates the mean of each group
            sd_PL = sd(Weight), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(Weight)/sqrt(n())) # calculates the standard error of each group

limits <- aes(ymax = data_summary$mean_PL + data_summary$SE_PL,
              ymin = data_summary$mean_PL - data_summary$SE_PL)

library(ggplot2)
p <- ggplot(aes(x = time_since_excl, y = mean_PL, group = time_since_excl), data = data_summary)

p +
  labs(x = "Years Since Removal", y = "Fine Fuel Loading (Mg/ha)") + theme_bw() +
  geom_bar(stat = "identity",
             position = position_dodge(0.9), fill="#BBBBBB",
             width = .66) +
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20))+
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t=0, r=10, b=0, l=0)))+
  theme(axis.title = element_text(size = rel(1.2)))

g <- ggplot(data, aes(x = time_since_excl, y = Weight)) + 
  geom_point(size = 1, alpha = 0.75) +
  theme_bw() + scale_color_grey(start = 0.2,end = 0.65) +
  labs(y="Fine Fuel Loading (Mg/ha)", x = "Years Since Removal")

g + geom_errorbar(stat="summary", fun.data="mean_se", size = 1.0,
                  width = 0.6, position=position_dodge(width=0.6)) +
  scale_y_continuous(limits = c(0, 25))


### ANOVA

data$time_since_excl <- factor(data$time_since_excl)
a<-aov(Weight~time_since_excl, data=data)
summary(a)
boxplot(Weight~time_since_excl, data=data)

TukeyHSD(a)

### predicted model results

library(ggeffects)
mmmm<-lmer(Weight~time_since_excl + ( 1 | Site), data=data)

library(lme4)
library(ez)
rt_preds <- ezPredict(fit = mmmm)
f <- ezPlot2(CI = .95,
             preds = rt_preds,
             x = time_since_excl,
             do_lines = FALSE) + geom_point(shape = 16, size = 2)
f +
  theme_bw() + scale_color_grey(start = 0.2,end = 0.65) +
  labs(y="Fine Fuel Loading (Mg/ha)", x = "Years Since Removal") +
  scale_y_continuous(limits = c(0, 25))
