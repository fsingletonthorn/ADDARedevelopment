library(ggplot2); library(lme4)

# generating random data - 150 cases from 10 schools
mathDat<-data.frame(ses = 1:150)
mathDat$ses <- rnorm(n = 150, mean = 0, sd = 1) 
mathDat$school <- rep(paste("school", 1:10), each=15)
mathDat$schoolIntercept <- rep(rnorm(n = 10, mean = 0, sd = 12), each=15)
mathDat$schoolSlope <- rep(rnorm(n = 10, mean = 0, sd = 5), each=15)

# Math scores which are a product of the above
mathDat$math <- mathDat$ses * 10 + mathDat$schoolIntercept + mathDat$ses*mathDat$schoolSlope + rnorm(nrow(mathDat), mean = 50, sd = 10)


# Plot with no regression slope
qplot(ses, math, col = school, data = mathDat, xlab = "SES", ylab = "Math scores")+ theme_classic() 

# Plot with single regression slope
qplot(ses, math, col = school, data = mathDat, xlab = "SES", ylab = "Math scores") + 
  theme_classic()+ 
  geom_smooth(aes(ses, math),data = mathDat, method = lm, inherit.aes = F, se = F, colour = "black")+
  ylim(-5,120)

qplot(ses, math, col = school, data = mathDat, 
      xlab = "SES", ylab = "Math scores") + 
  theme_classic()  + 
  geom_smooth(method = "lm", fill = NA, fullrange=TRUE) +
  ylim(-5,120)

# Plot with no y 
ggplot(data = mathDat , aes(0, math, col = school)) + 
  theme_classic() + geom_boxplot() + geom_point(position=position_dodge(width=0.75),aes(group=school)) +
xlab("Nothing") + ylab("Math scores") +  theme(axis.text.x=element_blank(),
                                               axis.ticks.x=element_blank())




# Random intercepts model plot 
mod <- lmer(math ~ ses + (1 | school), data = mathDat)

# Plot with single regression slope
qplot(ses, math, col = school, data = mathDat, xlab = "SES", ylab = "Math scores") + 
  theme_classic()+ 
  geom_abline(intercept = fixef(mod)[1] + ranef(mod)[[1]][[1]], slope = fixef(mod)[2], colour = factor(1:10)) +
  ylim(-5,120)



# Random effects model plot 
mod <- lmer(math ~ ses + (1 + ses | school), data = mathDat)

# Plot with single regression slope
qplot(ses, math, col = school, data = mathDat, xlab = "SES", ylab = "Math scores") + 
  theme_classic()+ 
  geom_abline(intercept = fixef(mod)[1] + ranef(mod)[[1]][[1]], slope = fixef(mod)[2] + ranef(mod)[[1]][[2]], colour = factor(1:10)) +
  ylim(-5,120)




