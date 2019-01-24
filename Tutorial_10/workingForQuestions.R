library(readr)
library(multilevel); library(brms)
 data("lq2002")


mod <- lmer(HOSTILE ~   TSIG + GLEAD + (1 + GLEAD | COMPID) , data = lq2002)

summary(mod)



mod <- lmer(HOSTILE ~   TSIG + GLEAD + (1 | COMPID) , data = lq2002)

summary(mod)
mod <- lmer(HOSTILE ~   TSIG + GLEAD + (1 + TSIG | COMPID) , data = lq2002, control = lmerControl(optimizer = "bobyqa"))
