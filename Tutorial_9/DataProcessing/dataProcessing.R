library(readr)
library(foreign)
siop <- read.spss('Tutorial_9/siop.sav', to.data.frame = T)
siop_g <- read.spss('Tutorial_9/siop-group.sav', to.data.frame = T)


write.csv(siop, "Tutorial_9/siop.csv")
write.csv(siop_g, "Tutorial_9/siop_group.csv")
