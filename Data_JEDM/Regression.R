library(readr)
library(dplyr)

data <- read_csv("/data.csv")

model <- glm(Dropout ~parC4+ parC3+ parC2+ befC4+ befC2+ PrevFail+ befFC5+ befFC6+ PrevDO+ befDOC5,family=binomial(link='logit'),data=data)

summary(model)



model2 <- glm(Dropout ~parC4+ parC3+ parC2+ parC4*parC3*parC2 + befC4+ befC2+ PrevFail+ befFC5+ befFC6+ PrevDO+ befDOC5,family=binomial(link='logit'),data=data)

summary(model2)

model3 <- glm(Dropout ~parC4+ parC3+ parC2+ parC4*parC3*parC2 + befC4+ befC2+ PrevFail+ befFC5+ befFC6+ PrevDO+ befDOC5 + befC9+befC10+befC11+befC9*befC10*befC11,family=binomial(link='logit'),data=data)

summary(model3)

