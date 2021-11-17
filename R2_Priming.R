library(tidyverse)
library(caret)
library(leaps)
ldt.dat <- read.csv("ldt_baseline.csv")


library(MASS)

#remove missing values
ldt.dat <- na.omit(ldt.dat)
# Fit the full model 

full.model <- lm(Ztarget.RT~., data = ldt.dat[,c(3:11,14:15)])
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

R2.dat <- data.frame(Model = c("Baseline","bnc.HAL","bnc.LSA","bnc.CBOW","tasa.LSA","bar.CBOW","R123_sw"),R2 = rep(NA,7))

baseline.model <- lm(Ztarget.RT~., data = ldt.dat[,c(3,10:11,14:15)])

bnc.HAL.model <- lm(Ztarget.RT~., data = ldt.dat[,c(3,4,10:11,14:15)])
bnc.LSA.model <- lm(Ztarget.RT~., data = ldt.dat[,c(3,5,10:11,14:15)])
bnc.CBOW.model <- lm(Ztarget.RT~., data = ldt.dat[,c(3,6,10:11,14:15)])
bar.CBOW.model <- lm(Ztarget.RT~., data = ldt.dat[,c(3,7,10:11,14:15)])
tasa.LSA.model <- lm(Ztarget.RT~., data = ldt.dat[,c(3,8:11,14:15)])
R123.model <- lm(Ztarget.RT~., data = ldt.dat[,c(3,9,10:11,14:15)])
R2.dat[1,2] <- summary(baseline.model)$r.squared
R2.dat[2,2] <- summary(bnc.HAL.model)$r.squared
R2.dat[3,2] <- summary(bnc.LSA.model)$r.squared
R2.dat[4,2] <- summary(bnc.CBOW.model)$r.squared
R2.dat[5,2] <- summary(tasa.LSA.model)$r.squared
R2.dat[6,2] <- summary(bar.CBOW.model)$r.squared
R2.dat[7,2] <- summary(R123.model)$r.squared
R2.dat$Model <- factor(R2.dat$Model, levels =level_order <- R2.dat[,1])
ggplot(R2.dat, aes(x = Model, y = R2))+
  geom_point()+geom_hline(yintercept = R2.dat[1,2])+ labs(subtitle ="Baseline: word length and subtitle frequency for both prime and target")