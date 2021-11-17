

library(MASS)

sw.dat <- read.csv("SWOW_regression.csv")


#remove missing values
sw.dat <- na.omit(sw.dat)
# Fit the full model 

full.model <- lm(R123_sw_z~., data = sw.dat[,1:10])
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

R2.dat <- data.frame(Model = c("Baseline","bnc.HAL","bnc.LSA","bnc.CBOW","tasa.LSA","bar.CBOW"),R2 = rep(NA,6))

baseline.model <- lm(R123_sw_z~.,sw.dat[,c(1,7:10)])
summary(baseline.model)$r.squared
bnc.HAL.model <- lm(R123_sw_z~.,sw.dat[,c(1,2,7:10)])
bnc.LSA.model <- lm(R123_sw_z~.,sw.dat[,c(1,3,7:10)])
bnc.CBOW.model <- lm(R123_sw_z~.,sw.dat[,c(1,4,7:10)])
tasa.LSA.model <- lm(R123_sw_z~.,sw.dat[,c(1,5,7:10)])
bar.CBOW.model <- lm(R123_sw_z~.,sw.dat[,c(1,6,7:10)])
R2.dat[1,2] <- summary(baseline.model)$r.squared
R2.dat[2,2] <- summary(bnc.HAL.model)$r.squared
R2.dat[3,2] <- summary(bnc.LSA.model)$r.squared
R2.dat[4,2] <- summary(bnc.CBOW.model)$r.squared
R2.dat[5,2] <- summary(tasa.LSA.model)$r.squared
R2.dat[6,2] <- summary(bar.CBOW.model)$r.squared
R2.dat$Model <- factor(R2.dat$Model, levels =level_order <- R2.dat[,1])
ggplot(R2.dat, aes(x = Model, y = R2))+
  geom_point()+geom_hline(yintercept = R2.dat[1,2])+ labs(subtitle ="Baseline: word length and subtitle frequency for both cue and response")
