##### Paper: water inundation sensitivity in dipterocarps
##### author: james margrove 
##### date: 26.01.2017

##### imported packaged 
require(ggplot2)
require(lme4)

data <- read.table("Experiment, mort, leafAB, dden, wden,sla.txt", header = TRUE);
str(data)

##### data exploration with ggplot. 
ggplot(data, aes(x=treat, y=surv)) + geom_point() + stat_smooth()

model1 <- glmer(surv ~ treat + dia + (1|sp/mother) + (1|block), data = data, family = "binomial")
summary(model1)

##### effect
# the effect of flooding overall on dipterocarps is negative. 