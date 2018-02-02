##### Paper: water inundation sensitivity in dipterocarps
##### author: james margrove 
##### date: 26.01.2017
################################################################################

##### remove clutter 
rm(list=ls())

##### imported packaged 
require(ggplot2)
require(lme4)
require(merTools)
require(lmerTest)
source(system.file("utils", "allFit.R", package = "lme4"))
source("./functions/newbinplot.R"); require(arm)
source("./functions/ranNorm.R")

survival_data <- read.table("./data/Experiment, mort, leafAB, dden, wden,sla.txt", header = TRUE);
str(survival_data)

##### data exploration with ggplot. 
ggplot(survival_data, aes(x=treat, y=surv)) + geom_point() + stat_smooth()


##### first model if treatmnean and diameter had an addative effect 
survival_model1 <- glmer(surv ~ treat * dia + 
                           (1|sp/mother) + 
                           (1|block), 
                         data = survival_data, 
                         family = "binomial")
summary(survival_model1)

##### effect
# the effect of flooding overall on dipterocarps is negative. 
##### model validation.

##### residuals & fitted values 
surv_model1_residuals <- resid(survival_model1, type =  "pearson");
surv_model1_fitted <- fitted(survival_model1)

# Some deviations from normallity but otherwise OK 
par(mfrow=c(1,3))
newbinplot(surv_model1_residuals, surv_model1_fitted);


eval_data <- data.frame(res = surv_model1_residuals, 
                        fit = surv_model1_fitted)

eval_data$cd <- with(eval_data, cut(fit, 100))

rc <- with(eval_data, tapply(res, cd, mean))
fc <- with(eval_data, tapply(fit, cd, mean))
par(mfrow=c(1,1))
plot(y = rc, fc)

?qqnorm
qqnorm(rc)
abline(a=0,b=1)
?cut

arm::binnedplot(x = surv_model1_fitted, y = surv_model1_residuals, nclass = 100)
