##### Paper: water inundation sensitivity in dipterocarps
##### author: james margrove 
##### date: 26.01.2017
################################################################################
##### imported packaged 
require(ggplot2)
require(lme4)
require(merTools)
source("./functions/newbinplot.R"); require(arm)
source("./functions/ranNorm.R")


##### remove clutter 
rm(list=ls())

survival_data <- read.table("Experiment, mort, leafAB, dden, wden,sla.txt", header = TRUE);
str(survival_data)

##### data exploration with ggplot. 
ggplot(survival_data, aes(x=treat, y=surv)) + geom_point() + stat_smooth()

survival_model1 <- glmer(surv ~ treat + dia + 
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
newbinplot(surv_model1_residuals, surv_model1_fitted);

# Random effects reasonabbly normally distributed
ranNorm("mother:sp", slope = 1, model = survival_model1)
ranNorm("sp", slope = 1, model = survival_model1)
ranNorm("block", slope = 1, model = surivival_model1)

##### Graphing the analysis
surv_preds <- expand.grid(dia = mean(survival_data$dia),
                          treat = seq(from = 0, to = 21, length = 100),
                          mother = 0,
                          sp = 0,
                          block = 0)

##### predicting the values 
surv_preds$p <- predict(survival_model1, 
                       newdata = surv_preds,
                       type = "response", 
                       re.form = NA)

predict(survival_model1, 
        newdata = surv_preds,
        type = "response", 
        re.form = NA)

summary(survival_model1)

head(preds)
##### Confidence intervals 
ggplot(surv_preds, aes(x=treat, y=p)) + geom_line() 
################################################################################
##### extremes of survival curve...
min(surv_preds$p)
max(surv_preds$p)
