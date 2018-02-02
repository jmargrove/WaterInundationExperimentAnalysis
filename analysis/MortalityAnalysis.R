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
eval_data <- data.frame(res = surv_model1_residuals, 
                        fit = surv_model1_fitted)

eval_data$cd <- with(eval_data, cut(fit, 100))

rc <- with(eval_data, tapply(res, cd, mean))
fc <- with(eval_data, tapply(fit, cd, mean))
par(mfrow=c(1,1))
plot(y = rc, fc)
car::qqPlot(rc, col = "black", pch = 19, cex = .75, grid = FALSE, ylab = "Sample Quantiles", xlab = "Theoretical Quantiles")

# Random effects reasonabbly normally distributed
ranNorm("mother:sp", slope = 1, model = survival_model1)
ranNorm("sp", slope = 1, model = survival_model1)
ranNorm("block", slope = 1, model = surivival_model1)








surv_model2 <- update(survival_model1, . ~ . - treat + log(treat+1))
surv_model2 <- update(survival_model1, . ~ . log(treat + 1)+ log(treat+1))

# Model did not converge 
surv_model3 <- update(surv_model2, . ~ . , 
                      control = glmerControl(optimizer = "nlminbw"))


##### residuals & fitted values 
surv_model3_residuals <- resid(surv_model3, type =  "pearson");
surv_model3_fitted <- fitted(surv_model3)

# Some deviations from normallity but otherwise OK 
par(mfrow=c(1,3))
newbinplot(surv_model3_residuals, surv_model3_fitted);
args(newbinplot)
summary(surv_model3)

# Residuals still have patterns, although the 



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

##### seedling survival analysis 
surv_preds_dia <- expand.grid(dia = seq(from = min(survival_data$dia, na.rm = TRUE), 
                                        to = max(survival_data$dia, na.rm = TRUE), 
                                        length = 100), 
                              treat = 9)

surv_preds_dia$p <- predict(survival_model1, 
                            newdata = surv_preds_dia, 
                            type = "response",
                            re.form = NA)

##### plotting the data 
ggplot(surv_preds_dia, aes(x = dia, y = p)) + geom_line()


################################################################################
################################################################################
################################################################################
##### species interaction - survival analysis... 
##### are the species different 
##### date: 30 01 2017

# first model 
surv_model2 <- glmer(surv ~ sp + treat + dia + sp:treat + 
                       (1 | mother) + (1| block), 
                     data = survival_data, 
                     family = "binomial")

# Model does not converge.
surv_model3 <- glmer(surv ~ sp + treat + dia + sp:treat + 
                       (1 | mother) + (1 | block), 
                     data = survival_data, 
                     family = "binomial", 
                     control=glmerControl(optimizer="nlminbw"))

# optimizer nlminbw allows convergence.
summary(surv_model3)

# residuals & fitted values.
surv_model1_residuals <- resid(surv_model3, type =  "pearson");
surv_model1_fitted <- fitted(surv_model3)

# Some deviations from normallity but otherwise OK (for ecology data).
# but there are strong cyclical patterns in the residuals... 
par(mfrow=c(1,3))
newbinplot(y = surv_model1_residuals, x = surv_model1_fitted);

# Random effects reasonably normally distributed
ranNorm("mother", slope = 1, model = surv_model3)
ranNorm("block", slope = 1, model = surv_model3)



################################################################################
#Graphing the data 
preds_sp_inter_surv <- expand.grid(sp = levels(survival_data$sp), 
                                   treat = seq(from = 0, 
                                               to = 21, 
                                               length = 100),
                                   dia = mean(survival_data$dia, na.rm = TRUE))

preds_sp_inter_surv$p <- predict(surv_model3, 
                                 newdata = preds_sp_inter_surv,
                                 type = "response",
                                 re.form = NA)

ggplot(preds_sp_inter_surv, aes(x = treat, y = p, color = sp)) + 
  geom_line()
  
coef <- fixef(surv_model3)

slope_coef <- data.frame(sp = levels(survival_data$sp), 
                         p = c(coef[11], coef[11] + coef[13:length(coef)]))

rownames(slope_coef) <- c()
#slope_coef <- slope_coef[order(-slope_coef$p), ]

# Testing the model with a type II anova 
car::Anova(surv_model3)
# Summarise the model 
summary(surv_model3)
# Calculate the differance in AIC
diff(AIC(surv_model3, survival_model1)[,2])
# write the coef results 
write.table(slope_coef, file = "./psSlopeCoef.txt")

