##### Paper: water inundation sensitivity in dipterocarps
##### author: james margrove 
##### date: 29.01.2017


##### remove clutter 
rm(list=ls())

##### imported packaged 
require(ggplot2)
require(lme4)
require(foreach)
library("lmerTest")
source("./functions/ranNorm.R")

################################################################################
data <- read.table("./data/Photosynthesis.txt",
                   header = TRUE);
str(data);

with(data[data$treat == 0,], tapply(E, sp, mean))

##### exploration 
ggplot(data, aes(x = treat, y = E)) + geom_point() + stat_smooth()

##### model addressing the hyporthesis, do dipterocarps generally show a decline
## in carbon aquisition with respect to water inundation. 
transp_model <- lmer(E ~ dia + treat + 
                      (1 | block) + 
                      (1 | sp / mother) + 
                      (1 | date), 
                    data = data)
##### model1 - judging from the exploration, log of the water inundation 
## frequency may be a better fit. 

transp_model2 <- lmer(E ~ dia + log(treat+1) + 
                       (1 | block) + 
                       (1 | sp / mother) + 
                       (1 | date), 
                     data = data)
summary(transp_model2)
aic_m1_m2 <- AIC(transp_model2, transp_model)
diff(aic_m1_m2[,2])
## show a differance in AIC of 6.45, so go with transp_model2

##### Evaluate the models. 

## fitted vs. residuals 
plot(transp_model2)
## no patterns in the residuals

ranNorm("mother:sp", slope = 1, model = transp_model2)
ranNorm("sp", slope = 1, model = transp_model2)
ranNorm("date", slope = 1, model = transp_model2)
ranNorm("block", slope = 1, model = transp_model2)

## generally the randome effects are looking OK


################################################################################
##### predicting the data values for the treatment  

transp_preds <- expand.grid(dia = mean(data$dia, na.rm = TRUE), 
                           treat = seq(from = 0, to = 21, length = 100))

transp_preds$E <-predict(transp_model2, 
                        transp_preds, 
                        type = "response", 
                        re.form = NA)


##### graphing the data with ggplot2 
ggplot(transp_preds, aes(x = treat, y = E)) + geom_line()
head(transp_preds)
tail(transp_preds)

##### predicting values for diameter 
transp_preds_dia <- expand.grid(dia = seq(from = min(data$dia, na.rm = TRUE), 
                                      to = max(data$dia, na.rm = TRUE), 
                                      length = 100),
                            treat = 9)

transp_preds_dia$E <- predict(transp_model2, 
                           newdata = transp_preds_dia, 
                           type = "response",
                           re.form = NA)
#### graphing the diameter
ggplot(transp_preds_dia, aes(x = dia, y = E)) + geom_line()
### those with higher diameters also have higher transperation

################################################################################
# Model the interaction between species and treatment. 
transp_model3 <- lmer(E ~ (log(treat+1) * sp + dia) + 
                       (1 | block) + 
                       (1 | mother) + 
                       (1 | date), 
                     data = data)
summary(transp_model3)
car::Anova(transp_model3)
diff(AIC(photo_model2, transp_model3)[,2])

# Some deviations from normallity but otherwise OK (for ecology data).
plot(transp_model3)

# Random effects reasonably normally distributed.
ranNorm("mother", slope = 1, model = transp_model3)
ranNorm("block", slope = 1, model = transp_model3)
ranNorm("date", slope = 1, model = transp_model3)
# summerise the model.
summary(transp_model3)
# Anova test the interaction.
car::Anova(transp_model3)

# extract coef from model.
coef <- fixef(transp_model3)

# Calculate fixed effect slopes for the species : treatment frequency.
slope_coef <- data.frame(sp = levels(data$sp), 
                         rgr = c(coef[11], coef[11] + coef[13:length(coef)]))
# Remove row-names.
rownames(slope_coef) <- c()
# write the coef results 
write.table(slope_coef, file = "./ESlopeCoef.txt")

# as S bec is the had the lowest difference, what is different to it 
data$sp <- relevel(data$sp, ref = "sbec")
# Same model, different primary level 
transp_model3 <- lmer(E ~ (log(treat + 1) * sp + dia) + 
                        (1 | block) + 
                        (1 | mother) + 
                        (1 | date), 
                      data = data)
summary(transp_model3)


# 
data$sp <- relevel(data$sp, ref = "dry")
transp_model4 <- lmer(E ~ (log(treat + 1) + sp + dia) + 
                        (1 | block) + 
                        (1 | mother) + 
                        (1 | date), 
                      data = data)
##
summary(transp_model4)
car::Anova(transp_model4)

